use std::collections::HashMap;
use std::fmt;
use std::fs::{self, OpenOptions};
use std::io::{BufRead, BufReader, BufWriter, Write};
use std::path::PathBuf;
use std::process::{Child, Command, Stdio};
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Arc, Mutex};
use std::time::{SystemTime, UNIX_EPOCH};

use async_channel::Sender;
use futures_channel::oneshot;
use serde::{Deserialize, Serialize, de};
use serde_json::Value;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub enum RequestMethod {
    #[serde(rename = "initialize")]
    Initialize,
    #[serde(rename = "account/read")]
    AccountRead,
    #[serde(rename = "account/login/start")]
    AccountLoginStart,
    #[serde(rename = "thread/list")]
    ThreadList,
    #[serde(rename = "thread/start")]
    ThreadStart,
    #[serde(rename = "thread/resume")]
    ThreadResume,
    #[serde(rename = "turn/start")]
    TurnStart,
}

impl RequestMethod {
    const fn as_str(self) -> &'static str {
        match self {
            Self::Initialize => "initialize",
            Self::AccountRead => "account/read",
            Self::AccountLoginStart => "account/login/start",
            Self::ThreadList => "thread/list",
            Self::ThreadStart => "thread/start",
            Self::ThreadResume => "thread/resume",
            Self::TurnStart => "turn/start",
        }
    }
}

#[derive(Debug, Clone, Serialize)]
pub struct Request {
    pub id: u64,
    pub method: RequestMethod,
    pub params: Value,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Response {
    pub id: u64,
    pub payload: ResponsePayload,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ResponsePayload {
    Success(Value),
    Error(RpcError),
}

#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
pub struct RpcError {
    pub code: i64,
    pub message: String,
    pub data: Option<Value>,
}

impl fmt::Display for RpcError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "RPC error {}: {}", self.code, self.message)
    }
}

impl<'de> Deserialize<'de> for Response {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: de::Deserializer<'de>,
    {
        #[derive(Deserialize)]
        struct RawResponse {
            id: u64,
            result: Option<Value>,
            error: Option<RpcError>,
        }

        let raw = RawResponse::deserialize(deserializer)?;

        let payload = match (raw.result, raw.error) {
            (Some(result), None) => ResponsePayload::Success(result),
            (None, Some(error)) => ResponsePayload::Error(error),
            (Some(_), Some(_)) => {
                return Err(de::Error::custom(
                    "response contains both `result` and `error`",
                ));
            }
            (None, None) => ResponsePayload::Success(Value::Null),
        };

        Ok(Response {
            id: raw.id,
            payload,
        })
    }
}

#[derive(Debug, Clone)]
pub struct ServerNotification {
    pub method: String,
    pub params: Value,
}

#[derive(Debug)]
enum IncomingMessage {
    Response(Response),
    Notification(ServerNotification),
    Other,
}

impl IncomingMessage {
    fn parse(line: &str) -> Result<Self, AppServerError> {
        let v: Value = serde_json::from_str(line)?;

        let has_id = v.get("id").is_some();
        let has_result_or_error = v.get("result").is_some() || v.get("error").is_some();

        if has_id && has_result_or_error {
            let response: Response = serde_json::from_value(v)?;
            return Ok(Self::Response(response));
        }

        let has_method = v.get("method").is_some();
        if has_method && !has_id {
            let method = v["method"].as_str().unwrap_or_default().to_owned();
            let params = v.get("params").cloned().unwrap_or(Value::Null);
            return Ok(Self::Notification(ServerNotification { method, params }));
        }

        Ok(Self::Other)
    }
}

#[derive(Debug)]
pub enum AppServerError {
    Io(std::io::Error),
    Json(serde_json::Error),
    Rpc(RpcError),
    Disconnected,
}

impl fmt::Display for AppServerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Io(e) => write!(f, "I/O error: {e}"),
            Self::Json(e) => write!(f, "JSON error: {e}"),
            Self::Rpc(e) => write!(f, "{e}"),
            Self::Disconnected => write!(f, "app-server disconnected"),
        }
    }
}

impl From<std::io::Error> for AppServerError {
    fn from(e: std::io::Error) -> Self {
        Self::Io(e)
    }
}

impl From<serde_json::Error> for AppServerError {
    fn from(e: serde_json::Error) -> Self {
        Self::Json(e)
    }
}

type PendingMap = HashMap<u64, oneshot::Sender<Result<Value, RpcError>>>;
type PendingTraceMap = HashMap<u64, PendingTraceEntry>;

const GLOBAL_THREAD_LOG_ID: &str = "_global";

#[derive(Debug, Clone)]
struct PendingTraceEntry {
    method: String,
    request_raw: String,
    thread_id: Option<String>,
    deferred_request_log: bool,
}

struct RawTraceLogger {
    directory: PathBuf,
    write_lock: Mutex<()>,
}

impl RawTraceLogger {
    fn new(directory: PathBuf) -> Self {
        Self {
            directory,
            write_lock: Mutex::new(()),
        }
    }

    fn log_request(
        &self,
        thread_id: &str,
        request_id: u64,
        method: &str,
        raw_json: &str,
    ) -> Result<(), std::io::Error> {
        self.append(
            thread_id,
            format_args!("REQUEST id={request_id} method={method}"),
            raw_json,
        )
    }

    fn log_response(
        &self,
        thread_id: &str,
        request_id: u64,
        method: &str,
        raw_json: &str,
    ) -> Result<(), std::io::Error> {
        self.append(
            thread_id,
            format_args!("RESPONSE id={request_id} method={method}"),
            raw_json,
        )
    }

    fn log_notify(&self, method: &str, raw_json: &str) -> Result<(), std::io::Error> {
        self.append(
            GLOBAL_THREAD_LOG_ID,
            format_args!("NOTIFY method={method}"),
            raw_json,
        )
    }

    fn log_notification(
        &self,
        thread_id: &str,
        method: &str,
        raw_json: &str,
    ) -> Result<(), std::io::Error> {
        self.append(
            thread_id,
            format_args!("NOTIFICATION method={method}"),
            raw_json,
        )
    }

    fn append(
        &self,
        thread_id: &str,
        heading: fmt::Arguments<'_>,
        raw_json: &str,
    ) -> Result<(), std::io::Error> {
        let _guard = self.write_lock.lock().expect("trace logger lock poisoned");
        fs::create_dir_all(&self.directory)?;

        let file_name = format!("{}.md", sanitize_thread_id(thread_id));
        let path = self.directory.join(file_name);

        let mut file = OpenOptions::new().create(true).append(true).open(path)?;
        writeln!(file, "### [{timestamp}] {heading}", timestamp = unix_now())?;
        writeln!(file, "```json")?;
        writeln!(file, "{raw_json}")?;
        writeln!(file, "```")?;
        writeln!(file)?;
        Ok(())
    }
}

fn unix_now() -> u64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|duration| duration.as_secs())
        .unwrap_or(0)
}

fn sanitize_thread_id(thread_id: &str) -> String {
    let mut sanitized = String::new();
    for ch in thread_id.chars() {
        if ch.is_ascii_alphanumeric() || matches!(ch, '-' | '_' | '.') {
            sanitized.push(ch);
        } else {
            sanitized.push('_');
        }
    }

    if sanitized.is_empty() {
        GLOBAL_THREAD_LOG_ID.to_string()
    } else {
        sanitized
    }
}

fn extract_thread_id_from_params(params: &Value) -> Option<String> {
    params
        .get("threadId")
        .and_then(Value::as_str)
        .or_else(|| params.get("thread_id").and_then(Value::as_str))
        .or_else(|| {
            params
                .get("thread")
                .and_then(|thread| thread.get("id"))
                .and_then(Value::as_str)
        })
        .map(str::to_owned)
}

fn extract_thread_id_from_payload(payload: &ResponsePayload) -> Option<String> {
    match payload {
        ResponsePayload::Success(value) => extract_thread_id_from_value(value),
        ResponsePayload::Error(error) => error.data.as_ref().and_then(extract_thread_id_from_value),
    }
}

fn extract_thread_id_from_value(value: &Value) -> Option<String> {
    let direct = value
        .get("threadId")
        .and_then(Value::as_str)
        .or_else(|| value.get("thread_id").and_then(Value::as_str))
        .or_else(|| {
            value
                .get("thread")
                .and_then(|thread| thread.get("id"))
                .and_then(Value::as_str)
        })
        .map(str::to_owned);

    if direct.is_some() {
        return direct;
    }

    match value {
        Value::Object(map) => map.values().find_map(extract_thread_id_from_value),
        Value::Array(items) => items.iter().find_map(extract_thread_id_from_value),
        _ => None,
    }
}

pub struct AppServer {
    outgoing_tx: Sender<String>,
    next_id: AtomicU64,
    pending: Arc<Mutex<PendingMap>>,
    pending_traces: Arc<Mutex<PendingTraceMap>>,
    trace_logger: Arc<RawTraceLogger>,
    _child: Mutex<Option<Child>>,
    event_rx: async_channel::Receiver<ServerNotification>,
}

impl AppServer {
    pub async fn connect() -> Result<Arc<Self>, AppServerError> {
        let server = Arc::new(Self::spawn()?);
        server.initialize().await?;
        Ok(server)
    }

    fn spawn() -> Result<Self, AppServerError> {
        let mut child = Command::new("codex")
            .arg("app-server")
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::inherit())
            .spawn()?;

        let stdin = child.stdin.take().expect("stdin was piped");
        let stdout = child.stdout.take().expect("stdout was piped");

        let (outgoing_tx, outgoing_rx) = async_channel::unbounded::<String>();
        let pending: Arc<Mutex<PendingMap>> = Arc::new(Mutex::new(HashMap::new()));
        let pending_traces: Arc<Mutex<PendingTraceMap>> = Arc::new(Mutex::new(HashMap::new()));
        let trace_logger = Arc::new(RawTraceLogger::new(
            std::env::current_dir()
                .unwrap_or_else(|_| PathBuf::from("."))
                .join("logs"),
        ));
        let (event_tx, event_rx) = async_channel::unbounded::<ServerNotification>();

        std::thread::spawn(move || {
            let mut writer = BufWriter::new(stdin);
            while let Ok(line) = outgoing_rx.recv_blocking() {
                if writer.write_all(line.as_bytes()).is_err()
                    || writer.write_all(b"\n").is_err()
                    || writer.flush().is_err()
                {
                    break;
                }
            }
        });

        let pending_for_reader = Arc::clone(&pending);
        let pending_traces_for_reader = Arc::clone(&pending_traces);
        let trace_logger_for_reader = Arc::clone(&trace_logger);
        let event_tx_for_reader = event_tx.clone();
        std::thread::spawn(move || {
            let reader = BufReader::new(stdout);
            for line in reader.lines() {
                let line = match line {
                    Ok(line) => line,
                    Err(_) => break,
                };
                if line.is_empty() {
                    continue;
                }

                match IncomingMessage::parse(&line) {
                    Ok(IncomingMessage::Response(response)) => {
                        let trace_entry = pending_traces_for_reader
                            .lock()
                            .expect("pending traces lock poisoned")
                            .remove(&response.id);

                        let payload_for_logging = response.payload.clone();
                        if let Some(trace_entry) = trace_entry {
                            let response_thread_id = trace_entry
                                .thread_id
                                .clone()
                                .or_else(|| extract_thread_id_from_payload(&payload_for_logging));
                            let thread_log_id = response_thread_id
                                .as_deref()
                                .unwrap_or(GLOBAL_THREAD_LOG_ID);

                            if trace_entry.deferred_request_log {
                                if let Err(error) = trace_logger_for_reader.log_request(
                                    thread_log_id,
                                    response.id,
                                    &trace_entry.method,
                                    &trace_entry.request_raw,
                                ) {
                                    eprintln!("app-server: failed to write request trace: {error}");
                                }
                            }

                            if let Err(error) = trace_logger_for_reader.log_response(
                                thread_log_id,
                                response.id,
                                &trace_entry.method,
                                &line,
                            ) {
                                eprintln!("app-server: failed to write response trace: {error}");
                            }
                        } else if let Err(error) = trace_logger_for_reader.log_response(
                            GLOBAL_THREAD_LOG_ID,
                            response.id,
                            "unknown",
                            &line,
                        ) {
                            eprintln!("app-server: failed to write response trace: {error}");
                        }

                        let tx = pending_for_reader
                            .lock()
                            .expect("pending lock poisoned")
                            .remove(&response.id);

                        if let Some(tx) = tx {
                            let result = match response.payload {
                                ResponsePayload::Success(value) => Ok(value),
                                ResponsePayload::Error(error) => Err(error),
                            };
                            let _ = tx.send(result);
                        }
                    }
                    Ok(IncomingMessage::Notification(notification)) => {
                        let thread_log_id = extract_thread_id_from_value(&notification.params)
                            .unwrap_or_else(|| GLOBAL_THREAD_LOG_ID.to_string());
                        if let Err(error) = trace_logger_for_reader.log_notification(
                            &thread_log_id,
                            &notification.method,
                            &line,
                        ) {
                            eprintln!("app-server: failed to write notification trace: {error}");
                        }
                        let _ = event_tx_for_reader.send_blocking(notification);
                    }
                    Ok(IncomingMessage::Other) => {}
                    Err(error) => {
                        eprintln!("app-server: failed to parse message: {error}");
                    }
                }
            }

            let mut map = pending_for_reader.lock().expect("pending lock poisoned");
            for (_, tx) in map.drain() {
                let _ = tx.send(Err(RpcError {
                    code: -1,
                    message: "app-server disconnected".into(),
                    data: None,
                }));
            }

            let mut traces = pending_traces_for_reader
                .lock()
                .expect("pending traces lock poisoned");
            for (id, trace_entry) in traces.drain() {
                if trace_entry.deferred_request_log {
                    let _ = trace_logger_for_reader.log_request(
                        GLOBAL_THREAD_LOG_ID,
                        id,
                        &trace_entry.method,
                        &trace_entry.request_raw,
                    );
                }
            }
        });

        Ok(Self {
            outgoing_tx,
            next_id: AtomicU64::new(0),
            pending,
            pending_traces,
            trace_logger,
            _child: Mutex::new(Some(child)),
            event_rx,
        })
    }

    async fn initialize(&self) -> Result<(), AppServerError> {
        self.call(
            RequestMethod::Initialize,
            serde_json::json!({
                "clientInfo": {
                    "name": "agent_hub",
                    "title": "Agent Hub",
                    "version": "0.1.0"
                }
            }),
        )
        .await?;

        self.notify("initialized", serde_json::json!({}))
    }

    pub async fn call(
        &self,
        method: RequestMethod,
        params: Value,
    ) -> Result<Value, AppServerError> {
        let id = self.next_id.fetch_add(1, Ordering::Relaxed);
        let (tx, rx) = oneshot::channel();

        self.pending
            .lock()
            .expect("pending lock poisoned")
            .insert(id, tx);

        let request = Request { id, method, params };
        let line = serde_json::to_string(&request)?;
        let thread_id = extract_thread_id_from_params(&request.params);
        let deferred_request_log =
            matches!(request.method, RequestMethod::ThreadStart) && thread_id.is_none();

        self.pending_traces
            .lock()
            .expect("pending traces lock poisoned")
            .insert(
                id,
                PendingTraceEntry {
                    method: request.method.as_str().to_string(),
                    request_raw: line.clone(),
                    thread_id: thread_id.clone(),
                    deferred_request_log,
                },
            );

        if !deferred_request_log {
            let thread_log_id = thread_id.as_deref().unwrap_or(GLOBAL_THREAD_LOG_ID);
            if let Err(error) =
                self.trace_logger
                    .log_request(thread_log_id, id, request.method.as_str(), &line)
            {
                eprintln!("app-server: failed to write request trace: {error}");
            }
        }

        if self.outgoing_tx.send(line).await.is_err() {
            self.pending
                .lock()
                .expect("pending lock poisoned")
                .remove(&id);
            if let Some(trace_entry) = self
                .pending_traces
                .lock()
                .expect("pending traces lock poisoned")
                .remove(&id)
            {
                if trace_entry.deferred_request_log {
                    let _ = self.trace_logger.log_request(
                        GLOBAL_THREAD_LOG_ID,
                        id,
                        &trace_entry.method,
                        &trace_entry.request_raw,
                    );
                }
            }
            return Err(AppServerError::Disconnected);
        }

        let result = rx.await.map_err(|_| AppServerError::Disconnected)?;
        result.map_err(AppServerError::Rpc)
    }

    pub fn subscribe(&self) -> async_channel::Receiver<ServerNotification> {
        self.event_rx.clone()
    }

    pub fn notify(&self, method: &str, params: Value) -> Result<(), AppServerError> {
        let msg = serde_json::json!({
            "method": method,
            "params": params,
        });
        let line = serde_json::to_string(&msg)?;
        if let Err(error) = self.trace_logger.log_notify(method, &line) {
            eprintln!("app-server: failed to write notify trace: {error}");
        }
        self.outgoing_tx
            .try_send(line)
            .map_err(|_| AppServerError::Disconnected)
    }
}

#[cfg(test)]
mod tests {
    use std::env;
    use std::fs;

    use super::{
        RawTraceLogger, ResponsePayload, RpcError, extract_thread_id_from_params,
        extract_thread_id_from_payload, extract_thread_id_from_value, sanitize_thread_id,
    };

    #[test]
    fn extracts_thread_id_from_supported_param_shapes() {
        let camel = serde_json::json!({ "threadId": "thr-camel" });
        let snake = serde_json::json!({ "thread_id": "thr-snake" });
        let nested = serde_json::json!({ "thread": { "id": "thr-nested" } });

        assert_eq!(
            extract_thread_id_from_params(&camel).as_deref(),
            Some("thr-camel")
        );
        assert_eq!(
            extract_thread_id_from_params(&snake).as_deref(),
            Some("thr-snake")
        );
        assert_eq!(
            extract_thread_id_from_params(&nested).as_deref(),
            Some("thr-nested")
        );
    }

    #[test]
    fn extracts_thread_id_from_response_payload() {
        let payload = ResponsePayload::Success(serde_json::json!({
            "thread": { "id": "thr-123" }
        }));
        assert_eq!(
            extract_thread_id_from_payload(&payload).as_deref(),
            Some("thr-123")
        );

        let error_payload = ResponsePayload::Error(RpcError {
            code: -1,
            message: "oops".to_string(),
            data: Some(serde_json::json!({
                "threadId": "thr-error"
            })),
        });
        assert_eq!(
            extract_thread_id_from_payload(&error_payload).as_deref(),
            Some("thr-error")
        );
    }

    #[test]
    fn extracts_thread_id_from_nested_value() {
        let nested = serde_json::json!({
            "event": {
                "payload": {
                    "context": {
                        "thread_id": "thr-nested-event"
                    }
                }
            }
        });

        assert_eq!(
            extract_thread_id_from_value(&nested).as_deref(),
            Some("thr-nested-event")
        );
    }

    #[test]
    fn sanitizes_thread_ids_for_file_names() {
        assert_eq!(sanitize_thread_id("thr-1"), "thr-1");
        assert_eq!(sanitize_thread_id("../thr:2"), ".._thr_2");
        assert_eq!(sanitize_thread_id(""), "_global");
    }

    #[test]
    fn writes_markdown_traces() {
        let dir = env::temp_dir().join(format!("agent-hub-trace-test-{}", std::process::id()));
        let _ = fs::remove_dir_all(&dir);
        let logger = RawTraceLogger::new(dir.clone());

        logger
            .log_request("thr-log", 1, "turn/start", r#"{"id":1}"#)
            .expect("request trace should write");
        logger
            .log_response("thr-log", 1, "turn/start", r#"{"id":1,"result":{}}"#)
            .expect("response trace should write");
        logger
            .log_notification(
                "thr-log",
                "item/agentMessage/delta",
                r#"{"method":"item/agentMessage/delta"}"#,
            )
            .expect("notification trace should write");

        let log_path = dir.join("thr-log.md");
        let content = fs::read_to_string(log_path).expect("trace file should exist");
        assert!(content.contains("REQUEST id=1 method=turn/start"));
        assert!(content.contains("RESPONSE id=1 method=turn/start"));
        assert!(content.contains("NOTIFICATION method=item/agentMessage/delta"));

        let _ = fs::remove_dir_all(&dir);
    }
}
