use std::collections::HashMap;
use std::fmt;
use std::io::{BufRead, BufReader, BufWriter, Write};
use std::path::PathBuf;
use std::process::{Child, Command, Stdio};
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Arc, Mutex};

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
    #[serde(rename = "account/logout")]
    AccountLogout,
    #[serde(rename = "config/read")]
    ConfigRead,
    #[serde(rename = "account/login/start")]
    AccountLoginStart,
    #[serde(rename = "model/list")]
    ModelList,
    #[serde(rename = "thread/list")]
    ThreadList,
    #[serde(rename = "thread/start")]
    ThreadStart,
    #[serde(rename = "thread/resume")]
    ThreadResume,
    #[serde(rename = "skills/list")]
    SkillsList,
    #[serde(rename = "turn/start")]
    TurnStart,
    #[serde(rename = "turn/interrupt")]
    TurnInterrupt,
    #[serde(rename = "turn/steer")]
    TurnSteer,
    #[serde(rename = "account/rateLimits/read")]
    AccountRateLimitsRead,
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
    pub request_id: Option<Value>,
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
        if has_method {
            let request_id = if has_id && !has_result_or_error {
                v.get("id").cloned()
            } else {
                None
            };
            let method = v["method"].as_str().unwrap_or_default().to_owned();
            let params = v.get("params").cloned().unwrap_or(Value::Null);
            return Ok(Self::Notification(ServerNotification {
                request_id,
                method,
                params,
            }));
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

pub struct AppServer {
    outgoing_tx: Sender<String>,
    next_id: AtomicU64,
    pending: Arc<Mutex<PendingMap>>,
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
        let codex_path = find_codex_executable();
        let mut child = Command::new(codex_path)
            .arg("app-server")
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::inherit())
            .spawn()?;

        let stdin = child.stdin.take().expect("stdin was piped");
        let stdout = child.stdout.take().expect("stdout was piped");

        let (outgoing_tx, outgoing_rx) = async_channel::unbounded::<String>();
        let pending: Arc<Mutex<PendingMap>> = Arc::new(Mutex::new(HashMap::new()));
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
        });

        Ok(Self {
            outgoing_tx,
            next_id: AtomicU64::new(0),
            pending,
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

        if self.outgoing_tx.send(line).await.is_err() {
            self.pending
                .lock()
                .expect("pending lock poisoned")
                .remove(&id);
            return Err(AppServerError::Disconnected);
        }

        let result = rx.await.map_err(|_| AppServerError::Disconnected)?;
        result.map_err(AppServerError::Rpc)
    }

    pub fn subscribe(&self) -> async_channel::Receiver<ServerNotification> {
        self.event_rx.clone()
    }

    pub async fn respond(&self, request_id: Value, result: Value) -> Result<(), AppServerError> {
        let msg = serde_json::json!({
            "id": request_id.clone(),
            "result": result,
        });
        let line = serde_json::to_string(&msg)?;
        self.outgoing_tx
            .send(line)
            .await
            .map_err(|_| AppServerError::Disconnected)
    }

    pub async fn respond_error(
        &self,
        request_id: Value,
        code: i64,
        message: impl Into<String>,
        data: Option<Value>,
    ) -> Result<(), AppServerError> {
        let msg = serde_json::json!({
            "id": request_id,
            "error": {
                "code": code,
                "message": message.into(),
                "data": data,
            },
        });
        let line = serde_json::to_string(&msg)?;
        self.outgoing_tx
            .send(line)
            .await
            .map_err(|_| AppServerError::Disconnected)
    }

    pub fn notify(&self, method: &str, params: Value) -> Result<(), AppServerError> {
        let msg = serde_json::json!({
            "method": method,
            "params": params,
        });
        let line = serde_json::to_string(&msg)?;
        self.outgoing_tx
            .try_send(line)
            .map_err(|_| AppServerError::Disconnected)
    }
}

fn find_codex_executable() -> PathBuf {
    let mut paths = vec![
        PathBuf::from("/opt/homebrew/bin/codex"),
        PathBuf::from("/usr/local/bin/codex"),
    ];

    if let Ok(home) = std::env::var("HOME") {
        paths.push(PathBuf::from(home).join(".cargo/bin/codex"));
    }

    for path in paths {
        if path.exists() {
            return path;
        }
    }

    PathBuf::from("codex")
}

#[cfg(test)]
mod tests {
    use super::IncomingMessage;

    #[test]
    fn parses_server_request_as_notification_with_request_id() {
        let line = serde_json::json!({
            "id": "0",
            "method": "item/commandExecution/requestApproval",
            "params": {
                "threadId": "thr-1",
                "turnId": "1",
                "itemId": "item-1"
            }
        })
        .to_string();

        let message = IncomingMessage::parse(&line).expect("server request should parse");
        match message {
            IncomingMessage::Notification(notification) => {
                assert_eq!(notification.method, "item/commandExecution/requestApproval");
                assert_eq!(notification.request_id, Some(serde_json::json!("0")));
                assert_eq!(
                    notification
                        .params
                        .get("threadId")
                        .and_then(serde_json::Value::as_str),
                    Some("thr-1")
                );
            }
            other => panic!("expected notification, got {other:?}"),
        }
    }

    #[test]
    fn parses_regular_notification_without_request_id() {
        let line = serde_json::json!({
            "method": "turn/started",
            "params": {
                "threadId": "thr-1"
            }
        })
        .to_string();

        let message = IncomingMessage::parse(&line).expect("notification should parse");
        match message {
            IncomingMessage::Notification(notification) => {
                assert_eq!(notification.method, "turn/started");
                assert_eq!(notification.request_id, None);
            }
            other => panic!("expected notification, got {other:?}"),
        }
    }
}
