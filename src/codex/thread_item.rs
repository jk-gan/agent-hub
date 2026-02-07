use std::collections::HashMap;

use serde::{Deserialize, Serialize};
use serde_json::Value;

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum CommandExecutionStatus {
    #[serde(rename = "inProgress")]
    InProgress,
    #[serde(rename = "completed")]
    Completed,
    #[serde(rename = "failed")]
    Failed,
    #[serde(rename = "declined")]
    Declined,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum PatchApplyStatus {
    #[serde(rename = "inProgress")]
    InProgress,
    #[serde(rename = "completed")]
    Completed,
    #[serde(rename = "failed")]
    Failed,
    #[serde(rename = "declined")]
    Declined,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum McpToolCallStatus {
    #[serde(rename = "inProgress")]
    InProgress,
    #[serde(rename = "completed")]
    Completed,
    #[serde(rename = "failed")]
    Failed,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum CollabAgentToolCallStatus {
    #[serde(rename = "inProgress")]
    InProgress,
    #[serde(rename = "completed")]
    Completed,
    #[serde(rename = "failed")]
    Failed,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum CollabAgentStatus {
    #[serde(rename = "pendingInit")]
    PendingInit,
    #[serde(rename = "running")]
    Running,
    #[serde(rename = "completed")]
    Completed,
    #[serde(rename = "errored")]
    Errored,
    #[serde(rename = "shutdown")]
    Shutdown,
    #[serde(rename = "notFound")]
    NotFound,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum CollabAgentTool {
    #[serde(rename = "spawnAgent")]
    SpawnAgent,
    #[serde(rename = "sendInput")]
    SendInput,
    #[serde(rename = "wait")]
    Wait,
    #[serde(rename = "closeAgent")]
    CloseAgent,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct CollabAgentState {
    pub status: CollabAgentStatus,
    pub message: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "type")]
pub enum PatchChangeKind {
    #[serde(rename = "add")]
    Add,
    #[serde(rename = "delete")]
    Delete,
    #[serde(rename = "update")]
    Update {
        #[serde(rename = "move_path")]
        move_path: Option<String>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct FileUpdateChange {
    pub path: String,
    pub kind: PatchChangeKind,
    pub diff: String,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(tag = "type")]
pub enum ThreadItem {
    #[serde(rename = "userMessage")]
    UserMessage { id: String, content: Vec<Value> },
    #[serde(rename = "agentMessage")]
    AgentMessage { id: String, text: String },
    #[serde(rename = "plan")]
    Plan { id: String, text: String },
    #[serde(rename = "reasoning")]
    Reasoning {
        id: String,
        #[serde(default)]
        summary: Vec<String>,
        #[serde(default)]
        content: Vec<String>,
    },
    #[serde(rename = "commandExecution")]
    CommandExecution {
        id: String,
        command: String,
        #[serde(rename = "commandActions")]
        command_actions: Vec<Value>,
        cwd: String,
        status: CommandExecutionStatus,
        #[serde(rename = "aggregatedOutput")]
        aggregated_output: Option<String>,
        #[serde(rename = "durationMs")]
        duration_ms: Option<i64>,
        #[serde(rename = "exitCode")]
        exit_code: Option<i32>,
        #[serde(rename = "processId")]
        process_id: Option<String>,
    },
    #[serde(rename = "fileChange")]
    FileChange {
        id: String,
        changes: Vec<FileUpdateChange>,
        status: PatchApplyStatus,
    },
    #[serde(rename = "mcpToolCall")]
    McpToolCall {
        id: String,
        server: String,
        tool: String,
        arguments: Value,
        status: McpToolCallStatus,
        #[serde(rename = "durationMs")]
        duration_ms: Option<i64>,
        error: Option<Value>,
        result: Option<Value>,
    },
    #[serde(rename = "collabAgentToolCall")]
    CollabAgentToolCall {
        id: String,
        tool: CollabAgentTool,
        status: CollabAgentToolCallStatus,
        #[serde(rename = "senderThreadId")]
        sender_thread_id: String,
        #[serde(rename = "receiverThreadIds")]
        receiver_thread_ids: Vec<String>,
        #[serde(rename = "agentsStates")]
        agents_states: HashMap<String, CollabAgentState>,
        prompt: Option<String>,
    },
    #[serde(rename = "webSearch")]
    WebSearch {
        id: String,
        query: String,
        action: Option<Value>,
    },
    #[serde(rename = "imageView")]
    ImageView { id: String, path: String },
    #[serde(rename = "enteredReviewMode")]
    EnteredReviewMode { id: String, review: String },
    #[serde(rename = "exitedReviewMode")]
    ExitedReviewMode { id: String, review: String },
    #[serde(rename = "contextCompaction")]
    ContextCompaction { id: String },
}

#[cfg(test)]
mod tests {
    use super::{CommandExecutionStatus, ThreadItem};

    #[test]
    fn parses_user_message_item() {
        let raw = serde_json::json!({
            "type": "userMessage",
            "id": "itm-1",
            "content": [
                { "type": "text", "text": "hello" }
            ]
        });

        let parsed: ThreadItem = serde_json::from_value(raw).expect("user message should parse");
        assert!(matches!(parsed, ThreadItem::UserMessage { .. }));
    }

    #[test]
    fn parses_command_execution_item() {
        let raw = serde_json::json!({
            "type": "commandExecution",
            "id": "itm-2",
            "command": "cargo test",
            "commandActions": [],
            "cwd": "/tmp/repo",
            "status": "completed",
            "aggregatedOutput": "ok",
            "durationMs": 12,
            "exitCode": 0,
            "processId": "pty-1"
        });

        let parsed: ThreadItem =
            serde_json::from_value(raw).expect("command execution should parse");

        match parsed {
            ThreadItem::CommandExecution { status, .. } => {
                assert_eq!(status, CommandExecutionStatus::Completed);
            }
            _ => panic!("expected command execution item"),
        }
    }
}
