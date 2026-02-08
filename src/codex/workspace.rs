use std::collections::HashMap;

use super::thread_item::ThreadItem;

pub type WorkspaceId = String;
pub type ThreadId = String;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum WorkspaceConnectionState {
    Disconnected,
    Connecting,
    Connected,
}

#[derive(Debug, Clone, PartialEq)]
pub struct WorkspaceThread {
    pub id: ThreadId,
    pub name: String,
    pub items: Vec<ThreadItem>,
    pub active_turn_id: Option<String>,
    pub is_processing: bool,
    pub has_unread: bool,
}

impl WorkspaceThread {
    pub fn new(id: impl Into<String>, name: impl Into<String>) -> Self {
        Self {
            id: id.into(),
            name: name.into(),
            items: Vec::new(),
            active_turn_id: None,
            is_processing: false,
            has_unread: false,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Workspace {
    pub id: WorkspaceId,
    pub name: String,
    pub path: String,
    pub connection_state: WorkspaceConnectionState,
    pub threads: HashMap<ThreadId, WorkspaceThread>,
}

impl Workspace {
    pub fn new(id: impl Into<String>, name: impl Into<String>, path: impl Into<String>) -> Self {
        Self {
            id: id.into(),
            name: name.into(),
            path: path.into(),
            connection_state: WorkspaceConnectionState::Disconnected,
            threads: HashMap::new(),
        }
    }

    pub fn set_connection_state(&mut self, connection_state: WorkspaceConnectionState) {
        self.connection_state = connection_state;
    }

    pub fn upsert_thread(
        &mut self,
        thread_id: impl Into<String>,
        thread_name: impl Into<String>,
    ) -> &mut WorkspaceThread {
        let thread_id = thread_id.into();
        let thread_name = thread_name.into();

        self.threads
            .entry(thread_id.clone())
            .and_modify(|thread| {
                thread.name = thread_name.clone();
            })
            .or_insert_with(|| WorkspaceThread::new(thread_id, thread_name))
    }

    pub fn get_thread(&self, thread_id: &str) -> Option<&WorkspaceThread> {
        self.threads.get(thread_id)
    }

    pub fn get_thread_mut(&mut self, thread_id: &str) -> Option<&mut WorkspaceThread> {
        self.threads.get_mut(thread_id)
    }

    pub fn remove_thread(&mut self, thread_id: &str) -> Option<WorkspaceThread> {
        self.threads.remove(thread_id)
    }

    pub fn push_item(&mut self, thread_id: &str, item: ThreadItem) -> Result<(), &'static str> {
        let thread = self
            .threads
            .get_mut(thread_id)
            .ok_or("thread does not exist")?;
        thread.items.push(item);
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::{Workspace, WorkspaceConnectionState};
    use crate::codex::thread_item::ThreadItem;

    #[test]
    fn workspace_starts_disconnected() {
        let mut workspace = Workspace::new("ws-1", "Repo", "/tmp/repo");
        assert_eq!(
            workspace.connection_state,
            WorkspaceConnectionState::Disconnected
        );
        assert_eq!(workspace.id, "ws-1");
        assert_eq!(workspace.name, "Repo");
        assert_eq!(workspace.path, "/tmp/repo");

        workspace.set_connection_state(WorkspaceConnectionState::Connecting);
        assert_eq!(
            workspace.connection_state,
            WorkspaceConnectionState::Connecting
        );
        workspace.set_connection_state(WorkspaceConnectionState::Connected);
        assert_eq!(
            workspace.connection_state,
            WorkspaceConnectionState::Connected
        );
    }

    #[test]
    fn upsert_thread_inserts_and_then_updates_name() {
        let mut workspace = Workspace::new("ws-1", "Repo", "/tmp/repo");

        workspace.upsert_thread("thr-1", "Initial");
        workspace.upsert_thread("thr-1", "Renamed");

        let thread = workspace.get_thread("thr-1").expect("thread should exist");
        assert_eq!(thread.name, "Renamed");
    }

    #[test]
    fn remove_thread_deletes_entry() {
        let mut workspace = Workspace::new("ws-1", "Repo", "/tmp/repo");
        workspace.upsert_thread("thr-1", "Agent");

        let removed = workspace.remove_thread("thr-1");
        assert!(removed.is_some());
        assert!(workspace.get_thread("thr-1").is_none());
    }

    #[test]
    fn push_item_appends_to_thread_items() {
        let mut workspace = Workspace::new("ws-1", "Repo", "/tmp/repo");
        workspace.upsert_thread("thr-1", "Agent");
        let item = ThreadItem::AgentMessage {
            id: "itm-1".into(),
            text: "hi".into(),
        };

        workspace
            .push_item("thr-1", item)
            .expect("thread should exist");

        let thread_mut = workspace
            .get_thread_mut("thr-1")
            .expect("thread should exist");
        thread_mut.has_unread = true;

        let thread = workspace.get_thread("thr-1").expect("thread should exist");
        assert_eq!(thread.items.len(), 1);
        assert!(thread.has_unread);
    }
}
