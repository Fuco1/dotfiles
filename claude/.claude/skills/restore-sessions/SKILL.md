---
name: restore-sessions
description: After a crash or reboot, resume the Claude Code sessions of the current directory that were still unfinished when the machine went down, one tmux window each, and report the ones left closed with a recap and a confidence that they are finished. Use when the operator says the computer crashed or rebooted, that sessions were killed, or asks to restore, resume or bring back the previous sessions.
---

# Restoring sessions after a crash

## 1. Candidates

The sessions to judge are the ones that were alive during the previous boot.

```
journalctl --list-boots | tail -2      # boot -1: start and end of the last run
ccarchive ls                           # this directory's sessions: uuid, state, cwd, title
```

`uptime -s` is wrong for this: it subtracts suspended time and reports a later boot.

A candidate is a session listed by `ccarchive ls` whose transcript,
`~/.claude/projects/<cwd with / replaced by ->/<uuid>.jsonl`, was last modified
inside the boot -1 window. Remove from the list:

- this session
- any session live now: `~/.claude/sessions/<pid>.json` names each running one's
  `sessionId`
- a singleton role that is already running again, such as orc's foreman: a second
  copy beside the live one does the same work twice

A session resumed after an earlier crash and never touched again has an mtime equal
to that resume. It still counts if that mtime is inside the window.

Transcripts of orc's worker and reviewer lanes live under other project directories
(`...-trees-worker-N-...`) and are never candidates: the fleet restarts its own lanes.

## 2. Judging each one

Read the end of the transcript. Timestamps are UTC.

```
# the conversation as text: last user requests and the final reply
jq -r 'select((.type=="user" or .type=="assistant") and .isSidechain!=true)
  | (.message.content | if type=="string" then . else map(select(.type=="text").text) | join("\n") end) as $t
  | select($t != "") | "=== \(.type) \(.timestamp)\n\($t)"' <uuid>.jsonl | tail -c 8000

# the last records, tool calls included, to see whether it died mid-turn
jq -r 'select(.isSidechain!=true and (.type=="user" or .type=="assistant"))
  | "\(.timestamp) \(.type) \(.message.content | if type=="string" then .[0:200] else map(.type + ": " + ((.text // .input // .content // "") | tostring | .[0:150])) | join(" | ") end)"' <uuid>.jsonl | tail -15
```

Unfinished, so resume it:

- the last record is a tool call or a tool result with no reply after it: the crash
  cut a turn short
- the final reply ends with a `Next Actions` row that is `Blocking`, or with a
  question, and no user message answers it
- the open item is still open now. Check it rather than trusting the transcript:
  `orc show <id>` for a node the reply says awaits promotion, approval or filing, or
  the repo's `origin/master` for a change it says is still to be made

Finished, so leave it closed:

- the final reply says `Nothing outstanding`, or every item it left open has since
  been settled (the node is `DONE`, the branch merged)
- the session only ran a command such as `/login`

When the request was answered but the reply still proposes non-blocking follow-ups,
call it finished and say so in the recap with a lower confidence.

## 3. Resuming

One window per session, in the tmux session this one runs in, named with a short
slug of the session's subject. Avoid the names of orc's lane windows (`worker-N`,
`review-N`, `merge`, `orcd`, `foreman`, `dashboard`).

```
T=$(tmux display -p -t "$TMUX_PANE" '#S')
tmux new-window -d -t "$T:" -n <slug> -c <session cwd> "ccarchive resume <uuid>"
```

About 15 seconds later each uuid appears in one of `~/.claude/sessions/*.json`. A uuid missing
from the registry, or a window that closed, is a resume that failed: report it. The
window's `pane_current_command` stays `fish`, because `claude` runs as a child of
the shell tmux starts the command in.

## 4. Report

- the resumed sessions, each with its window name and the open item that kept it
  unfinished
- one table of the sessions left closed: name, uuid prefix, a one-line recap of what
  it did and how it ended, and the confidence in percent that it is finished
- a session left closed for a reason other than being finished (a superseded
  singleton) goes in the same table, with that reason in the recap

Archive nothing. Archiving the finished ones is the operator's call.
