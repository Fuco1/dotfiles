---
name: create-task
description: Plan and track a multi-step engineering task in the current project. Use when the user asks to "plan", "create a task", "break this down", or when a change is non-trivial enough to want a written plan with reviewable, individually-applicable steps. Produces `.ai/tasks/<task-name>/PLAN.md` (under the project root) and updates it as steps are executed; capture files are created on demand to record real output from steps already run, never as runbooks written ahead of time.
---

# create-task

A task is a written, multi-step plan for a non-trivial change to the
current project. It lives under `.ai/tasks/<task-name>/` (relative to
the project root) so that:

- The plan survives across Claude sessions.
- Each step can be reviewed, applied, and committed independently.

The plan itself lives in `PLAN.md` and is self-contained — every step
includes the concrete commands, edits, and verification needed to
execute it. Optional capture files (see below) exist only to record
real output produced *during* execution that a later step or a
restarted session will need to read back.

## Directory layout

```
.ai/tasks/<task-name>/
├── PLAN.md                 # the plan itself — single source of truth
└── (optional capture files, created on demand once a step has run)
```

- `<task-name>` is short kebab-case and describes the change, not the
  symptom. Good: `gateway-worker-vm-refactor`, `vault-pki-rotation`.
  Bad: `fix-bug`, `task1`.
- Capture file names, when needed, start with the step number that
  produced them, then a hyphen, then a short kebab-case slug:
  `3-state-list.md`, `5-plan-output.md`.

## When to create a task

Create a task when **any** of these is true:

- The change spans multiple commits or PRs.
- It needs migrations, state surgery, or any other ordered sequence of
  operations the user will want to review before running.
- It touches shared / hard-to-reverse state (infrastructure, databases,
  auth, public APIs) where a wrong order of operations is costly.
- The user explicitly asks for a plan.

Do **not** create a task for one-off edits, single-file fixes, or
exploratory questions — those belong in the conversation.

## PLAN.md structure

PLAN.md is the single source of truth. Each step must be self-contained:
the commands, edits, and verification all live inside the step itself,
not in a separate file. Skeleton:

```markdown
# <Task title — full sentence is fine>

## Goal

One short paragraph: what we are changing and the success criterion
(e.g. "no resource recreation", "no behavioral drift", "passes
`terraform plan` with zero diff on unrelated callers").

## Context / mismatch summary    (optional)

A table or bulleted list of source-vs-target gaps, current-vs-desired
state, or anything else the reader needs before the steps make sense.
Skip this section if the goal already makes the situation obvious.

## Step-by-step plan

Each step is intended to land as its own commit / atomic operation
with verification in between.

### Step 1 — <short imperative title>

- Bulleted list of concrete edits, commands, or decisions — written
  inline. Fenced code blocks for the actual commands / diffs are fine
  and expected; do not push them out into a separate file just because
  they exist.
- **Verification**: how we confirm this step is safe before moving on
  (a dry-run, a plan output, a test, a query result, etc.).
- If a capture file was produced while executing this step (see below),
  reference it inline: "output in [1-state-list.md](1-state-list.md)".

### Step 2 — <short imperative title>

...

## Risks & open questions    (optional)

Anything the reader should know that doesn't belong inside a step:
unresolved decisions, known sharp edges, out-of-scope items.
```

Tone: terse, decision-oriented, written so a future maintainer (or a
future Claude session) can pick it up cold. Prefer concrete commands
and addresses over prose.

## Marking progress

When a step has been **fully implemented and verified** (code merged,
state moves run, plan clean), append `[executed]` to its header:

```markdown
### Step 1 — <short imperative title> [executed]
```

Conventions:

- Only mark `[executed]` after the user confirms the step landed (or
  after you applied it on the user's behalf and verified). Drafting
  code is not "executed".
- Do not delete or rewrite an executed step — it is now a historical
  record. If the approach has to change, add a follow-up step instead.
- **Preserve the original assignment; append results below it.** The
  step body describes what was *asked* of this step — it must remain
  intact after execution so a future reader can still see what the
  step was. Record what happened by *appending* to the step (under a
  short `**Result:**` / `**Outcome:**` / `**Decision:**` line, or by
  linking to a capture file). Never overwrite the instructions with
  the result.
  - For decision steps ("decide whether to use X or Y"), keep the
    question and the options; append the chosen answer and a one-line
    rationale.
  - For investigation steps ("find out how Z is wired up"), keep the
    question; append the finding inline if short, or link to a capture
    file if long.
  - For action steps ("run terraform apply for module foo"), keep the
    command; append the observed outcome / any drift / a link to the
    captured output.
- If a step is partially done, leave the header alone and add a short
  note in the body. Reserve `[executed]` for fully complete steps.
- Other markers are allowed when useful and should be self-explanatory:
  `[skipped]`, `[superseded by step N]`. Default to `[executed]`.

## Capture files

Capture files exist for **one reason only**: to record real output
produced while a step was executed, so a future Claude session (or
the user, after a context reset) can pick up the work without having
to re-run the command.

A capture file is appropriate when **all** of the following hold:

- The step has actually been executed (or partially executed) — the
  content is observed reality, not a plan.
- A later step, a verification, or a possible session restart will
  need to read it back. If nothing later depends on it, don't
  bother — the conversation/git history is enough.
- The content is too long or too noisy to drop inline into PLAN.md
  without hurting readability (e.g. a 200-line `terraform plan`, a
  full `puppet agent -t` trace, a `azurerm_*` state listing, a query
  result set).

Typical good captures: a `terraform plan` excerpt that step N+1's
verification will be compared against; a snapshot of database rows
that later steps will diff against; a long error message that
informed a follow-up step's design.

Do **not** create capture files for:

- Runbooks or playbooks written ahead of time. The commands a step
  will run belong inside the step itself in PLAN.md, in a fenced
  code block.
- Diffs of code you are *about to* write — write them in PLAN.md
  (or just commit the code and reference the commit).
- Design rationale or decisions — those go in the relevant step's
  body, or in "Risks & open questions" if cross-cutting.
- A predetermined "one capture file per step" pattern. Most steps
  produce nothing worth capturing.

Format, when you do write one:

- Name: `<step-number>-<slug>.md`, where the step number is the step
  that produced the output (e.g. `2-data-disk-state.md`).
- Lead with a one-line "what / when / how produced" header so a
  future reader knows it is recorded output, not a plan.
- Then the raw content in a fenced block. Keep it copy-pasteable.
- Reference from PLAN.md with a relative link from the step that
  produced it, e.g. `output in [2-data-disk-state.md](2-data-disk-state.md)`.

If a capture file is referenced from PLAN.md but doesn't exist yet,
that is a sign you are pre-writing a runbook — stop and inline the
content into the step instead.

## Updating an existing task

When the user asks to continue, extend, or revise a task:

1. Read PLAN.md first. Treat executed steps as immutable.
2. If new information changes the approach, append a new step rather
   than editing an executed one.
3. If a not-yet-executed step needs to change, edit it in place and
   note the reason briefly in the body if it isn't self-evident.
4. Add or update capture files as needed; reference them from the
   step that depends on them.

## Bootstrap

If `.ai/tasks/` doesn't exist in the project yet, just create it
alongside the new task directory — no separate setup step needed.
