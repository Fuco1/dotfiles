---
name: commit-messages
description: Write a git commit message. Use before every `git commit`, and whenever asked to commit, amend, reword, squash, or draft a message. Covers the one-line conventional-commit subject, when a body is allowed at all, and what never goes in one.
---

# A commit message is one line

`<type>(<scope>): <subject>` — imperative, lowercase, no trailing period, under
~72 characters.

`feat` `fix` `docs` `style` `refactor` `perf` `test` `build` `ci` `chore` `revert`.

**The scope comes from the repo, not from you.** Its packages, modules or
top-level directories, and whatever `git log --oneline -30` already uses. If the
project's own `CLAUDE.md` fixes a scope list, that list wins. Omit the scope
rather than invent one.

## Refuse to write a long body

Most commits have no body. The diff is the description of the diff.

A body earns its place only when the *why* is not evident from the diff, and
then it is one or two short paragraphs. Link instead of restating — issue, task
or node id, doc path, URL.

Never in a body:

- a bullet-by-bullet retelling of the diff
- a summary of the files touched
- a narrative of the debugging that led here
- what the linked issue or doc already says
- a tombstone: what the code used to do, what was removed, why the old thing is
  gone. That is what the history is for, and this *is* the history.

## One commit is one kind of change

**Never mix a mechanical reformat with a content change.** `git add -A` is how
that rule gets broken without noticing — stage named paths.
