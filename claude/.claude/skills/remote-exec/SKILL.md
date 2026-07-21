---
name: remote-exec
description: Run shell commands on the project's remote nodes (gateway, workers, wrappers, etc.) over SSH. Use when verifying infrastructure state, inspecting puppet/nomad/vault on a node, checking devices/services, or following up on a terraform apply. Trigger on phrases like "check on the gateway", "ssh into worker-1", "run X on <node>", or any task where the answer lives on a managed host rather than the local repo.
---

# Running commands on remote nodes

All remote nodes in this project are reachable via SSH config aliases — the
node's short name **is** the SSH host. No `user@`, no `-i`, no jump host
needed from this workstation.

## Invocation

```bash
ssh <node-name> <command>
```

Examples:

```bash
ssh gateway lsblk
ssh gateway ls -l /dev/disk/azure/scsi1/
ssh worker-1 systemctl status nomad
```

## Sudo

`sudo` is passwordless on every managed node. Prefix the remote command
with `sudo` — do **not** try `sudo ssh` (that runs sudo locally).

```bash
ssh gateway sudo cat /etc/puppetlabs/puppet/puppet.conf
ssh gateway sudo systemctl restart puppetserver
```

If you need a pipeline or redirection that involves sudo, wrap the whole
remote side in a quoted shell invocation so the redirect runs on the
remote host:

```bash
ssh gateway "sudo blkid /dev/sdc1 || true"
ssh gateway "sudo bash -c 'echo line >> /etc/some-file'"
```

## Permission policy

- **Non-destructive commands run without asking.** Reads, inspections,
  status checks, `cat`/`ls`/`lsblk`/`mount`/`systemctl status`/`journalctl`/
  `puppet agent --noop`, etc. — just run them.
- **Destructive or state-changing commands require user confirmation
  first.** Anything that writes to disk, restarts a service, kills a
  process, mounts/unmounts filesystems, formats devices, edits files,
  or pushes config. State the exact command you intend to run on which
  host and wait for a yes before invoking. Examples that need
  confirmation:
  - `mkfs.*`, `parted`, `wipefs`, `dd`
  - `systemctl restart|stop|disable`, `kill`, `pkill`
  - `mount`, `umount`, edits to `/etc/fstab`
  - `puppet agent -t` (applies changes), `r10k deploy`
  - Anything writing under `/etc`, `/var/lib`, or service data dirs

When in doubt, treat it as destructive and ask first. A wrong read is
free; a wrong write on shared infra is not.

## Tips

- Quote the whole remote command if it contains shell metacharacters
  (`|`, `&&`, `>`, globs) so they're interpreted on the remote host:
  `ssh gateway "ls /etc/puppetlabs/code/environments/*/manifests"`.
- For multi-line scripts, prefer a heredoc piped over ssh:
  ```bash
  ssh gateway sudo bash <<'EOF'
  set -euo pipefail
  ...
  EOF
  ```
- `ssh <node> hostname` is a cheap connectivity check before a longer
  command.
