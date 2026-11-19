---
name: vbox-screens
description: Fix the screens of this Linux VirtualBox guest when a screen has the wrong size or shows black. It happens after the Windows host resumes from hibernate or the VM resumes from a saved state. Use when the operator says a screen is black, the resolution is wrong, fullscreen does not resize, or resizing got stuck.
---

# Fixing the VirtualBox guest screens

The guest runs xmonad on X11 with the `modesetting` driver, a VMSVGA adapter
(`vmwgfx`) and two outputs, `Virtual-1` (primary, left) and `Virtual-2` (right).
Each host monitor is 2560x1440.

`VBoxDRMClient` passes each size the host window requests to the kernel. X then
lists that size as the output's preferred mode, marked `+` in `xrandr`. The xmonad
session doesn't run a program that switches an output to its preferred mode. The
screens keep their old size until `xrandr` sets it.

## 1. Read the state

```sh
xrandr --listmonitors
xrandr | grep -A1 -E '^Virtual-[12] '
```

The first command prints the size each output has. The second prints the size the
host requests for each output, on the line ending in `+`.

When the operator reports a black screen, capture the guest framebuffer and look
at it:

```sh
import -window root <scratchpad>/root.png
```

Content on both halves means the guest draws both screens and the black is in the
host's VirtualBox window. The fix in step 2 repairs that case too.

## 2. Set both outputs

Switch both outputs off, then set both modes and the framebuffer size explicitly
in one call. With the VM fullscreen on both host monitors:

```sh
xrandr --output Virtual-1 --off --output Virtual-2 --off
xrandr --fb 5120x1440 --output Virtual-1 --mode 2560x1440 --pos 0x0 --primary --output Virtual-2 --mode 2560x1440 --pos 2560x0
```

For windowed sizes, use each output's preferred mode. `--fb` is the sum of the
two widths by the larger height, and `Virtual-2` sits at `--pos <width of
Virtual-1>x0`. A preferred size that matches neither a host monitor nor a host
window appears while a host window is black. Use 2560x1440 for that output.

Confirm with `xrandr --listmonitors`, then ask the operator whether both screens
show content at full size with no black bars.

## Steps that leave the problem in place

- `xrandr --output Virtual-1 --auto --output Virtual-2 --auto --right-of Virtual-1`
  can fail with `BadMatch` and leave both outputs off. When it succeeds, a screen
  that was black in the host window is still black.
- Switching only `Virtual-2` off and on.
- Restarting the `VBoxClient` user services or `vboxadd-service`.
- View → Virtual Screen 2 → Disable in the host's VirtualBox window.

## Unknown

Until 2026-11-16 the screens resized on their own with the same Guest Additions
(7.2.18), kernel, X server and xmonad config. Automatic resizing stopped after the
guest resumed from a 68-hour freeze on 2026-11-16. Guest reboots did not restore
it. A cold start (shut down the guest, start the VM without a saved state)
is untested as a way to bring it back.
