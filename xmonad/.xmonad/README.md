My xmonad configuration.

The `xmonad.hs` file contains only updates to the default config and tons of keybindings.  Most of the interesting stuff is implemented as separate modules under the `lib` directory (that's where xmonad looks by default)

* `Utils.hs` is just regular pure functions used to make some things a bit more convenient
* `Constants.hs` contains constants and settings, such as managehooks, printers, prompts and so on.
* `MPD.hs` is an interface to `mpd`.  It has some custom prompts and lifts some of `Network.MPD` actions into `X` monad.  Might interest you if you use `mpd`.
* `StackSetExtra.hs` contains some extra operations on window sets or helpers to execute stuff on other screens in multiscreen setup.
* `Workspaces.hs` contains utils to work with workspaces.  Currently only some facilities to make binding of keys less annoying (you can bind an action to a set of workspaces at once, such as "view the workspace `n`").

## xmobar

Also included is `xmobar` config, see the file `xmobarrc`.
