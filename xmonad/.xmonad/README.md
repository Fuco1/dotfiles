# My xmonad configuration.

This is slowly turning my xmonad setup into a homebrew desktop environment.  Most features are implemented directly by the xmonad instance while some of the more sophisticated stuff is in external libraries.

The `xmonad.hs` file contains only updates to the default config and tons of keybindings.  Most of the interesting stuff is implemented as separate modules under the `lib` directory (that's where xmonad looks by default)

* `Brightness.hs` has some code to set brightness levels.
* `Constants.hs` contains constants and settings, such as managehooks, printers, prompts and so on.
* `MPD.hs` is an interface to `mpd`.  It has some custom prompts and lifts some of `Network.MPD` actions into `X` monad.  Might interest you if you use `mpd`.
* `Mount.hs` is some magic for semi-automatic mounting of volumes.   Very experimenta.
* `Mpris.hs` is an interface to [mpris2](https://specifications.freedesktop.org/mpris-spec/latest/) protocol, powered by my other library: [mpris](https://github.com/Fuco1/mpris).  It has some interop with `MPD.hs` to allow for seamless switching.
* `PulseAudio.hs` controls pulseaudio volume levels.
* `StackSetExtra.hs` contains some extra operations on window sets or helpers to execute stuff on other screens in multiscreen setup.
* `Utils.hs` is just regular pure functions used to make some things a bit more convenient
* `Workspaces.hs` contains utils to work with workspaces.  Currently only some facilities to make binding of keys less annoying (you can bind an action to a set of workspaces at once, such as "view the workspace `n`").

## xmobar

Also included is `xmobar` config, see the file `xmobarrc`.

## todo

* Add some code to detect changes in attached displays and automatically reconfigure
