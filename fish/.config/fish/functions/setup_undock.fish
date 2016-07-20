function setup_undock --description "Setup the environment after undocking"
  echo "Resetting input..."
  bash ~/.inputsetup
  echo "Resetting screen layout..."
  xrandr --auto
  pkill xmobar
  xmonad --restart
  pkill trayer
  setup-trayer
end
