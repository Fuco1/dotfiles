function setup_dock --description "Setup the environment after redocking"
  echo "Resetting input..."
  bash ~/.inputsetup
  echo "Resetting screen layout..."
  bash ~/.screenlayout/logio.sh
  pkill trayer
  setup-trayer
end
