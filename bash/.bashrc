[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"
[[ -s "$NVM_DIR/nvm.sh" ]] && source "$NVM_DIR/nvm.sh"
[[ -f ~/.fzf.bash ]] && source ~/.fzf.bash

source /home/matus/.local/bin/virtualenvwrapper.sh

complete -C /usr/bin/nomad nomad

complete -C /usr/bin/consul consul

[[ -s "/home/matus/.gvm/scripts/gvm" ]] && source "/home/matus/.gvm/scripts/gvm"
