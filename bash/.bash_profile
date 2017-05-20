# interactive login-shell     => .bash_profile loads .profile and .bashrc
# non-interactive login-shell => .bash_profile loads .profile
# interactive non-login-shell => .bashrc

[[ -s "$HOME/.profile" ]] && source "$HOME/.profile"
[[ $- == *i* && -s "$HOME/.bashrc" ]] && source "$HOME/.bashrc"
