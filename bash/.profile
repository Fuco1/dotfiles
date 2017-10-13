export NVM_DIR="/home/matus/.nvm"

# Set up PATH
export PATH="$PATH:$HOME/.cask/bin"
export PATH="$PATH:$HOME/.cabal/bin"
export PATH="$PATH:$HOME/.rvm/bin"
export PATH="$PATH:$HOME/.composer/vendor/bin"
export PATH="$PATH:$HOME/sources/gocode/bin"
export PATH="$PATH:/opt/Unity/Editor"
export PATH="$HOME/.local/bin:$PATH"  # highest priority

export EDITOR="emacsclient -nw"
export VISUAL="$HOME/bin/ecedit"
export XDG_CONFIG_HOME="$HOME/.config"
export TEXMFHOME="$HOME/texmf"
export TEXMFVAR="$HOME/.texmf-var"
export TEXMFCONFIG="$HOME/.texmf-config"
export GPG_TTY=$(tty)
export MAIL="$HOME/Maildir"
export LEDGER_FILE="$HOME/org/ledger.ledger"
export AUTOSSH_POLL=60
export FZF_DEFAULT_OPTS="-x"
export LC_ALL=en_US.utf8
export LOCATE_PATH="$HOME/.config/.media.db"

# to unconfuse AWT about Xmonad
export _JAVA_AWT_WM_NONREPARENTING=1
