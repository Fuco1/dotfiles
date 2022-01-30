export GOPATH="$HOME/dev/go"

# Set up PATH
export PATH="$PATH:$HOME/.cask/bin"
export PATH="$PATH:$HOME/.cabal/bin"
export PATH="$PATH:$HOME/.rvm/bin"
export PATH="$PATH:$HOME/.composer/vendor/bin"
export PATH="$PATH:$HOME/.config/composer/vendor/bin"
export PATH="$PATH:$GOPATH/bin"
export PATH="$PATH:/opt/Unity/Editor"
export PATH="$PATH:/opt/mssql-tools/bin"
export PATH="$HOME/.local/bin:$PATH"  # highest priority

export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_RUNTIME_HOME="$HOME/.local/tmp"
export XDG_CACHE_HOME="$HOME/.cache"

export TEXMFHOME="$HOME/texmf"
export TEXMFVAR="$HOME/.texmf-var"
export TEXMFCONFIG="$HOME/.texmf-config"

export EDITOR="emacsclient -nw"
export VISUAL="ecedit"

export HISTFILE="$XDG_DATA_HOME"/bash/history
export GPG_TTY=$(tty)
export MAIL="$HOME/Maildir"
export LEDGER_FILE="$HOME/org/ledger.ledger"
export AUTOSSH_POLL=60
export FZF_DEFAULT_OPTS="-x"
export LC_ALL=en_US.utf8
export LOCATE_PATH="$HOME/.config/.media.db"

export MPLAYER_HOME="$XDG_CONFIG_HOME"/mplayer
export NOTMUCH_CONFIG="$XDG_CONFIG_HOME"/notmuch/config
export NPM_CONFIG_USERCONFIG=$XDG_CONFIG_HOME/npm/npmrc
export NVM_DIR="$XDG_DATA_HOME"/nvm
export STACK_ROOT="$XDG_DATA_HOME"/stack

# to unconfuse AWT about Xmonad
export _JAVA_AWT_WM_NONREPARENTING=1
