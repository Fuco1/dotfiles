# exports
set -x PATH "/opt/netbeans-8.0/bin" "$HOME/.cabal/bin" "$HOME/.cask/bin" "$HOME/bin" "$HOME/sources/gocode/bin" $PATH
set -x EDITOR "emacsclient -nw"
set -x VISUAL "/home/matus/bin/ecedit"
set -x XDG_CONFIG_HOME "/home/matus/.config"
set -x TEXMFHOME "/home/matus/texmf"
set -x TEXMFVAR "/home/matus/.texmf-var"
set -x TEXMFCONFIG "/home/matus/.texmf-config"
set -x GPG_TTY (tty)
set -x MAIL "$HOME/Maildir"
# to unconfuse AWT about Xmonad
set -x _JAVA_AWT_WM_NONREPARENTING 1
set -x GPG_TTY (tty)
set -x AUTOSSH_POLL 60

# ls aliases
alias ll "ls -l --group-directories-first"
alias la "ls -A --group-directories-first"
alias lla "ls -lA --group-directories-first"
alias lle "ll --color-never | le"
alias lg "ls -lA | grep -i "

# navigation
alias .. "cd .."
alias ... "cd ../.."
alias .... "cd ../../.."
alias ..... "cd ../../../.."
alias ...... "cd ../../../../.."

# text viewers/editors
alias le "less -MN"

# processes
alias psef='ps -ef'
alias psg='ps -ef | grep '
alias topu="top -u $USER"

# directory commands
alias ds "du -sm"
alias du1 "du -h --max-depth 1"
alias mc "mc -S fuco"

# archives
alias untar "tar xvfz"

# ssh
alias dasnet "ssh fuco@dasnet.cz"

# app installation
alias ins "sudo apt-get install"
alias agr "sudo apt-get remove"
alias acs "sudo apt-cache search"

# general
alias ta "tmux attach"
alias grep "pcregrep --color=auto"
alias yd "youtube-dl"
alias vlca "vlc --vout none"
alias cvlca "cvlc --vout none"

# bindings
function fish_user_key_bindings
  bind \eg my_fish_grep
end

function runjava
  javac -cp "guava-17.0.jar" "$argv[1].java"; and java -ea -cp "guava-17.0.jar:." "$argv[1]"
end