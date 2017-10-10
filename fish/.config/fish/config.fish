# exports
set -x PATH \
  "/opt/netbeans-8.0/bin" \
  "$HOME/.local/bin" \
  "$HOME/.config/composer/vendor/bin" \
  "$HOME/.composer/vendor/bin" \
  "$HOME/.cabal/bin" \
  "$HOME/.cask/bin" \
  "$HOME/bin" \
  "$HOME/sources/gocode/bin" \
  "/opt/Unity/Editor" \
  $PATH
set -x EDITOR "emacsclient -nw"
set -x VISUAL "/home/matus/bin/ecedit"
set -x XDG_CONFIG_HOME "/home/matus/.config"
set -x TEXMFHOME "/home/matus/texmf"
set -x TEXMFVAR "/home/matus/.texmf-var"
set -x TEXMFCONFIG "/home/matus/.texmf-config"
set -x GPG_TTY (tty)
set -x MAIL "$HOME/Maildir"
set -x LEDGER_FILE "$HOME/org/ledger.ledger"
# to unconfuse AWT about Xmonad
set -x _JAVA_AWT_WM_NONREPARENTING 1
set -x GPG_TTY (tty)
set -x AUTOSSH_POLL 60
set -x FZF_DEFAULT_OPTS "-x"
set -x LC_ALL en_US.utf8
set -x LOCATE_PATH "/home/matus/.config/.media.db"
set -x NVM_DIR "/home/matus/.nvm"

function nvm
    bass source ~/.nvm/nvm.sh --no-use ';' nvm $argv
end

eval (python -m virtualfish auto_activation)

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

alias dh "df -h"

# archives
alias untar "tar xvfz"

# ssh
alias dasnet "ssh fuco@dasnet.cz"
alias irc "ssh -t anaxagoras ta"

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

alias centos "sudo docker run -ti centos:7"

if test -n (echo $INSIDE_EMACS)
    if test -n (echo $TMUX)
        function -e fish_prompt prompt_AnSiT
            printf "\033Ptmux;\033\033AnSiTc %s\n\033\\" $PWD
            printf "\033Ptmux;\033\033AnSiTu %s\n\033\\" $LOGNAME
            printf "\033Ptmux;\033\033AnSiTh %s\n\033\\" (hostname -s)
            printf "\033Ptmux;\033\033AnSiTt %s\n\033\\" (tmux display-message -p '#S:#I.#P')
        end
    else
        function -e fish_prompt prompt_AnSiT
            printf "\033AnSiTc %s\n" $PWD
            printf "\033AnSiTu %s\n" $LOGNAME
            printf "\033AnSiTh %s\n" (hostname -s)
        end
    end
end

function preexec_set_title --on-event fish_preexec
    echo -ne '\033k'$argv[1]'\033\\'
end

function postexec_set_title --on-event fish_postexec
    echo -ne '\033kfish\033\\'
end

rvm default
