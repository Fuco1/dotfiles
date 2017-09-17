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
alias pw "find ~/.password-store | sed -r 's/.*?\.password-store\/(.*?)\.gpg/\1/' | fzf | xargs pass -c"

# bindings
function fish_user_key_bindings
  bind \eg my_fish_grep
end

function runjava
  javac -cp "guava-17.0.jar" "$argv[1].java"; and java -ea -cp "guava-17.0.jar:." "$argv[1]"
end

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

#rvm default
