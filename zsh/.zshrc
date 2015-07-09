[[ $TERM == "dumb" ]] && unsetopt zle && PS1='$ ' && return

#_* Defaults from compinstall
# The following lines were added by compinstall
zstyle ':completion:*' completer _complete _ignored _approximate
zstyle ':completion:*' completions 1
zstyle ':completion:*' glob 1
zstyle ':completion:*' matcher-list 'm:{[:lower:][:upper:]}={[:upper:][:lower:]} r:|[._-]=* r:|=*' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]} r:|[._-]=* r:|=* l:|=*' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]} r:|[._-]=* r:|=* l:|=*' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]} r:|[._-]=* r:|=* l:|=*'
zstyle ':completion:*' max-errors 2 not-numeric
zstyle ':completion:*' prompt '%e'
zstyle ':completion:*' substitute 1
zstyle :compinstall filename '/home/matus/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall
# Lines configured by zsh-newuser-install
HISTFILE=~/.zsh_histfile
HISTSIZE=10000
SAVEHIST=10000
setopt autocd extendedglob notify chaselinks
unsetopt beep
bindkey -e
# End of lines configured by zsh-newuser-install

#_* Basic settings
setopt APPEND_HISTORY
setopt INC_APPEND_HISTORY
setopt EXTENDED_HISTORY
setopt HIST_IGNORE_DUPS
setopt HIST_ALLOW_CLOBBER
setopt HIST_REDUCE_BLANKS

#_* prompt settings
# allow substitution on prompt
setopt PROMPT_SUBST

# setup colors
autoload colors zsh/terminfo
if [[ "$terminfo[colors]" -ge 8 ]]; then
    colors
fi
for color in RED GREEN YELLOW BLUE MAGENTA CYAN WHITE GREY; do
    eval PR_$color='%{$terminfo[bold]$fg[${(L)color}]%}'
    eval PR_LIGHT_$color='%{$fg[${(L)color}]%}'
    (( count = $count + 1 ))
done
PR_NO_COLOR="%{$terminfo[sgr0]%}"

# [time]{subshell test, prints +}user@host:pwd(last error status)>%
PS1="${PR_GREY}[%D{%H:%M:%S}]%(3L.+.)${PR_GREEN}%n@%m${PR_NO_COLOR}:${PR_BLUE}%~${PR_NO_COLOR}
>%(?..${PR_RED}(%?%))${PR_NO_COLOR}%# "
PS2="%_>"

#_* aliases & functions
if [[ -f ~/.config/zsh/aliases ]]; then
    . ~/.config/zsh/aliases
fi

if [[ -f ~/.config/zsh/scripts ]]; then
    . ~/.config/zsh/scripts
fi

#_* source highlight on the fly
source ~/.config/zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets pattern)
# styles for main highlighter
ZSH_HIGHLIGHT_STYLES[globbing]='fg=cyan'

bindkey -e '^w' kill-region
