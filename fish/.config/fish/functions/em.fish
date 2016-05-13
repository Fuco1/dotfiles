function em --description "Run emacsclient in current directory"
    set -l dir (pwd)
    emacsclient -nw -e "(dired \"$dir\")"
end
