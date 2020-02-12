function my_fish_awk --description "AWK the output of current command"
    set -l cmd awk
    if commandline -j | grep -v "$cmd *\$" >/dev/null
        commandline -aj " | $cmd '{print }'"
        commandline -f end-of-line
        commandline -f backward-char
        commandline -f backward-char
    end
end
