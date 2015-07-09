function my_fish_grep --description "Grep the output of current command"
  set -l cmd grep
  if commandline -j | grep -v "$cmd *\$" >/dev/null
    commandline -aj " | $cmd "
    commandline -f end-of-line
  end
end
