function fkill --description "Kill a process using fzf-tmux selection"
    psg "$argv[1]" | fzf-tmux | awk '{print $2}' | xargs kill
end
