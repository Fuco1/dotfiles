function quad --description "Setup 4 tmux panes in a quad layout."
    tmux split-window -h
    tmux split-window
    tmux select-pane -L
    tmux split-window
    tmux select-pane -U
end
