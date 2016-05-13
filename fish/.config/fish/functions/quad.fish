function quad --description "Setup 4 tmux panes in a quad layout."
    set -l recon (tmux-reconnect)
    tmux split-window -h "$recon"
    tmux split-window "$recon"
    tmux select-pane -L
    tmux split-window "$recon"
    tmux select-pane -U
end
