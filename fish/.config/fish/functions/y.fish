function y --description "Play youtube video"
    set -l data (youtube-dl -e -g "$argv[1]" 2>/dev/null)
    echo $data[1]
    cvlc --play-and-exit --norepeat --novideo $data[2] 'vlc://quit'
end
