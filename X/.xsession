#xfce4-session
#!/usr/bin/env bash

[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources

# autocutsel -fork &
# autocutsel -selection PRIMARY -fork &
xcmenu --daemon

# /usr/bin/udisks --mount /dev/sda1
# /usr/bin/udisks --mount /dev/sda2

# start urxvt daemon
urxvtc
if [ $? -eq 2 ] ; then
    urxvtd -q -f -o
else
    pkill urxvtc
fi

# start mpd
if [ ! -f ~/.mpd/pid ] ; then
    mpd
fi

eval $(gpg-agent)
eval $(ssh-agent -s)
ssh-add "/home/matus/.ssh/id_rsa"

mountavfs
tagsistant "/home/matus/files"
feh --bg-scale '/home/matus/media/pictures/haskell.png'

bash /home/matus/.inputsetup
sh "/home/matus/.screenlayout/$(cat /home/matus/.whereami).sh"

bash /home/matus/bin/setup-tunnels
bash /home/matus/bin/setup-trayer

if [ ! -z $(which ibus-daemon) ]; then
    ibus-daemon --xim -d
fi

exec /usr/bin/xmonad
