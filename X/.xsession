#xfce4-session
#!/usr/bin/env bash

[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources

if [ ! -z $(which xcmenu) ]; then
    xcmenu --daemon
fi

# start urxvt daemon
if [ ! -z $(which urxvtc ) ]; then
    urxvtc
    if [ $? -eq 2 ] ; then
       urxvtd -q -f -o
    else
       pkill urxvtc
    fi
fi

# start mpDris to enable mpris control of mpd
if [ ! -z $(which mpDris2) ]; then
    mpDris2 &
fi

eval $(ssh-agent -s)
ssh-add "/home/matus/.ssh/id_rsa"
ssh-add "/home/matus/.ssh/id_rsa_democritos"

mountavfs
if [ ! -z $(which tagsistant) ]; then
    tagsistant "/home/matus/files"
fi

bash /home/matus/.local/bin/inputsetup
sh "/home/matus/.screenlayout/$(cat /home/matus/.whereami).sh"

bash /home/matus/.local/bin/setup-trayer

if [ ! -z $(which ibus-daemon) ]; then
    ibus-daemon --xim -d
fi

exec /home/matus/.local/bin/xmonad
