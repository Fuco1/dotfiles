#xfce4-session
#!/usr/bin/env bash

[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources

# start urxvt daemon
urxvtc
if [ $? -eq 2 ] ; then
    urxvtd -q -f -o
else
    pkill urxvtc
fi

if [ ! -z $(which twmnd) ]; then
   pkill notify-osd
   pkill notifyd
   twmnd &
fi

eval $(gpg-agent --daemon --write-env-file "/home/matus/.cache/gpg-agent-info")
eval $(ssh-agent -s)
ssh-add "/home/matus/.ssh/id_rsa"
ssh-add "/home/matus/.ssh/id_rsa_personal"

bash /home/matus/bin/inputsetup
sh "/home/matus/.screenlayout/$(cat /home/matus/.whereami).sh"

bash /home/matus/bin/setup-trayer

exec /home/matus/bin/xmonad
