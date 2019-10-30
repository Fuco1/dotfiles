#!/bin/bash

notify_timeout=5000

# notify <'ok'|'error'> <title> <text>
function notify() {
  if [ "$1" = "error" ]; then
      notify-send -a ImgurScreenshot -u critical -c "im.error" -i "$imgur_icon_path" -t "$notify_timeout" "$2" "$3"
  else
      notify-send -a ImgurScreenshot -u low -c "transfer.complete" -i "$imgur_icon_path" -t "$notify_timeout" "$2" "$3"
  fi
  sleep $(( notify_timeout / 1000 ))
}
