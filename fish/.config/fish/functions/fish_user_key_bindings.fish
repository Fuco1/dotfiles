# bindings
function fish_user_key_bindings
  bind \eg my_fish_grep
  bind \ee 'begin; set -l _visual "$VISUAL"; set -e VISUAL; edit_command_buffer; set -x VISUAL "$_visual"; end'
end
