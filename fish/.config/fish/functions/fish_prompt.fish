function get_pwd -d "Abbreviate $HOME in $PWD"
  echo $PWD | sed -e "s|^$HOME|~|"
end

function get_cwd_color -d "Get the color for cwd"
    xrdb -query | grep "URxvt.color12" | cut -f 2 | sed 's/#//'
end

function get_user_color -d "Get the color for user"
    xrdb -query | grep "URxvt.color10" | cut -f 2 | sed 's/#//'
end

function fish_prompt --description "Write out the prompt"
  set -l fish_prompt_cwd

  switch $USER
    case root
      set fish_prompt_cwd (set_color ff5f5f)
    case '*'
      set fish_prompt_cwd (set_color (get_cwd_color))
  end

  echo -n -s (set_color "$fish_color_autosuggestion") (date "+[%H:%M:%S]") (set_color (get_user_color)) "$USER" @ (hostname) (set_color normal) ":" "$fish_prompt_cwd" (get_pwd) (set_color normal) "
> "
end
