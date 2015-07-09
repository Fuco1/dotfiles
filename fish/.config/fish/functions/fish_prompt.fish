function get_pwd -d "Abbreviate $HOME in $PWD"
  echo $PWD | sed -e "s|^$HOME|~|"
end

function fish_prompt --description "Write out the prompt"
  set -l fish_prompt_cwd
  switch $USER
    case root
      set fish_prompt_cwd (set_color ff5f5f)
    case '*'
      set fish_prompt_cwd (set_color 5f5fff)
  end
  echo -n -s (set_color "$fish_color_autosuggestion") (date "+[%H:%M:%S]") (set_color 55ff55) "$USER" @ (hostname) (set_color normal) ":" "$fish_prompt_cwd" (get_pwd) (set_color normal) "
> "
end
