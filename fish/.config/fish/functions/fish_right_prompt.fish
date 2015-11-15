function my_git_branch -d "Print the current git branch or commit"
  set -l branch (git rev-parse --abbrev-ref HEAD 2> /dev/null)

  if test $branch = "HEAD"
    echo -n $current_commit | cut -b 1-7
  else
    echo -n $branch
  end
end

function my_git_status -d "Check if any file changed in the repository"
  git diff-files --quiet --ignore-submodules 2>/dev/null

  echo -n $status
end

function my_git_print_prompt -d "Print the git information prompt"
  # Print the checked out branch name or commit hash of a git repository.
  set -l current_commit (git rev-parse HEAD 2> /dev/null)

  if test $status -gt 0
    return
  end

  echo -n ":"

  if test (my_git_status) = 0
    set_color green
  else
    set_color red
  end

  echo -n (my_git_branch)

  set_color normal
end

function fish_right_prompt -d "Write out the right prompt"
  set -l last_status $status

  # Print a red dot for failed commands.
  if test $last_status -gt 0
    set_color red;    echo -n "($last_status)"
    set_color normal; echo -n " "
  end

  if set -q VIRTUAL_ENV
    echo -n -s (set_color red) "{" (basename "$VIRTUAL_ENV") "}" (set_color normal)
  end

  # Print the checked out branch name or commit hash of a git repository.
  my_git_print_prompt

  set_color normal
end
