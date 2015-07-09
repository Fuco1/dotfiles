function __my_mpc_using_command
  set cmd (commandline -opc)
  if [ (count $cmd) -gt 1 ]
    if [ $argv[1] = $cmd[2] ]
      return 0
    end
  end
  return 1
end

function __my_mpc_get_update_candidates
  set -l ctoken (commandline -ct)
  for f in ~/media/music/"$ctoken"*/
    echo $f | sed 's/\/home\/matus\/media\/music\///'
  end
end

complete -f -c mpc -n '__my_mpc_using_command update' -a '(__my_mpc_get_update_candidates)'
