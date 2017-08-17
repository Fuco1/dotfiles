function scdiff --description "Run diff inside docker-compose `app` and colorize with colordiff"
    docker-compose exec app diff -u "$argv[1]" "$argv[2]" | colordiff
end
