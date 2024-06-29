function get_nomad_token
    nomad login -json | jq -r .SecretID | read NOMAD_TOKEN
    set -Ux NOMAD_TOKEN $NOMAD_TOKEN
end
