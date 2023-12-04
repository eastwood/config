#!/bin/bash 

AUTH_URL=https://api-gateway.nib.com.au/iam/oauth2/token

# Grab secret from cognito
client_id=779g899bo84tkq2dnjfs0o2bvh
client_secret=vf5goghknsnu9kg1lpfgtmgfq15abegtog8kpae6lm74gl29ffe

curl ${AUTH_URL} --user "${client_id}:${client_secret}" --data-urlencode 'grant_type=client_credentials' \
| jq -r ".access_token"
