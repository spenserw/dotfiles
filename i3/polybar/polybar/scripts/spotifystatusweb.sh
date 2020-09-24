#!/bin/sh
main() {
  resp=$(curl -X "GET" \
    "https://api.spotify.com/v1/me/player/currently-playing?market=ES" \
    -H "Accept: application/json" \
    -H "Content-Type: application/json" \
    -H "Authorization: Bearer "
  )

  contexttype=$(echo $resp | jq -r '.context.type')

  trackname=$(echo $resp | jq -r '.item.name')
  album=$(echo $resp | jq -r '.item.album.name')
  artist=$(echo $resp | jq -r '.item.artists[0].name')

  echo "'$trackname' | $album | $artist"
  # echo "$resp"
}

main "$@"
