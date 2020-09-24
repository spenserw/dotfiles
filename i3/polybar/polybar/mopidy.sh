result=$( mpc | awk 'BEGIN { RS = "" ; FS = "\n" } { print $1 }')
result+=" | "
result+=$(mpc | awk 'BEGIN { RS = "\t" ; FS = "\n" } { print $2 }' | awk '{print $3, $4}')

echo $result
