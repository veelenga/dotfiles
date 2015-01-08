lang=${1:-"us"}

if [ "$lang" == "us" ]
then
  layout=us,ua
else
  layout=ua,us
fi

setxkbmap -layout $layout -option -option caps:super

exit 0
