
VOLUME_STATUS=$(amixer | awk '/  Front Left: Playback/ {print $6}')
DEVICE=$(pacmd list-sinks | grep 'active port' | awk '{print $3}')


#if [ $DEVICE != 39314 ] 
#then
    #echo -n ' '
#fi

if [ "${VOLUME_STATUS}" == "[off]" ]
then 
    echo ' 0%'
    echo "#FF8000"
    exit 1
fi

VOLUME=$( amixer | awk '/^  Front Left: Playback/ {print $5}' | grep -Go "[0-9]*")

if [ $VOLUME -gt 59 ]
then 
    echo -n ' ' 
    echo -n "${VOLUME}%"
elif [ $VOLUME -gt -21 ]
then 
    echo -n ' ' 
    echo -n "${VOLUME}%"
else
    echo ' sdfkj' 
    echo "${VOLUME}%"
fi

if [ "${DEVICE}" == "<analog-output-headphones>" ]
then
    echo '  '
else
    echo ""
fi    

#if [ $DEVICE != 39314 ] 
#then
    #echo '  '
#fi
