
VOLUME_STATUS=$(amixer | awk '/  Front Left: Playback/ {print $6}')


if [ "${VOLUME_STATUS}" == "[off]" ]
then 
    echo ' 0%'
    echo "#FF8000"
    exit 1
fi

VOLUME=$(amixer | awk '/^  Front Left: Playback/ {print $5}' | cut -b 2-3)


echo -n ' ' 
echo "${VOLUME}%"
# 
# amixer | awk '/^  Front Left: Playback/ {print $5}' | cut -b 2-3