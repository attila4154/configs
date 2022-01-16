#!/bin/bash

DATE=$(date +%d.%m.%Yi)

RESPONSE=$(curl --silent https://www.cnb.cz/cs/financni-trhy/devizovy-trh/kurzy-devizoveho-trhu/kurzy-devizoveho-trhu/denni_kurz.txt?date=${DATE} | grep -i rub)

BY=$(echo "${RESPONSE}" | sed 's/|/\t/g' | cut -f 3) 
VALUE=$(echo "${RESPONSE}" | sed 's/|/\t/g' | cut -f 5 | sed 's/\,/\./g')
RESULT=$(echo $BY / $VALUE | bc -l | cut -c -5)


echo -n "CZK/RUB: "
echo $RESULT
#echo ${VALUE}
#echo ${BY}










#sanya lox
