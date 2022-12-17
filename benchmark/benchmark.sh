#!/usr/bin/env sh

HECs="2 4 8 16"

stack build

tss=""
for cores in $HECs
do
    echo "Running on $cores HECs"
    ts=$(stack exec ParVarys-exe -- +RTS -N$cores | \
       awk '/^Calculation\ Time:/{print $3 $4}')

    # append wall-clock time result
    test -z "$tss" && tss=$ts || tss="$tss, $ts"
done

echo $HECs | sed "s/\ /,\ /g"
echo $tss
