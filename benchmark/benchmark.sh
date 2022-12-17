#!/usr/bin/env sh
sequentialHECs="1"
HECs="1 2 4 8 16"

stack build

tssSeq=""
tssPar=""

echo "Sequential Benchmarking"
for cores in $sequentialHECs
do
    echo "Running on $cores HECs"
    ts=$(stack exec ParVarys-exe -- -t "seq" +RTS -N$cores | \
       awk '/^Calculation\ Time:/{print $3 $4}')

    # append wall-clock time result
    test -z "$tssSeq" && tssSeq=$ts || tssSeq="$tssSeq, $ts"
done

echo "Parallel Benchmarking"
for cores in $HECs
do
    echo "Running on $cores HECs"
    ts=$(stack exec ParVarys-exe -- +RTS -N$cores | \
       awk '/^Calculation\ Time:/{print $3 $4}')

    # append wall-clock time result
    test -z "$tssPar" && tssPar=$ts || tssPar="$tssPar, $ts"
done

echo $HECs | sed "s/\ /,\ /g"
echo "Sequential Benchmark Results"
echo $tssSeq
echo $tssSeq > benchmark/results.txt
echo "Parallel Benchmark Results"
echo $tssPar
echo $tssPar >> benchmark/results.txt
