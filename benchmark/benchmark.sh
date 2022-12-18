#!/usr/bin/env sh
iter=$1
test -n $iter || iter=1
shift

varys_opts="-n 5000 -i 5000 -F 500"
ghc_opts="+RTS $@"
out="benchmark/results.txt"
HECs="1 2 4 6 8 10 12 14 16"

tSeq=""
tsPar=""

stack build

for i in $(seq $iter);
do
    echo
    echo "------------"
    echo "iteration $i"
    echo "------------"
    echo

    echo "Sequential Benchmarking"
    t=$(stack exec ParVarys-exe -- -t seq $ghc_opts -N1 | \
        awk '/^Calculation\ Time:/{print $3 $4}')
    test -z "$tSeq" && tSeq=$t || tSeq="$tSeq $t"

    echo "Parallel Benchmarking"
    for cores in $HECs
    do
        echo "Running on $cores HECs"
        t=$(stack exec ParVarys-exe -- $varys_opts $ghc_opts -N$cores | \
        awk '/^Calculation\ Time:/{print $3 $4}')

        # append wall-clock time result
        test -z "$tsPar" && tsPar=$t || tsPar="$tsPar $t"
    done
done

echo "# HECs: $HECs"
echo $HECs > $out
echo "Sequential Benchmark Results"
echo $tSeq
echo $tSeq >> $out
echo "Parallel Benchmark Results"
echo $tsPar
echo $tsPar >> $out
