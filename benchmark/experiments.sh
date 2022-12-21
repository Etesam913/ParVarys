# experiment 1: garbage collection configuration
stack exec ParVarys-exe -- -t seq -n 2000 -i 1000 -e 1000 -F 500 +RTS -N1 -lf -s
stack exec ParVarys-exe -- -t seq -n 2000 -i 1000 -e 1000 -F 500 +RTS -N1 -lf -s -H8G -I10

# experiment 2: chunk size for lazy IntMap
# chunk sizes: 1, 10, 20, 50, 100
benchmark 3 -H8G -I10
chunkN=100 cp benchmark/results.txt \
              ~/Desktop/2022_fall/pfp/final_project/benchmark_results/final_chunk$chunkN.txt