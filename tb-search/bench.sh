#!/bin/sh

rm -r bench
mkdir bench

./bench2.sh random
./bench2.sh first 0 1

./bench2.sh random-1k   0 1024
./bench2.sh random-16k  0 16384
./bench2.sh random-64k  0 65536
./bench2.sh random-256k 0 262144
./bench2.sh random-1m   0 $((1024*1024))
./bench2.sh random-8m   0 $((8*1024*1024))

./bench2.sh stride-1    -1 -1 1
./bench2.sh stride-16   -1 -1 17
./bench2.sh stride-64   -1 -1 65
./bench2.sh stride-1k   -1 -1 1025
./bench2.sh stride-16k  -1 -1 16385
./bench2.sh stride-64k  -1 -1 65537
./bench2.sh stride-256k -1 -1 262145
./bench2.sh stride-1m   -1 -1 $((1024*1024+1))
