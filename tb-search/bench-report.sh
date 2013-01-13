#!/bin/sh

rm -r summary
mkdir summary

./bench3.sh random
./bench3.sh first

./bench3.sh random-1k
./bench3.sh random-16k
./bench3.sh random-64k
./bench3.sh random-256k
./bench3.sh random-1m
./bench3.sh random-8m

./bench3.sh stride-1
./bench3.sh stride-16
./bench3.sh stride-64
./bench3.sh stride-1k
./bench3.sh stride-16k
./bench3.sh stride-64k
./bench3.sh stride-256k
./bench3.sh stride-1m

tar -jcf bench.tar.bz2 summary/*
