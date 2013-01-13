#!/bin/sh

SUFFIX=$1

shift

[ -f bench/table-$SUFFIX ] && rm bench/table-$SUFFIX
[ -f bench/cache-$SUFFIX ] && rm bench/cache-$SUFFIX


for I in `seq 0 9`;
do
    TCMALLOC_LARGE_ALLOC_REPORT_THRESHOLD=$((20*1024*1024*1024)) LD_LIBRARY_PATH=$HOME/lib/ numactl -m 1 -N 1 ./parallel $* >> bench/table-$SUFFIX 2>> bench/cache-$SUFFIX
    TCMALLOC_LARGE_ALLOC_REPORT_THRESHOLD=$((20*1024*1024*1024)) LD_LIBRARY_PATH=$HOME/lib/ numactl -m 1 -N 1 ./serial $* >> bench/table-$SUFFIX 2>> bench/cache-$SUFFIX
done
