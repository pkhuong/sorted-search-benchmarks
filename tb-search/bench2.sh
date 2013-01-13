#!/bin/sh

PREFIX=$1
shift

for i in `seq 4 30`;
do
    time ./bench1.sh $PREFIX-$i $i $*
done

echo 'method\tsize\tL1\tL2\tL3\tTLB' > bench/cache-$PREFIX
cat bench/cache-$PREFIX-* >> bench/cache-$PREFIX
rm bench/cache-$PREFIX-*
echo 'method\tsize\tcycles' > bench/table-$PREFIX
cat bench/table-$PREFIX-* >> bench/table-$PREFIX
rm bench/table-$PREFIX-*
