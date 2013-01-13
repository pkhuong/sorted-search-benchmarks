#!/bin/sh

gcc -O3 -W -Wall -std=gnu99 leaf-search.c -lm -lc

mkdir bench;
rm bench/*;

for i in `seq 1 6`;
do
    echo "i = $i"
    ./a.out $i > bench/search-cache-$((1<<i))
    ./a.out $i 0 > bench/search-uncached-$((1<<i))
    ./a.out $i 1 0 2 > bench/half-search-cache-$((1<<i))
    ./a.out $i 0 0 2 > bench/half-search-uncached-$((1<<i))
done

rm bench/half-search-cache-{2,4}
rm bench/half-search-uncached-{2,4}

./a.out 6 1 47 49 > bench/half-search-cache-64s
./a.out 6 0 47 49 > bench/half-search-uncached-64s
sed -e 's/\t64\t/\t"64*"\t/' -i bench/half-search-cache-64s
sed -e 's/\t64\t/\t"64*"\t/' -i bench/half-search-uncached-64s

for i in `seq 3 6`;
do
    echo "i = $i (no search)"
    ./a.out $i 1 7 8 > bench/no-search-cache-$((1<<i))
    ./a.out $i 0 7 8 > bench/no-search-uncached-$((1<<i))
done

./a.out 5 1 31 32 > bench/no-search-cache-32s
./a.out 5 0 31 32 > bench/no-search-uncached-32s
sed -e 's/\t32\t/\t"32*"\t/' -i bench/no-search-cache-32s
sed -e 's/\t32\t/\t"32*"\t/' -i bench/no-search-uncached-32s

./a.out 6 1 63 64 > bench/no-search-cache-64s
./a.out 6 0 63 64 > bench/no-search-uncached-64s
sed -e 's/\t64\t/\t"64*"\t/' -i bench/no-search-cache-64s
sed -e 's/\t64\t/\t"64*"\t/' -i bench/no-search-uncached-64s


cat bench/search-cache-* | awk 'BEGIN{print "Search\tSize\tTicks"} ($1 != "Search") {print}' > bench/search-cache
cat bench/search-uncached-* | awk 'BEGIN{print "Search\tSize\tTicks"} ($1 != "Search") {print}' > bench/search-uncached
cat bench/half-search-cache-* | awk 'BEGIN{print "Search\tSize\tTicks"} ($1 != "Search") {print}' > bench/half-search-cache
cat bench/half-search-uncached-* | awk 'BEGIN{print "Search\tSize\tTicks"} ($1 != "Search") {print}' > bench/half-search-uncached
cat bench/no-search-cache-* | awk 'BEGIN{print "Search\tSize\tTicks"} ($1 != "Search") {print}' > bench/no-search-cache
cat bench/no-search-uncached-* | awk 'BEGIN{print "Search\tSize\tTicks"} ($1 != "Search") {print}' > bench/no-search-uncached

tar -jcf bench.tar.bz2 bench
