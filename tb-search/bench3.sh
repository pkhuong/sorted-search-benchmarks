#!/bin/sh

awk -e '{ if (i++ == 0) { print "method\tsize\ttype\tcount" } else { printf("%s\t%u\t%s\t%f\n", $1, $2, "L1", $3); printf("%s\t%u\t%s\t%f\n", $1, $2, "L2", $4); printf("%s\t%u\t%s\t%f\n", $1, $2, "L3", $5); printf("%s\t%u\t%s\t%f\n", $1, $2, "TLB", $6); } }' < bench/cache-$1 > summary/cache-$1
awk -e 'BEGIN{srand()} { if ((i++ == 0) || (rand() < 0.01)) print $0; }' bench/table-$1 > summary/table-$1
