#!/bin/bash

PREFIX=$1

shift

echo -n "param	"; head -1 $PREFIX-$1

for i in $*;
do
    awk "{if (i++) { printf(\"$i\t%s\n\", \$0) }}" $PREFIX-$i
done
