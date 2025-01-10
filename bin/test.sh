#!/bin/bash

for i in ./test/*.hi
do
    if ! cabal v2-run hibiscus $i ; then
        echo "error to compile $i";
        exit 1;
    fi
done

for i in ./test/*.hi.asm
do
    spirv-as "$i" -o "./test/out.spv"
    if ! spirv-val "./test/out.spv"; then
        echo "error in $i";
        exit 1;
    fi

done
