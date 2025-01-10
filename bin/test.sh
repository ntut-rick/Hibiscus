#!/bin/bash

for i in ./test/*.hi
do
    echo "-------------------------------------------------------"
    echo "Start compile $i"
    echo "--"
    output=$(cabal v2-run hibiscus "$i" 2>&1)
    if [ $? -ne 0 ] ; then
        echo "$output"
        echo "error to compile $i";
        exit 1;
    fi
    echo "Ok"
done

for i in ./test/*.hi.asm
do
    echo "-------------------------------------------------------"
    echo "Start val $i"
    echo "--"
    spirv-as "$i" -o "./test/out.spv"
    if ! spirv-val "./test/out.spv"; then
        echo "error in $i";
        exit 1;
    fi
    echo "Ok"

done
