#!/bin/bash

error_flag=0

for i in ./test/*.hi
do
    echo "-------------------------------------------------------"
    echo "Start compile $i"
    echo "--"
    output=$(cabal v2-run hibiscus "$i" 2>&1)
    if [ $? -ne 0 ] ; then
        echo "$output"
        echo "error to compile $i"
        error_flag=1
    else
        echo "Ok"
    fi
done

for i in ./test/*.hi.asm
do
    echo "-------------------------------------------------------"
    echo "Start val $i"
    echo "--"
    spirv-as "$i" -o "./test/out.spv"
    if ! spirv-val "./test/out.spv"; then
        echo "error in $i"
        error_flag=1
    else
        echo "Ok"
    fi
done

# Exit with error if any step failed
if [ $error_flag -ne 0 ]; then
    exit 1
else
    exit 0
fi
