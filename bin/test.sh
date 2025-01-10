for i in ./test/*.hi.asm
do
    spirv-as "$i" -o "./test/out.spv"
    if ! spirv-val "./test/out.spv"; then
        echo "error in $i"
        exit 1;
    fi

done
