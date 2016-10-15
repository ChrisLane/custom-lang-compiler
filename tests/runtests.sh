#!/bin/sh

dir=$1

for file in **/*.src
do
    output=$(basename "$file" | cut -d. -f1)
    origin=$(dirname "$file")
    
    if $(diff <(../main "$file") <(cat $origin/output/$output.out)); then
        echo "Passed test for file: $file"
    else
        echo "Failed test for file: $file"
    fi
done
