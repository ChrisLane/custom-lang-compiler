#!/bin/sh
set -e

dir=$1

cd $(dirname $0)

for file in **/*.src
do
    output=$(basename "$file" | cut -d. -f1)
    origin=$(dirname "$file")

    if [ -z "$(diff <(../main parse "$file" 2>&1 || true) $origin/output/$output.out)" ]; then
        echo "Passed test for file: $file"
    else
        echo "Failed test for file: $file"
    fi
done
