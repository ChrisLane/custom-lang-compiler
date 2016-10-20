#!/bin/sh
set -e

cd $(dirname $0)

fails=0

echo
echo "Parsing Tests"
echo "============="

for file in parse/**/*.src
do
    output=$(basename "$file" | cut -d. -f1)
    origin=$(dirname "$file")

    if [ -z "$(diff <(../main parse "$file" 2>&1 || true) $origin/output/$output.out)" ]; then
        echo "Passed test for file: $file"
    else
        echo "Failed test for file: $file" && fails=1
    fi
done

echo
echo "Evaluation Tests"
echo "================"

for file in evaluate/**/*.src
do
    output=$(basename "$file" | cut -d. -f1)
    origin=$(dirname "$file")

    if [ -z "$(diff <(../main evaluate "$file" 2>&1 || true) $origin/output/$output.out)" ]; then
        echo "Passed test for file: $file"
    else
        echo "Failed test for file: $file" && fails=1
    fi
done

echo
exit $fails
