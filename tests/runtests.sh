#!/usr/bin env sh
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
echo "Optimisation Tests"
echo "================"

for file in evaluate/**/*.src
do
    output=$(basename "$file" | cut -d. -f1)
    origin=$(dirname "$file")

    if [ -z "$(diff <(../main evaluate -o "$file" 2>&1 || true) $origin/output/$output.out)" ]; then
        echo "Passed test for file: $file"
    else
        echo "Failed test for file: $file" && fails=1
    fi
done

echo
for file in optimise/**/*.src
do
    output=$(basename "$file" | cut -d. -f1)
    origin=$(dirname "$file")

    if [ -z "$(diff <(../main parse -o "$file" 2>&1 || true) $origin/output/$output.out)" ]; then
        echo "Passed test for file: $file"
    else
        echo "Failed test for file: $file" && fails=1
    fi
done

echo
echo "Interpret Tests"
echo "================"

for file in interpret/**/*.src
do
    output=$(basename "$file" | cut -d. -f1)
    origin=$(dirname "$file")

    if [ -z "$(diff <(../main interpret "$file" 2>&1 || true) $origin/output/$output.out)" ]; then
        echo "Passed test for file: $file"
    else
        echo "Failed test for file: $file" && fails=1
    fi
done

echo
echo "Codegen Tests"
echo "================"

for file in codegen/**/*.src
do
    output=$(basename "$file" | cut -d. -f1)
    origin=$(dirname "$file")

    if [ -z "$(diff <(../main codegen "$file" 2>&1 || true) $origin/output/$output.out)" ]; then
        echo "Passed test for file: $file"
    else
        echo "Failed test for file: $file" && fails=1
    fi
done

echo
echo "Compile Tests"
echo "================"

for file in compile/**/*.src
do
    output=$(basename "$file" | cut -d. -f1)
    origin=$(dirname "$file")

    if [ -z "$(diff <(./compileandrun.sh "tests/$file" 2>&1 || true) $origin/output/$output.out)" ]; then
        echo "Passed test for file: $file"
    else
        echo "Failed test for file: $file" && fails=1
    fi
done

echo
exit $fails
