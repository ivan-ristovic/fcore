#!/bin/bash

set -e;

stack=$(ghcup whereis stack)

$stack test
$stack build

dir=samples

ignore_list=(
)

for src in "$dir"/*.fc; do
    if echo "${ignore_list[@]}" | grep -qw "$src"; then
        echo "ignoring : $src"
    else
        echo "executing: $src"
        $stack run -- -vv "$src" > "$src.out" # 2>&1
    fi
    sleep 1
done

