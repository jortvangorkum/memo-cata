#!/bin/bash
name=benchmarks/data/memory/run
i=0
while [[ -e $name-$i.txt || -L $name-$i.txt ]] ; do
    let i++
done
name=$name-$i
stack run memo-cata-regular-memory -- --regress="allocated:iters" +RTS -T > "${name}.txt"