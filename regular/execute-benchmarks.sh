#!/bin/bash

run_memory_benchmark () {
    name="benchmarks/data/memory/run"
    i=0
    while [[ -e $name-$i.txt || -L $name-$i.txt ]] ; do
        let i++
    done
    name=$name-$i
    stack run memo-cata-regular-memory -- --regress="allocated:iters" +RTS -T > "${name}.txt"
}

run_time_benchmark () {
    name="benchmarks/data/time/run"
    i=0
    while [[ -e $name-$i.csv || -L $name-$i.csv ]] ; do
        let i++
    done
    name=$name-$i
    stack run memo-cata-regular-time -- --csv="${name}.csv" 
}

run_memory_benchmark
run_time_benchmark