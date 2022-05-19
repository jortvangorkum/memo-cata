#!/bin/bash

run_memory_benchmark () {
    path="benchmarks/data/memory/run"
    i=0
    while [[ -e $path-$i.txt || -L $path-$i.txt || -d $path-$i ]] ; do
        let i++
    done
    path=$path-$i

    benchmarks=$(stack run memo-cata-regular-memory -- --list)
    readarray -t y <<< $benchmarks
    
    for benchmark in "${y[@]}"
    do
        dir="$(dirname "$path/$benchmark")"
        mkdir -p "$dir"
        stack run memo-cata-regular-memory -- --match glob "$benchmark" +RTS -t --machine-readable 2> "$path/$benchmark.txt"
    done
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

# run_time_benchmark
run_memory_benchmark