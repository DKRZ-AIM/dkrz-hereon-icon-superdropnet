#!/bin/sh

nx1=$1
nx2=$2

cd ../first_demo

for count in `seq 1 10`; do
srun -A ka1176 --partition=compute --exclusive --nodes=1 --tasks-per-node=1 --time=00:10:00 ./my_demo $nx1 $nx2 > ../timings/logs/log_${nx1}_${nx2}_${count}.log
done
