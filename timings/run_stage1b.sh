#!/bin/sh

nx1=$1
nx2=$2

nodes=$3
tasks=$4

cd ../first_demo

for count in `seq 1 10`; do
srun -A ka1176 --partition=compute --exclusive --nodes=$nodes --tasks-per-node=$tasks --time=00:10:00 ./my_demo_mpi $nx1 $nx2 > ../timings/logs_mpi_nodes-${nodes}_tasks-${tasks}/log_${nx1}_${nx2}_${count}.log
done
