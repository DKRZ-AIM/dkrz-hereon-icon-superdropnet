#!/bin/sh

#exit 77
#exit 77

LD_LIBRARY_PATH="/work/ka1176/caroline/gitlab/yaxt/src/.libs:$LD_LIBRARY_PATH"; export LD_LIBRARY_PATH;
PYTHONPATH="/home/k/k202141/rootgit/yac/python:$PYTHONPATH"; export PYTHONPATH;

module load intel-oneapi-mpi
echo "module load done"
mpiifort -O2 -I/work/ka1176/caroline/gitlab/yac/src/ -o helloworld.o helloworld.f90
echo "compilation done"

/sw/spack-levante/openmpi-4.1.2-yfwe6t/bin/mpiexec -n 1 './helloworld.o' 0 : -n 1 python3 './helloworld.py' 1
