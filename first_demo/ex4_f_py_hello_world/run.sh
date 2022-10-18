#!/bin/sh

#exit 77
#exit 77

LD_LIBRARY_PATH="/work/ka1176/caroline/gitlab/yaxt/src/.libs:$LD_LIBRARY_PATH"; export LD_LIBRARY_PATH;
PYTHONPATH="/home/k/k202141/rootgit/yac/python:$PYTHONPATH"; export PYTHONPATH;

FC=/sw/spack-levante/openmpi-4.1.2-yfwe6t/bin/mpif90

module load intel-oneapi-mpi
echo "module load done"
${FC} -O2  -I/work/ka1176/caroline/gitlab/yac/src -I/work/ka1176/caroline/gitlab/yac/contrib -L/work/ka1176/caroline/gitlab/yaxt/src/.libs -I.  -I -I/work/ka1176/caroline/gitlab/yac/clapack/INCLUDE -I/work/ka1176/caroline/gitlab/yac/mtime/include -I/usr/include/libxml2 -I/work/ka1176/caroline/gitlab/yaxt/inst_headers -o helloworld.o helloworld.f90
echo "compilation done"

#source ~/.bashrc
#conda activate iconml
echo "conda activated"

/sw/spack-levante/openmpi-4.1.2-yfwe6t/bin/mpiexec -n 1 './helloworld.o' 0 : -n 1 python3 './helloworld.py' 1
