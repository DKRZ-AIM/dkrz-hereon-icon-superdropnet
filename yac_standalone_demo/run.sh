#!/bin/sh

# Coupled python : python run with stand-alone yac
#
# Caroline Arnold 15.11.22 
#
# 
# --- Dummy-ICON ----- Python
# --- PUT ic2py  ----- GET ic2py -> v
# ---     |      ----- v' = f(v)
# --- GET py2ic  ----- PUT py2ic(v')
# 
# Paths can be adapted for bundled ICON as 
# given in the documentation

# change to your paths
YAC_ROOT=/work/ka1176/caroline/gitlab/yac
YAXT_ROOT=/work/ka1176/caroline/gitlab/yaxt

source ~/.bashrc
conda activate iconml

PYTHONPATH="${YAC_ROOT}/python:$PYTHONPATH"; export PYTHONPATH;
LD_LIBRARY_PATH="${YAXT_ROOT}/src/.libs:$LD_LIBRARY_PATH"; export LD_LIBRARY_PATH;

# For bundled yac, these were required as well
# i needed to add these as well
LD_LIBRARY_PATH="/sw/spack-levante/mambaforge-4.11.0-0-Linux-x86_64-sobz6z/lib/:$LD_LIBRARY_PATH"; export LD_LIBRARY_PATH;
LD_LIBRARY_PATH="/sw/spack-levante/eccodes-2.26.0-o6jniw/lib64/:$LD_LIBRARY_PATH"; export LD_LIBRARY_PATH;

/sw/spack-levante/openmpi-4.1.2-yfwe6t/bin/mpiexec -n 1 python3 yac_caroline_dummy.py : -n 1 python3 yac_caroline_test.py
