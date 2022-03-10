#!/bin/sh

#SBATCH -A ka1176
#SBATCH --partition=compute
#SBATCH --time=00:10:00
#SBATCH --nodes=2
#SBATCH --tasks-per-node=3

module load intel intelmpi

export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:"/work/ka1176/caroline/gitlab/2022-03-hereon-python-fortran-bridges/first_demo"

map --profile srun ./my_demo_mpi
