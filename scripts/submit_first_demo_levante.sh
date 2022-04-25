#!/bin/sh

#SBATCH -A ka1176
#SBATCH --partition=compute
#SBATCH --time=00:10:00
#SBATCH --nodes=2
#SBATCH --tasks-per-node=6

module load intel-oneapi-compilers intel-oneapi-mpi

# limit stacksize ... adjust to your programs need
# and core file size
ulimit -s 102400
ulimit -c 0

export I_MPI_PMI_LIBRARY=/usr/lib64/libpmi.so

# change this to your git directory
cd ../first_demo
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:"${PWD}"

srun -l --cpu_bind=verbose --hint=nomultithread --distribution=block:cyclic ./my_demo_mpi
