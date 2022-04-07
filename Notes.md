# Notes

Caroline Arnold, DKRZ, 2022

## Info

ICON version: 2.6.1

## Demo using CFFI

The Makefile compiles the shared object (dynamic library) `libplugin.so` along with the Fortran modules. 

### Mistral

To run the demo:

```bash
cd first_demo

module load intel intelmpi

make clean
make my_demo

# Need to add the current directory to the search path for SOs at runtime
# change this to your git root directory
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:"$PWD"


# need to update the pythonpath (?)
export PYTHONPATH=$PYTHONPATH:"$PWD"

# execute locally
./my_demo

# -- or --

# submit as a job
srun --pty -A ka1176 --partition=compute --exclusive --nodes=1 --tasks-per-node=1 --time=00:10:00 ./my_demo <nx1> <nx2>
```

Alternatively using MPI on the Fortran side:

```bash
make my_demo_mpi

# alternatively execute using MPI with 4 processes
mpirun -np 4 ./my_demo_mpi
```

Using MPI with multiple nodes in a job script:

```bash
#!/bin/sh

#SBATCH -A ka1176
#SBATCH --partition=compute
#SBATCH --time=00:10:00
#SBATCH --nodes=2
#SBATCH --tasks-per-node=6

module load intel intelmpi

export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:"$PWD"

mpirun -np 12 ./my_demo_mpi
```

Note from `https://cffi.readthedocs.io/en/latest/embedding.html`:

> You can avoid the LD_LIBRARY_PATH issue if you compile libmy_plugin.so with the path hard-coded inside in the first place.




### Levante

##### Conda environment

```bash
module load python3

# yes it was not so clever to copy paste now the env on levante is called iconml_mistral
conda create -n iconml_mistral
conda activate iconml_mistral
conda install pytorch cpuonly -c pytorch
conda install cffi
```

#### Run the demo

```bash
# load the required modules
module load python3
module load intel-oneapi-compilers intel-oneapi-mpi

cd first_demo

# activate the conda environment
conda init 
source ~/.bashrc
conda activate iconml_mistral

# make builds everything
make

# need to set both these variables to include the current directory
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:"$PWD"
export PYTHONPATH=$PYTHONPATH:"$PWD"

# execute
./my_demo
```

#### MPI

Like in the previous section, but add

```bash
export I_MPI_PMI_LIBRARY=/usr/lib64/libpmi.so

srun --pty -A ka1176 --partition=shared --mem=1024 --cpu_bind=verbose --hint=nomultithread --distribution=block:cyclic --nodes=1 --tasks-per-node=2 ./my_demo_mpi
```

TODO: Fails, figure out.

### Changing functions

Useful blog post: https://www.noahbrenowitz.com/post/calling-fortran-from-python/

Using CFFI: https://cffi.readthedocs.io/en/release-0.6/

> Any python function that we want to expose to fortran must be defined in 3 places. First, its C header declaration must be put in header.h. Second, its implementation must be defined in the module string of builder.py—or in an external module as described above. Finally, the fortran code must contain an interface block defining the subroutine.

## Demo using Pipes

### Mistral

Compiles and runs similar to the demo using CFFI.

## Demo using MPI

### Mistral

Python-Fortran bridge via MPI. How this bridge works. Let's first look at the sbatch script that runs the bridge. There are three lines in this script that are important to us. The line with the command `#SBATCH --nodes=2` reserves two nodes for computation. The line `#SBATCH --tasks-per-node=1` allocates one task per node. The command `mpirun -np 1 ./my_demo_mpi : -np 1 python3 python_main.py` forces `my_demo_mpi` and `python_main.py` code run together, with the `-np 1` command requiring 1 CPU for each code.

In Fortran and Python, we declare a common _MPI_Comm_WORLD_ communicator. Note that we have only two nodes, each with one job. Hence, we have only two processes. The process associated with Fortran gets rank 0, and with Python, it gets rank 1, respectively. Then the communication between the processes will be the same as in MPI within one communicator. Fortran sends a message indicating the address of process 1 (python), and python as an addressee indicates the address of process 0 (Fortran). The tags in _MPI_SEND/RECV_ are specified to keep the order of the outgoing/incoming messages.

To run the MPI bridge you need to install `mpi4py` in your python environment using e.g. `conda`. To run the code, follow the instructions below:

```bash
cd first_demo

module load intelmpi/2018.5.288 intel/18.0.4 python3/unstable

conda activate activate icon_mpi # replace with your conda environment

make clean

make

cd ../scripts

sbatch ./submit_first_demo_openMPI_mistral.sh

gedit mpi.o* &
```


