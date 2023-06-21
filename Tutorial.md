# Tutorial

[20.06.23] Caroline & Shivani

If any of the steps fail, please contact arnold@dkrz.de.

## Log in to Levante

Use your Levante account `k.../b.../g...` to login. You need to be associated with a valid project, here we use our AIM project `ka1176`.

```bash
ssh k......@levante.dkrz.de
```

## Obtain the code

### Fresh install

We are working in a fork of `icon-aes` version 2.6.5. You need a valid ssh key for DKRZ Gitlab. Clone the feature branch `icon-aes-ml-bridges` including the external repositories that are included as git submodules. You may need to enter your username and password to access the external repositories. 

> We refer to the root directory of this git repository as `~icon-aes` in the context of this tutorial.

```bash
module load git
git clone --recursive -b icon-aes-ml-bridges git@gitlab.dkrz.de:k202141/icon-aes.git
```

The Python code and analysis scripts are included as a git submodule in `~icon-aes/externals/mlbridges`. To obtain the repository separately, run

```bash
git clone git@gitlab.dkrz.de:aim/2022-03-hereon-python-fortran-bridges.git
```

### Update

To update the code, and the external submodules, run

```bash
cd ~icon-aes
git pull                         # for the icon-aes fork
git submodule update --recursive # for the git submodules
```


## Configure

Allocate an interactive job, this can be very slow on the login nodes.

```bash
salloc -A ka1176 --partition=interactive --ntasks=8 --time=04:00:00
```

You receive a node allocation `l40...`

```bash
ssh l40...
cd ~icon-aes
```

`autoconf` tools were missing from the subrepository. Add them

```bash
cd externals/mlbridges
automake --add-missing
cd ~icon-aes
```

Configure for using CFFI and Pipes bridge (if you do not need the YAC bridge):

```bash
 ./config/dkrz/levante.intel-2021.5.0 --enable-mlbridges
```

> :warning: This has not been tested (TODO)
>
> Configure for using CFFI, Pipes and YAC bridge:
> 
> ```bash
> module load python3
>  ./config/dkrz/levante.intel-2021.5.0 --enable-mlbridges --enable-python-bindings
> ```

## Make `icon`

The two modules need to be loaded to make the pipes interface - TODO add them to the config files.

```bash
cd ~icon-aes

intel-oneapi-compilers/2022.0.1-gcc-11.2.0
intel-oneapi-mpi/2021.5.0-intel-2021.5.0

make -j8
```

## Set up Python environment

Follow the instructions to set up miniconda: https://docs.dkrz.de/doc/software&services/machine-learning/conda.html

Run the following:

```bash
source ~/.bashrc
cd ~icon-aes/externals/mlbridges
conda env create --file docker/kernel-env-cuda11.yaml
```

To add the machine learning libraries, run

```bash
conda activate iconml
pip install pytorch_lightning==1.7.0
pip install torch==1.11.0 torchvision==0.12.0 torchaudio==0.11.0
```


## Obtain runscripts

Copy the two warm bubble runscripts for CFFI (SuperdropNet) and Fortran (bulk moment scheme):

```bash
cd ~icon-aes
cp /work/ka1176/caroline/gitlab/2022-03-hereon-python-fortran-bridges/icon_runscripts/exp.warm_bubble_fortran run/
cp /work/ka1176/caroline/gitlab/2022-03-hereon-python-fortran-bridges/icon_runscripts/exp.warm_bubble_cffi run/
```

:warning: Important: Replace the paths to the best model to YOUR `~icon-aes`

```
> mlbridges_config%model_path = '~icon-aes/externals/mlbridges/cffi_interface/trained_models/best_model.ckpt'
```

```bash
cd ~icon-aes
./make_runscripts warm_bubble_fortran
./make_runscripts warm_bubble_cffi
```

**Make the following edits to the run scripts that were just generated**

:warning: As of 19.06.2023 there was an issue with the variable `icon_data_rootFolder`. Edit the runscript manually so that the corresponding variable starts with `/pool` instead of the current prefix.

:warning: As of 19.06.2023 there was an issue with the START command. Replace

```bash
export START="srun -l --kill-on-bad-exit=1 --nodes=${SLURM_JOB_NUM_NODES:-1} --hint=nomultithread --ntasks=$((no_of_nodes * mpi_procs_pernode)) --ntasks-per-node=${mpi_procs_pernode} --cpus-per-task=${OMP_NUM_THREADS}"
```

:warning: Increase the stack size

```bash
# limit stacksize ... adjust to your programs need
# and core file size
ulimit -s 204800
ulimit -c 0
```

## Run the warm bubble scenario

### Fortran

Run the experiment

```bash
cd run
sbatch -A ka1176 --partition=compute -o log_fortran --time=00:30:00 --nodes=1 exp.warm_bubble_fortran.run
```

### CFFI

Activate the conda environment and update the Python path

```bash
source ~/.bashrc
conda activate iconml
export PYTHONPATH=${PWD}/externals/mlbridges/cffi_interface
```

Run the experiment

```bash
cd run
sbatch -A ka1176 --partition=compute -o log_cffi --time=00:30:00 --nodes=1 exp.warm_bubble_cffi.run
```

## Analyze results

### Output

Output is stored in 

```bash
~icon-aes/experiments/EXP_NAME
```

Check the files `*atm_2d*` for vertically integrated variables, and `*atm_3d*` for full grid variables that are stores at every atmospheric *z*-level.

### Timer report

The timer report logs the time spent in individual submodules of ICON. For comparing the execution time of `SuperdropNet` vs. the two-moment bulk-moment scheme, check the line in the corresponding logfile:

```bash
grep phys_2mom_warmrain $logfile
```

## Create new experiments

To create a new experiment, copy the experiment files and adapt them. Always edit the experiment files, not the runscripts, as these will be overwritten whenever `./make_runscripts` is executed.