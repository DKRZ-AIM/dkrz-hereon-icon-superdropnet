# Experiment description

All experiment files are located in `~git/icon_runscripts/`

## Usage

**Copy the files to the ~icon/run/ directory to run them**

In `~icon` execute:

```bash
./make_runscripts warm_bubble_cffi
cd run
sbatch -A ka1176 --partition=compute exp.warm_bubble_cffi.run
```

And likewise for the other experiments. Follow Tutorial.md for necessary edits of the runscripts and analysis.

## Description

### Warm bubble

Warm bubble scenario including the two-moment scheme for cloud microphysics. Outputs tracers for droplets and droplet concentrations of all six hydrometeors.

```bash
exp.warm_bubble_cffi    # Uses CFFI bridge for SuperdropNet
exp.warm_bubble_fortran # Uses Fortran bulk moment scheme
```

### Cold bubble

Cold bubble scenario including the two-moment scheme for cloud microphysics. Outputs tracers for droplets and droplet concentrations of all six hydrometeors.

```bash
exp.cold_bubble_cffi    # Uses CFFI bridge for SuperdropNet
exp.cold_bubble_fortran # Uses Fortran bulk moment scheme
```

### ICON R2B4

Since SuperdropNet requires a 20 second time step, it is currently not realistic to use it in a standard ICON grid.
