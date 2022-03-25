# Benchmarks for Stage 1

Stage 1 refers to the first stage of the demo, where Fortran and Python communicate while Fortran is running in a single thread. The benchmark is executed using the following:

```bash
srun --pty -A ka1176 --partition=compute --exclusive --nodes=1 --tasks-per-node=1 --time=00:10:00 ./my_demo <nx1> <nx2>
```

## Mistral

Architecture: Haswell (partition compute)

### 1D scalar field

|  nx     | Fortran |  CFFI  | Pipes | MPI (same node)  | MPI (cross node)
|------   |---------|--------|-------|------------------|------------------
| 10      |  --     |   $1.67 \times 10^{-4}$ ||||
| 100     ||||||
| 1000    ||||||
| 10000   ||||||
| 100000  ||||||
| 1000000 ||||||

### 2D scalar field

|  nx1    |  nx2    | Fortran |  CFFI  | Pipes | MPI (same node) | MPI (cross node)
|------   |---------|---------|--------|-------|-----------------|------------------
| 10      | 10      ||||||
| 100     | 100     ||||||
| 1000    | 1000    ||||||
| 10000   | 10000   ||||||


## Trial

Architecture: AMD EPYC 7742

### 1D scalar field

|  nx     | Fortran |  CFFI  | Pipes | MPI (same node)  | MPI (cross node)
|------   |---------|--------|-------|------------------|------------------
| 10      ||||||
| 100     ||||||
| 1000    ||||||
| 10000   ||||||
| 100000  ||||||
| 1000000 ||||||

### 2D scalar field

|  nx1    |  nx2    | Fortran |  CFFI  | Pipes | MPI (same node) | MPI (cross node)
|------   |---------|---------|--------|-------|-----------------|------------------
| 10      | 10      ||||||
| 100     | 100     ||||||
| 1000    | 1000    ||||||
| 10000   | 10000   ||||||

### 2D scalar field

|  nx     | Fortran |  CFFI  | Pipes | MPI (same node)  | MPI (cross node)
|------   |---------|--------|-------|------------------|------------------
| 10      ||||||
| 100     ||||||
| 1000    ||||||
| 10000   ||||||
| 100000  ||||||
| 1000000 ||||||

### 2D scalar field

|  nx1    |  nx2    | Fortran |  CFFI  | Pipes | MPI (same node) | MPI (cross node)
|------   |---------|---------|--------|-------|-----------------|------------------
| 10      | 10      ||||||
| 100     | 100     ||||||
| 1000    | 1000    ||||||
| 10000   | 10000   ||||||



## Levante

Architecture: AMD 7763

### 1D scalar field

|  nx     | Fortran |  CFFI  | Pipes | MPI (same node)  | MPI (cross node)
|------   |---------|--------|-------|------------------|------------------
| 10      ||||||
| 100     ||||||
| 1000    ||||||
| 10000   ||||||
| 100000  ||||||
| 1000000 ||||||

### 2D scalar field

|  nx     | Fortran |  CFFI  | Pipes | MPI (same node)  | MPI (cross node)
|------   |---------|--------|-------|------------------|------------------
| 10      ||||||
| 100     ||||||
| 1000    ||||||
| 10000   ||||||
| 100000  ||||||
| 1000000 ||||||

### 2D scalar field

|  nx1    |  nx2    | Fortran |  CFFI  | Pipes | MPI (same node) | MPI (cross node)
|------   |---------|---------|--------|-------|-----------------|------------------
| 10      | 10      ||||||
| 100     | 100     ||||||
| 1000    | 1000    ||||||
| 10000   | 10000   ||||||
