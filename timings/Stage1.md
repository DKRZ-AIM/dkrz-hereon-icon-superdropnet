# Benchmarks for Stage 1

Stage 1 refers to the first stage of the demo, where Fortran and Python communicate while Fortran is running in a single thread. The benchmark is executed using the following:

```bash
srun --pty -A ka1176 --partition=compute --time=00:10:00 ./my_demo
```

## Mistral

### 1D scalar field

|  nx     | Fortran |  CFFI  | Pipes | MPI  |
|------   |---------|--------|-------|------|
| 10      |||||
| 100     |||||
| 1000    |||||
| 10000   |||||
| 100000  |||||
| 1000000 |||||
