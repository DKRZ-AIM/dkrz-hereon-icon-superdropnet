#!/bin/bash

nx1=$1
nx2=$2

# FORTRAN 1D case
echo "--- FORTRAN 1D"
rm _fort_1D_${nx1}.log
for count in `seq 1 10`; do
  grep 'Time per Fortran' logs_mpi_nodes-4_tasks-2/log_${nx1}_${nx2}_${count}.log  |head -n 1 |awk '{printf "%.4e\n", $7}' >> _fort_1D_${nx1}.log
done
awk '{ total += $1; count++ } END { printf "%.4e\n", total/count }' _fort_1D_${nx1}.log

# FORTRAN 2D case
echo "--- FORTRAN 2D"
rm _fort_2D_${nx1}_${nx2}.log
for count in `seq 1 10`; do
  grep 'Time per Fortran' logs_mpi_nodes-4_tasks-2/log_${nx1}_${nx2}_${count}.log  |tail -n 1 |awk '{printf "%.4e\n", $7}' >> _fort_2D_${nx1}_${nx2}.log
done
awk '{ total += $1; count++ } END { printf "%.4e\n", total/count }' _fort_2D_${nx1}_${nx2}.log

# CFFI 1D case
echo "--- CFFI 1D"
rm _cffi_1D_${nx1}.log
for count in `seq 1 10`; do
  grep 'Time per Py-cffi' logs_mpi_nodes-4_tasks-2/log_${nx1}_${nx2}_${count}.log  |head -n 1 |awk '{printf "%.4e\n", $7}' >> _cffi_1D_${nx1}.log
done
awk '{ total += $1; count++ } END { printf "%.4e\n", total/count }' _cffi_1D_${nx1}.log

# CFFI 2D case
echo "--- CFFI 2D"
rm _cffi_2D_${nx1}_${nx2}.log
for count in `seq 1 10`; do
  grep 'Time per Py-cffi' logs_mpi_nodes-4_tasks-2/log_${nx1}_${nx2}_${count}.log  |tail -n 1 |awk '{printf "%.4e\n", $7}' >> _cffi_2D_${nx1}_${nx2}.log
done
awk '{ total += $1; count++ } END { printf "%.4e\n", total/count }' _cffi_2D_${nx1}_${nx2}.log

# Pipes 1D case
echo "--- Pipes 1D"
rm _pipe_1D_${nx1}.log
for count in `seq 1 10`; do
  grep 'Time per Py-pipes' logs_mpi_nodes-4_tasks-2/log_${nx1}_${nx2}_${count}.log  |head -n 1 |awk '{printf "%.4e\n", $7}' >> _pipe_1D_${nx1}.log
done
awk '{ total += $1; count++ } END { printf "%.4e\n", total/count }' _pipe_1D_${nx1}.log

# Pipes 2D case
echo "--- Pipes 2D"
rm _pipe_2D_${nx1}_${nx2}.log
for count in `seq 1 10`; do
  grep 'Time per Py-pipes' logs_mpi_nodes-4_tasks-2/log_${nx1}_${nx2}_${count}.log  |tail -n 1 |awk '{printf "%.4e\n", $7}' >> _pipe_2D_${nx1}_${nx2}.log
done
awk '{ total += $1; count++ } END { printf "%.4e\n", total/count }' _pipe_2D_${nx1}_${nx2}.log
