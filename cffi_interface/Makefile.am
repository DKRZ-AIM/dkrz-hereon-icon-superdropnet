PYTHON=/sw/spack-levante/mambaforge-4.11.0-0-Linux-x86_64-sobz6z/bin/python

all: ../lib/libcffi.so

../lib/libcffi.so: builder.py cffi_module.py transfer_arrays.py solvers/moment_solver.py models/plModel.py
	${PYTHON} builder.py

clean:
	-rm ../lib/libcffi.so
	-rm plugin.h libcffi.o libcffi.c
	-rm -rf __pycache__
