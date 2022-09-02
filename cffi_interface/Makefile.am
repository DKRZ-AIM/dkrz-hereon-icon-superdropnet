all: builder.py cffi_module.py transfer_arrays.py solvers/moment_solver.py models/plModel.py
	python builder.py

clean:
	-rm ../lib/libcffi.so
	-rm plugin.h libcffi.o libcffi.c
