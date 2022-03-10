# 2022-03 HEREON Python Fortran Bridges
# Makefile
# following https://aoterodelaroza.github.io/devnotes/modern-fortran-makefiles/
#


# module load intel intelmpi
FC=mpiifort

# -g debug option
FCFLAGS=-L./ -lplugin -g
TARGET_ARCH=''
COMPILE = $(FC) $(FCFLAGS) $(TARGET_ARCH) -c

EXEC_NAME=my_demo
EXEC_NAME_MPI=my_demo_mpi

#SOURCES=src/main.f90 src/fortran/mo_type.f90
SOURCES=mo_type.f90 mo_function.f90 main.f90
#OBJECTS=${SOURCES:.f90=.o}
OBJECTS=mo_type.o mo_function.o libplugin.so main.o
OBJECTS_MPI=mo_type.o mo_function.o libplugin.so main_mpi.o

my_demo: $(OBJECTS)
	$(FC) -o $(EXEC_NAME) $(FCFLAGS) $+

my_demo_mpi: $(OBJECTS_MPI)
	$(FC) -o $(EXEC_NAME_MPI) $(FCFLAGS) $+

%.o %.mod %.smod: %.f90
	$(COMPILE) -o $*.o $<
	@touch $@

libplugin.so: builder.py transfer_arrays.py my_module.py
	python builder.py

.PHONY: clean
clean:
	-rm -f *.o *.mod *.smod my_demo my_demo_mpi
	-rm -f my_plugin.c libplugin.so plugin.h
	-rm -rf __pycache__
	#-rm -f src/fortran/*.o src/fortran/*.mod src/fortran/*.smod src/*.o main
