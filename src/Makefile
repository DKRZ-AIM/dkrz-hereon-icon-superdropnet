# 2022-03 HEREON Python Fortran Bridges
# Makefile
# following https://aoterodelaroza.github.io/devnotes/modern-fortran-makefiles/
#

# module load intel
FC=ifort
FCFLAGS=''
TARGET_ARCH=''
COMPILE = $(FC) $(FCFLAGS) $(TARGET_ARCH) -c

EXEC_NAME=my_demo

#SOURCES=src/main.f90 src/fortran/mo_type.f90
SOURCES=main.f90 mo_type.f90 mo_function.f90
#OBJECTS=${SOURCES:.f90=.o}
OBJECTS=main.o mo_type.o mo_function.o

main: $(OBJECTS)
	$(FC) -o $(EXEC_NAME) $+

%.o %.mod %.smod: %.f90
	$(COMPILE) -o $*.o $<
	@touch $@

.PHONY: clean
clean:
	-rm -f *.o *.mod *.smod main
	#-rm -f src/fortran/*.o src/fortran/*.mod src/fortran/*.smod src/*.o main
