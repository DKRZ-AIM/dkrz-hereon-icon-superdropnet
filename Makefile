# 2022-03 HEREON Python Fortran Bridges
# Makefile
# following https://aoterodelaroza.github.io/devnotes/modern-fortran-makefiles/
#

# module load intel
FC=ifort
FCFLAGS=''
TARGET_ARCH=''
COMPILE = $(FC) $(FCFLAGS) $(TARGET_ARCH) -c

SOURCES=main.f90 mo_type.f90
OBJECTS=${SOURCES:.f90=.o}

%.o %.mod %.smod: %.f90
	$(COMPILE) -o $*.o $<
	@touch $@


main: $(OBJECTS)
	$(FC) -o $@ $+

.PHONY: clean
clean:
	-rm -f *.o *.mod *.smod main
