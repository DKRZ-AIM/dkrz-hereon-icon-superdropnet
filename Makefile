# 2022-03 HEREON Python Fortran Bridges
# Makefile
# 
# For building the libraries
#
# Caroline Arnold, DKRZ 2022

# module load intel intelmpi
CFFI_SRC_DIR=cffi_interface

BUILD_DIR=lib
CFFI_LIB=cffi_plugin.so

CFFI_SRC=${CFFI_SRC_DIR}/*.py

all:
	@$(MAKE) -C cffi_interface -f cffi_interface.mk
