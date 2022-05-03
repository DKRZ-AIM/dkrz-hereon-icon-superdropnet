# 2022-03 HEREON Python Fortran Bridges
# Makefile
# 
# For building the CFFI libraries
#
# Caroline Arnold, DKRZ 2022

BUILD_DIR=../lib
CFFI_LIB=cffi_plugin.so

CFFI_SRC= *.py

${BUILD_DIR}/${CFFI_LIB}: ${CFFI_SRC}
	python builder.py

.PHONY: clean
clean:
	-rm -f cffi_plugin.c libplugin.so plugin.h mo_pipes_c.so
	-rm -rf __pycache__
