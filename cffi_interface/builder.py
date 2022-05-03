import cffi

ffibuilder = cffi.FFI()

header = """
extern void i_hello_world();
extern void i_get_emi_number(int *);
extern void i_get_emi_float(double *);
extern void i_add_one(int *, double *);
extern void i_add_emi_echam_ttr(int *, int *, int *, int *, double *, double *, double *, double *, double *, double *);
"""

with open("plugin.h", "w") as f:
    f.write(header)

ffibuilder.embedding_api(header)
ffibuilder.set_source("my_plugin", r'''
    #include "plugin.h"
''')

module = open('my_module.py', 'r').read()

ffibuilder.embedding_init_code(module)
ffibuilder.compile(target="libplugin.so", verbose=True)
