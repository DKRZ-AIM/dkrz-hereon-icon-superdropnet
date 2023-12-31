import cffi

ffibuilder = cffi.FFI()

header = """
extern void i_hello_world();
extern void i_check_interface(int *);
extern void i_get_emi_number(int *);
extern void i_get_emi_float(double *);
extern void i_add_one(int *, double *);
extern void i_add_emi_echam_ttr(int *, int *, int *, int *, double *, double *, double *, double *, double *, double *);
extern void i_load_pretrained_model(int *, char *);
extern void i_retrieve_model_from_handle(int *);
extern void i_warm_rain_nn(int *, int *, int *, double *, double *, char *, double *, double *, int *);
extern void i_checksum(int *, int *, int *, double *, double *, int *);
"""

with open("plugin.h", "w") as f:
    f.write(header)

ffibuilder.embedding_api(header)
ffibuilder.set_source("libcffi", r'''
    #include "plugin.h"
''')

module = open('cffi_module.py', 'r').read()

ffibuilder.embedding_init_code(module)
ffibuilder.compile(target="../lib/libcffi.so", verbose=True)
