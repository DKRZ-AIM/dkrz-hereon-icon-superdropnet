void ip_init_pipes(int* mpi_rank);
void ip_close_pipes();
void ip_scalar_field_1d(int* nx, float* x, float* phi);
void ip_scalar_field_2d(int* nx1, int* nx2, float* x1, float* x2, float* phi);