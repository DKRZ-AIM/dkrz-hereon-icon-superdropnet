void ip_init_pipes(int* shm_rank, int* shm_size);
void ip_close_pipes();
void ip_config_pipes(double* emi_flux, double* emi_lat_n, double* emi_lat_s);
void ip_warm_rain_pipes_nn (int *dim_i, int *dim_k, int *n_moments, double current_moments[*][*][*], double new_moments[*][*][*], char *trained_model_path, int *pipes_return_state);
void ip_checksum_pipes (int* dim_i, int* dim_k, int* n_moments, double* current_moments, double* new_moments, int* pipes_return_state);
