#include <stdio.h>
#include <string.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>
#include <stdlib.h>
//#include <mpi.h>
#include <errno.h>
#include <stdbool.h>

static const char PIPE_OUT_PATH_PREFIX[] = "/tmp/esmdemopipe_out";
static const char PIPE_IN_PATH_PREFIX[] = "/tmp/esmdemopipe_in"; 

int pipe_in, pipe_out;
char pipe_in_fn[200], pipe_out_fn[200];
char cmd[200];
bool send_terminate = false;

void ip_init_pipes(int* shm_rank, int* shm_size) {
	// determine SLURM_JOB_ID
	char* job_id;
    job_id = getenv("SLURM_JOB_ID");
	if (job_id == NULL) {
		printf("Error: Could not determine SLURM_JOB_ID!");
		return;
	}
	printf("Pipes - MPI shm rank: %i, size: %i\n", *shm_rank, *shm_size);
	if (*shm_rank == 0) {
		printf("This rank will send the stop command later.\n");
		send_terminate = true;
		printf("This rank also launches the worker. Launching now...\n");
		snprintf(cmd, 200, "python ${basedir}/externals/mlbridges/pipes_interface/warm_rain_pipe_worker.py -s %s -n %i --create-pipes --remove-pipes&", job_id, *shm_size);
		printf("worker launch command: %s\n", cmd);
		system(cmd); 
		sleep(2);
	}
	// create file (fifo) names from SLURM JOB ID
	snprintf(pipe_in_fn, 200, "%s_%s_%i.tmp", PIPE_IN_PATH_PREFIX, job_id, *shm_rank);
	snprintf(pipe_out_fn, 200, "%s_%s_%i.tmp", PIPE_OUT_PATH_PREFIX, job_id, *shm_rank);
	printf("Opening pipes %i in ICON...\n", *shm_rank);
	printf("pipe_out file name: %s\n", pipe_out_fn);
	printf("pipe_in file name: %s\n", pipe_in_fn);
	bool pipe_exists = false;
	// if the pipe does not exist yet, the pipe worker is not ready yet; 
	// so loop until the pipe can be opened.
	while (pipe_exists == false) {
		pipe_out = open(pipe_out_fn, O_WRONLY);
		if (pipe_out == -1) {
			// errno 2 means file does not exist
			if (errno != 2) {
				printf("Pipe_out open failed for pipes %i with errno %i\n", *shm_rank, errno);
			}
			// printf("Pipe %i not ready yet - waiting...\n", *shm_rank);
			sleep(1);
		}
		else {
			pipe_exists = true;
		}
	}
	// also open the in pipe; assumes that if out pipe is ready, in pipe is ready as well
	// as they are created in that order by the pipe worker
	pipe_in = open(pipe_in_fn, O_RDONLY);
	if (pipe_in == -1) {
		printf("Pipe_in open failed for pipes %i with errno %i\n", *shm_rank, errno);
	}
	printf("Pipes %i opened in ICON. pipe_out: %i, pipe_in: %i\n", *shm_rank, pipe_out, pipe_in);
	
	
	printf("Sending ping through pipe %i\n", *shm_rank);
	// send a ping back and forth to test communication
	int s_command = 10;
	int recvd = 0;
	int n = 0;
	write(pipe_out, &s_command, 4);
	printf("Pipe %i sent ping, now waiting...\n", *shm_rank);
	n = read(pipe_in, &recvd, 4);
	printf("Pipe %i received ping of %i bytes, value: %i\n", *shm_rank, n, recvd);
	if (recvd != 42) {
		printf("ERROR: Ping return value wrong for pipe %i!", *shm_rank);
	}
}

void ip_close_pipes() {
	int n = 0;
	if (send_terminate) {
		printf("Sending pipe worker stop command to pipe: %s\n", pipe_out_fn);
		write(pipe_out, &n, 4); // stop worker command
	}
	close(pipe_out);
	close(pipe_in);
	printf("Pipes closed in ICON.\n");
	//unlink(pipe_out_fn);
	//unlink(pipe_in_fn);
	//printf("Pipes removed by ICON.\n");
}

/*
void ip_config_pipes(double* emi_flux, double* emi_lat_n, double* emi_lat_s) {
	int n = 2;
	write(pipe_out, &n, 4);  // case indicator
	write(pipe_out, emi_flux, 8);
	write(pipe_out, emi_lat_n, 8);
	write(pipe_out, emi_lat_s, 8);
}
*/

void ip_warm_rain_pipes_nn (int *dim_i, int *dim_k, int *n_moments, double *current_moments, double *new_moments, char *trained_model_path, int *pipes_return_state) {
	// current moments and new moments:
	// new_moments(dim_i, dim_k, dim_m)
	int n = 1;
	
	write(pipe_out, &n, 4);  // case indicator
	write(pipe_out, dim_i, 4); 
	write(pipe_out, dim_k, 4);
	write(pipe_out, n_moments, 4);
	write(pipe_out, current_moments, *dim_i**dim_k**n_moments*8);
	
	// typical values: n_moments is 4; dim_k is 1; dim_i is 7 or 8

	// write out current_moments according to lecture; avoids possible F->C memory fragmentation issues
	
	// new_moments and return_state will be returned
	n = read(pipe_in, new_moments, *dim_i**dim_k**n_moments*8);
	n = read(pipe_in, pipes_return_state, 4);
}

/*
void ip_add_emi_echam_ttr_pipes(int* jg, int* jcs, int* jce, int* kbdim, double* air_mass, double* clat, double* dxdt) {
	int n = 1;
	write(pipe_out, &n, 4);  // case indicator
	write(pipe_out, jg, 4); 
	write(pipe_out, jcs, 4);
	write(pipe_out, jce, 4);
	write(pipe_out, kbdim, 4);
	write(pipe_out, air_mass, *kbdim*8);
	write(pipe_out, clat, *kbdim*8);
	// dxdt(kbdim) will be returned
	n = read(pipe_in, dxdt, *kbdim*8);
}
*/

void ip_checksum_pipes (int* dim_i, int* dim_k, int* n_moments, double* current_moments, double* new_moments, int* pipes_return_state) {
  // TODO: implement
}

