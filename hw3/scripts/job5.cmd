#!/bin/bash
#SBATCH -N 1
#SBATCH --ntasks 4
#SBATCH --job-name Job5
#SBATCH --time 0:01:00
#SBATCH -o job5.out
#SBATCH -e job5.err

mpirun -np 4 ./hello_mpi_hb
