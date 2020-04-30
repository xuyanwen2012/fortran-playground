#!/bin/bash
#SBATCH -N 2
#SBATCH --ntasks 8
#SBATCH --job-name Job6
#SBATCH --time 0:01:00
#SBATCH -o job6.out
#SBATCH -e job6.err

mpirun -np 8 ./hello_mpi_hb
