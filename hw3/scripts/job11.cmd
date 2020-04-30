#!/bin/bash
#SBATCH -N 1
#SBATCH --ntasks 4
#SBATCH --job-name Job11
#SBATCH --time 0:01:00
#SBATCH -o job11.out
#SBATCH -e job11.err

export OMP_NUM_THREADS=4
./hello_omp_hb
