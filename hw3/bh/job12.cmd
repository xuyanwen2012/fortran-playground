#!/bin/bash
#SBATCH -N 1
#SBATCH --ntasks 8
#SBATCH --job-name Job12
#SBATCH --time 0:01:00
#SBATCH -o job12.out
#SBATCH -e job12.err

export OMP_NUM_THREADS=8
./hello_omp_hb
