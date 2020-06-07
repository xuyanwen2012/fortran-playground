#!/bin/bash
#SBATCH -N 2
#SBATCH --ntasks 8
#SBATCH --job-name GameOfLife
#SBATCH --time 0:01:00
#SBATCH -o gof.out
#SBATCH -o gof.err

mpirun -np 8 ./life

