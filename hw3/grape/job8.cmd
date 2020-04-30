#PBS -S /bin/tcsh
#PBS -q newest
#PBS -N job8
#PBS -l nodes=1:ppn=8
#PBS -l walltime=00:01:00

cd $PBS_O_WORKDIR
setenv OMP_NUM_THREAD 8
./hello_omp_grape
