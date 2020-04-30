#PBS -S /bin/tcsh
#PBS -q newest
#PBS -N job7
#PBS -l nodes=1:ppn=4
#PBS -l walltime=00:01:00

cd $PBS_O_WORKDIR
setenv OMP_NUM_THREAD 4
./hello_omp_grape
