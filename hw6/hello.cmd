#PBS -S /bin/tcsh
#PBS -q newest
#PBS -N hello_world
#PBS -l nodes=1:ppn=4
#PBS -l walltime=00:01:00

cd $PBS_O_WORKDIR
setenv OMP_NUM_THREADS 4
./hello
