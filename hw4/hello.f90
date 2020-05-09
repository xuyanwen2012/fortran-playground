program main

    ! Yanwen Xu
    ! yxu83@ucsc.edu
    ! AM 250 
    ! Homework 4    
    
    use mpi

    implicit none
    
    integer ierr, myid, numprocs

    call MPI_INIT(ierr)
    call MPI_COMM_RANK(MPI_COMM_WORLD, myid, ierr)
    call MPI_COMM_SIZE(MPI_COMM_WORLD, numprocs, ierr)

    print *, "Hello world from processor ", myid, " out of ", numprocs

    call MPI_FINALIZE(ierr)

end program main
