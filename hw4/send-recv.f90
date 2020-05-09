! Yanwen Xu
! yxu83@ucsc.edu
! AM 250 
! Homework 4

program main
    use mpi
    implicit none
    
    integer ierr, myid, numprocs
    integer tag, source, destination, count
    integer buffer
    integer stat(MPI_STATUS_SIZE)

    call MPI_INIT(ierr)
    call MPI_COMM_RANK(MPI_COMM_WORLD, myid, ierr)
    call MPI_COMM_SIZE(MPI_COMM_WORLD, numprocs, ierr)

    tag = 1234
    source = 0
    destination = 1
    count = 1

    if (myid .eq. source) then
        buffer = 5678
        print *, "On processor ", myid, ", before send, my buffer = ", buffer
        call MPI_SEND(buffer, count, MPI_INTEGER, destination, tag, MPI_COMM_WORLD, ierr)
        print *, "On processor ", myid, ", after send, my buffer = ", buffer
    endif

    if (myid .eq. destination) then
        print *, "On processor ", myid, ", before recv, my buffer = ", buffer
        call MPI_RECV(buffer, count, MPI_INTEGER, source, tag, MPI_COMM_WORLD, stat, ierr)
        print *, "On processor ", myid, ", after recv, my buffer = ", buffer
    endif

    call MPI_FINALIZE(ierr)

end program main

