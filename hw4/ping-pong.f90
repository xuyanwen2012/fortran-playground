program main

    ! Yanwen Xu
    ! yxu83@ucsc.edu
    ! AM 250 
    ! Homework 4    

    use mpi

    implicit none
    
    integer :: ierr, myid, numprocs
    integer :: tag, other, count
    integer :: stat(MPI_STATUS_SIZE)
    integer :: i

    call MPI_INIT(ierr)
    call MPI_COMM_RANK(MPI_COMM_WORLD, myid, ierr)
    call MPI_COMM_SIZE(MPI_COMM_WORLD, numprocs, ierr)

    tag = 1234
    ! Set other to be the other thread
    other = modulo(myid + 1, 2)
    count = 1

    ! Ping Pong 10 times
    do i = 0, 10
        if (myid == modulo(i, 2)) then
            call MPI_SEND(i, count, MPI_INTEGER, other, tag, MPI_COMM_WORLD, ierr)
            print *, "On processor ", myid, ", Ping! ", i
        else 
            call MPI_RECV(i, count, MPI_INTEGER, other, tag, MPI_COMM_WORLD, stat, ierr) 
            print *, "On processor ", myid, ", Pong! ", i
        endif
    enddo

    call MPI_FINALIZE(ierr)

end program main

