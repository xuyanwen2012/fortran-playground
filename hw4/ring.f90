program main

    ! Yanwen Xu
    ! yxu83@ucsc.edu
    ! AM 250 
    ! Homework 4    

    use mpi

    implicit none
    
    integer :: ierr, myid, numprocs
    integer :: tag, other, count
    integer :: buffer
    integer :: stat(MPI_STATUS_SIZE)
    integer :: i

    call MPI_INIT(ierr)
    call MPI_COMM_RANK(MPI_COMM_WORLD, myid, ierr)
    call MPI_COMM_SIZE(MPI_COMM_WORLD, numprocs, ierr)

    tag = 1234
    count = 1

    
    if (myid /= 0) then
        call MPI_RECV(buffer, count, MPI_INTEGER, myid - 1, tag, MPI_COMM_WORLD, stat, ierr) 

        print *, "On processor ", myid, ", recieved ", buffer, " from ", myid -1
    else 
        ! The starting process should create a initial buffer value and 
        ! send it to the next one before recieving anything from the last one
        buffer = 123
    endif

    call MPI_SEND(buffer, count, MPI_INTEGER, modulo(myid + 1, numprocs), tag, MPI_COMM_WORLD, ierr)

    if (myid == 0) then
        call MPI_RECV(buffer, count, MPI_INTEGER, numprocs - 1, tag, MPI_COMM_WORLD, stat, ierr) 
        
        print *, "On processor ", myid, ", recieved ", buffer, " from ", numprocs -1
    endif

    call MPI_FINALIZE(ierr)

end program main

