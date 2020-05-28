program main

    ! Yanwen Xu
    ! yxu83@ucsc.edu
    ! AM 250 
    ! Homework 5    

    use mpi

    implicit none
    
    integer :: myid, numprocs
    integer :: other, count
    integer, dimension(10000) :: send_buff, recv_buff
    integer :: itag, istatus(MPI_STATUS_SIZE), ierr
    
    double precision :: t0, t1, sum
    integer :: i

    call MPI_INIT(ierr)
    call MPI_COMM_RANK(MPI_COMM_WORLD, myid, ierr)
    call MPI_COMM_SIZE(MPI_COMM_WORLD, numprocs, ierr)

    itag = 1234
    other = modulo(myid + 1, numprocs)

    count = 10000
    send_buff = myid + 10
    recv_buff = 0
    sum = 0

    ! Main loop & timer

    t0 = MPI_WTIME()
    do i = 1 , 10000
        call MPI_Sendrecv( & 
            send_buff, count, MPI_INTEGER, other, itag, &
            recv_buff, count, MPI_INTEGER, other, itag, &
            MPI_COMM_WORLD, istatus, ierr)
    enddo
    t1 = MPI_WTIME()

    if (myid == 0) then
        print *, "t1 - t0 = ", t1 - t0
    endif

    call MPI_FINALIZE(ierr)

end program main

