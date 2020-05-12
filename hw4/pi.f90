program main

    ! Yanwen Xu
    ! yxu83@ucsc.edu
    ! AM 250 
    ! Homework 4    

    use mpi

    implicit none
    
    interface
        logical function point_in_circle(x, y) result(is_in)
            implicit none
            real, intent(in) :: x, y
            logical :: is_in
        end function point_in_circle
    end interface

    integer :: ierr, myid, numprocs
    integer :: stat(MPI_STATUS_SIZE)
    integer :: tag, main_thread, count
    integer :: seed(12)
    integer :: i, simulation_per_thread, num_in_dart, sum_in_dart
    real :: size
    real :: rnd(1)
    real :: x, y

    call MPI_INIT(ierr)
    call MPI_COMM_RANK(MPI_COMM_WORLD, myid, ierr)
    call MPI_COMM_SIZE(MPI_COMM_WORLD, numprocs, ierr)

    tag = 1234
    main_thread = 0
    count = 1
    simulation_per_thread = 10000000
    num_in_dart = 0
    sum_in_dart = 0

    size = 2.0

    if (myid == main_thread) then
        ! In main thread we aggreate data from other threads

        do i = 0, numprocs - 1
            if (i /= myid) then
                call MPI_RECV(num_in_dart, count, MPI_INTEGER, i, tag, MPI_COMM_WORLD, stat, ierr)
                sum_in_dart = sum_in_dart + num_in_dart
                print *, i, ") simulated ", num_in_dart, "/", simulation_per_thread, " darts in circle."
            endif
        enddo

        ! Now we can calculate PI
        print *, "PI = ", (real(sum_in_dart, 8) / (simulation_per_thread * (numprocs - 1))) * 4

    else
        ! In other threads we do Monte Carlo simulation to estimate PI
        call RANDOM_SEED(put=seed)


        do i = 1, simulation_per_thread
            call RANDOM_NUMBER(rnd)
            x = rnd(1) * 2 - 1

            call RANDOM_NUMBER(rnd)
            y = rnd(1) * 2 - 1

            if (point_in_circle(x, y)) then
                num_in_dart = num_in_dart + 1
            endif
            
        enddo

        call MPI_SEND(num_in_dart, count, MPI_INTEGER, main_thread, tag, MPI_COMM_WORLD, ierr)

    endif    

    call MPI_FINALIZE(ierr)

end program main

pure logical function point_in_circle(x, y) result(is_in)
    ! Return if the dart at point (x, y) is locatted in the unit circle.
    ! Assume x and y are from -1.0 to 1.0
    real, intent(in) :: x, y
    logical :: is_in

    if (sqrt(x ** 2 + y ** 2) < 1.0) then
        is_in = .true.
    else 
        is_in = .false.
    endif

end function point_in_circle