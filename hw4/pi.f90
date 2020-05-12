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
    integer :: tag, main_thread, count
    integer :: seed(12)
    real :: size
    real :: rnd(1)
    real :: x, y
    logical :: res

    call MPI_INIT(ierr)
    call MPI_COMM_RANK(MPI_COMM_WORLD, myid, ierr)
    call MPI_COMM_SIZE(MPI_COMM_WORLD, numprocs, ierr)

    tag = 1234
    main_thread = 0
    count = 1

    size = 2.0

    if (myid == main_thread) then
        ! In main thread we aggreate data from other threads

    else
        ! In other threads we do Monte Carlo simulation to estimate PI
        call RANDOM_SEED(put=seed)

        call RANDOM_NUMBER(rnd)
        x = rnd(1) * 2 - 1

        call RANDOM_NUMBER(rnd)
        y = rnd(1) * 2 - 1

        res = point_in_circle(x, y)

        print *, myid, ") ", res


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