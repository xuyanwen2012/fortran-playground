program main

    ! Yanwen Xu
    ! yxu83@ucsc.edu
    ! AM 250 
    ! Final Project - Game of Life  

    use mpi

    implicit none
    
    ! MPI related parameters

    integer :: ierr, myid, num_procs
    integer :: istat(MPI_STATUS_SIZE)
    integer :: tag

    ! Game-of-Life related parameters

    integer, parameter :: width = 4
    integer, parameter :: height = 4
    logical, dimension(width, height) :: cells = .false.
    logical, dimension(width, height) :: buffer

    integer :: i, j, k
    integer :: num_live_neighbors

    ! Start MPI
    call MPI_INIT(ierr)
    call MPI_COMM_RANK(MPI_COMM_WORLD, myid, ierr)
    call MPI_COMM_SIZE(MPI_COMM_WORLD, num_procs, ierr)

    ! temp initializing the universe

    cells(2, 1:3) = .true.

    ! temp print the universe
    do i= 1, width
        do j= 1, height
            if (cells(i, j)) then
                write(*, '(A)', advance='no') "X"
            else
                write(*, '(A)', advance='no') "O"
            endif
        enddo
        print *, ''
    enddo

    do k = 1, 10
        ! Clone the state of the universe, do the update on the buffer. 
        buffer = cells

        ! Tick the universe, 
        do i= 1, width
            do j= 1, height

                ! Count number of live neighbors at cell (i, j)
                if (cells(i, j)) then
                    num_live_neighbors = sum(count(cells(i-1:i+1, j-1:j+1), 1)) - 1
                else
                    num_live_neighbors = sum(count(cells(i-1:i+1, j-1:j+1), 1))
                endif

                ! Perform GoF calculation, update the buffer
                select case (num_live_neighbors)
                    case (3)
                        buffer(i, j) = .true.
                    case (2)
                        ! Do nothing
                    case default
                        buffer(i, j) = .false.
                endselect

            enddo
        enddo

        ! Update cells
        cells = buffer

        ! temp print the universe
        do i= 1, width
            do j= 1, height
                if (cells(i, j)) then
                    write(*, '(A)', advance='no') "X"
                else
                    write(*, '(A)', advance='no') "O"
                endif
            enddo
            print *, ''
        enddo
        print *, ''
        
    enddo

    ! End MPI
    call MPI_FINALIZE(ierr)

end program main

