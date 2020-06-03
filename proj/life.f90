program main

    ! Yanwen Xu
    ! yxu83@ucsc.edu
    ! AM 250 
    ! Final Project - Game of Life  

    use mpi

    implicit none
    
    ! MPI related parameters

    integer :: ierr, my_rank, num_procs
    integer :: istat(MPI_STATUS_SIZE)
    integer :: tag

    ! Game-of-Life related parameters

    integer, parameter :: width = 4
    integer, parameter :: height = 4
    logical, dimension(width, height) :: cells = .false.
    logical, dimension(width, height) :: buffer

    integer :: i, j, k
    integer :: num_live_neighbors

    ! Parellel GoF related parameters
    integer :: left_procs, right_procs ! the process id of left and right  
    integer :: start_col, end_col ! starting/ending column index
    logical, dimension(height) :: loc_left, loc_right ! Local Ghost buffer (Sending out)
    logical, dimension(height) :: rev_left, rev_right ! Local Ghost buffer (Recieved)
    integer :: send_count


    ! Start MPI
    call MPI_INIT(ierr)
    call MPI_COMM_RANK(MPI_COMM_WORLD, my_rank, ierr)
    call MPI_COMM_SIZE(MPI_COMM_WORLD, num_procs, ierr)

    ! Initialize a universe, 
    ! 
    cells(2, 1:3) = .true.


    ! Temporary check, subject change later
    if (modulo(width, num_procs) .ne. 0) then
        print *, ("width and height of the world can not divid by number of processors!")
        call exit(0)
    endif

    ! Compute
    start_col = (width / num_procs) * my_rank + 1
    end_col = start_col + (width / num_procs) - 1

    ! Determine the neighbor ranks 
    left_procs = modulo(my_rank - 1, num_procs)
    right_procs = modulo(my_rank + 1, num_procs)

    ! Debug Print, ignore
    print *, &
    "At rank ", my_rank , &
    ! "left_procs", left_procs, &
    ! "right_procs", right_procs, &
    ! ", sub-width = ", (width / num_procs), &
    ", from column ", start_col, &
    " to ", end_col

    loc_left = cells(start_col, :)
    loc_right = cells(end_col, :)

    call MPI_BARRIER(MPI_COMM_WORLD, ierr)

    call MPI_SEND(loc_left, height, MPI_LOGICAL, left_procs, tag, MPI_COMM_WORLD, ierr)
    call MPI_SEND(loc_right, height, MPI_LOGICAL, right_procs, tag, MPI_COMM_WORLD, ierr)

    call MPI_RECV(rev_left, height, MPI_LOGICAL, left_procs, tag, MPI_COMM_WORLD, istat, ierr)
    call MPI_RECV(rev_right, height, MPI_LOGICAL, right_procs, tag, MPI_COMM_WORLD, istat, ierr)





    ! temp print the universe
    do i= 1, width
        do j= 1, height
            if (cells(i, j)) then
                write(*, '(A)', advance='no') "X"
            else
                write(*, '(A)', advance='no') "O"
            end if
        end do
        print *, ''
    end do
    print *, ''

    ! Assuming we are on 4 x 4
    !   | 1 2 3 4 (i)
    ! _ | -------
    ! 1 | 
    ! 2 | 
    ! 3 | 
    ! 4 | 
    ! (j)


    ! do k = 1, 10
    !     ! Clone the state of the universe, do the update on the buffer. 
    !     buffer = cells

    !     ! Tick the universe, note Fortran is column major 
    !     do j= 1, height
    !         do i= 1, width

    !             ! Count number of live neighbors at cell (i, j)
    !             if (cells(i, j)) then
    !                 num_live_neighbors = sum(count(cells(i-1:i+1, j-1:j+1), 1)) - 1
    !             else
    !                 num_live_neighbors = sum(count(cells(i-1:i+1, j-1:j+1), 1))
    !             end if

    !             ! Perform GoF calculation, update the buffer
    !             select case (num_live_neighbors)
    !                 case (3)
    !                     buffer(i, j) = .true.
    !                 case (2)
    !                     ! Do nothing
    !                 case default
    !                     buffer(i, j) = .false.
    !             end select

    !         end do
    !     end do

    !     ! Update cells
    !     cells = buffer

    !     ! temp print the universe
    !     do i= 1, width
    !         do j= 1, height
    !             if (cells(i, j)) then
    !                 write(*, '(A)', advance='no') "X"
    !             else
    !                 write(*, '(A)', advance='no') "O"
    !             end if
    !         end do
    !         print *, ''
    !     end do
    !     print *, ''
        
    ! end do

    ! End MPI
    call MPI_FINALIZE(ierr)

end program main

