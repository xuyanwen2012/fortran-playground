program main

    ! ---------------------------------------------------------------------
    ! Yanwen Xu
    ! yxu83@ucsc.edu
    ! AM 250 
    ! Final Project - Game of Life  
    ! ---------------------------------------------------------------------

    use mpi

    implicit none
    
    ! ---------------------------------------------------------------------
    ! MPI related parameters
    ! ---------------------------------------------------------------------

    integer, parameter :: root_rank = 0
    integer :: ierr, my_rank, num_procs
    integer :: itag, irequest
    integer :: istat(MPI_STATUS_SIZE)

    ! ---------------------------------------------------------------------
    ! Game-of-Life(GoF) related parameters
    ! ---------------------------------------------------------------------
    integer, parameter :: global_height = 4
    integer, parameter :: global_width = 16
    integer, dimension(global_height, global_width) :: global_cells

    ! ---------------------------------------------------------------------
    ! Parellel GoF related parameters
    ! ---------------------------------------------------------------------

    ! [height], [width] is the actual board size of this **particular thread**
    ! TODO: Should be dynamic????
    integer, parameter :: height = 4
    integer, parameter :: width = 4

    ! [recv_buffer] is the raw 1D buffer recieved from MPI_SCATTER
    ! which is intented to be reshaped to 2D.
    !
    ! [recv_cells] should be the actual 2D board (w*h) and it is **LOCAL**
    ! which should be passed in from the main thread.
    ! 
    ! [aug_cells] augmented cells, which should have 2 more lines of information
    ! than the [recv_cells]. But when we do the simulation, we should only 
    ! work on the area from (2, 2) to (w+1, h+1)
    integer, dimension(height, width) :: recv_buffer
    integer, dimension(height, width) :: recv_cells
    integer, dimension(height + 2, width + 2) :: aug_cells

    ! [rev_left] and [rev_right] are strip (1*h), i.e. The local Ghost buffer
    ! which should be recieved from tge left, the right neighbor, respectively
    ! 
    ! [loc_left] and [loc_right] are out sending local buffer,  
    ! which are essentially: cells(1, :) and cell(:, width)
    integer, dimension(height) :: rev_left, rev_right 
    integer, dimension(height) :: loc_left, loc_right 

    ! For MPI use only: the process id of left and right neighbor
    integer :: left_procs, right_procs

    integer :: num_cell_per_task

    integer :: i, j, k
    ! integer :: num_live_neighbors

    ! ---------------------------------------------------------------------
    ! Code: Start MPI
    ! ---------------------------------------------------------------------

    call MPI_INIT(ierr)
    call MPI_COMM_RANK(MPI_COMM_WORLD, my_rank, ierr)
    call MPI_COMM_SIZE(MPI_COMM_WORLD, num_procs, ierr)

    ! Temporary check, subject to remove
    if (modulo(global_width, num_procs) .ne. 0) then
        print *, ("width and height of the world can not divid by number of processors!")
        call exit(0)
    endif

    ! MPI related: Compute the neighbor ranks
    left_procs = modulo(my_rank - 1, num_procs)
    right_procs = modulo(my_rank + 1, num_procs)

    ! Debug Print, remove this when finished
    ! print *, &
    ! "At rank ", my_rank , &
    ! "left_procs", left_procs, &
    ! "right_procs", right_procs, &
    ! ", sub-width = ", (global_width / num_procs)

    ! Initialize main cell 
    if (my_rank .eq. root_rank) then

    endif

    ! Initialize World  
    ! Example: [A] should look like this (4x4)
    !
    ! 1  5  9 13
    ! 2  6 10 14
    ! 3  7 11 15
    ! 4  8 12 16
    global_cells = reshape((/ (i, i = 1,  global_height * global_width) /), (/global_height, global_width/))

    ! Scatter and distribute the board to processes
    num_cell_per_task = (global_height * global_width) / num_procs

    call MPI_SCATTER(global_cells, num_cell_per_task, MPI_INTEGER, &
                     recv_buffer, num_cell_per_task, MPI_INTEGER, &
                     root_rank, MPI_COMM_WORLD, ierr)

    ! Convert recieved 1D raw buffer into 2D local cells
    recv_cells = reshape(recv_buffer, (/height, width/))

    if (my_rank .eq. 0) then
        print *, 'num_cell_per_task: ', num_cell_per_task
        print *, 'height: ', height
        print *, 'width: ', width
        ! Print the board
        do i = 1, height
            do j = 1, width
                write(*, '(I3)', advance='no') recv_cells(i, j)
            enddo
            print *, ''
        enddo
        print *, ''
    endif

    loc_left = recv_cells(:, 1)
    loc_right = recv_cells(:, width)

    call MPI_BARRIER(MPI_COMM_WORLD, ierr)

    ! call MPI_Sendrecv( & 
    !     loc_left, height, MPI_LOGICAL, left_procs, itag, &
    !     rev_left, height, MPI_LOGICAL, left_procs, itag, &
    !     MPI_COMM_WORLD, istat, ierr)

    ! call MPI_Sendrecv( & 
    !     loc_right, height, MPI_INTEGER, right_procs, itag, &
    !     rev_right, height, MPI_INTEGER, right_procs, itag, &
    !     MPI_COMM_WORLD, istat, ierr)

    call MPI_ISEND(loc_left, height, MPI_INTEGER, left_procs, itag, MPI_COMM_WORLD, irequest, ierr)
    call MPI_ISEND(loc_right, height, MPI_INTEGER, right_procs, itag, MPI_COMM_WORLD, irequest, ierr)
    call MPI_RECV(rev_left, height, MPI_INTEGER, left_procs, itag, MPI_COMM_WORLD, istat, ierr)
    call MPI_RECV(rev_right, height, MPI_INTEGER, right_procs, itag, MPI_COMM_WORLD, istat, ierr)

    ! Prepare the augmented cells

    ! Copy [A] to [D]
    aug_cells(2 : height + 1, 2 : width + 1) = recv_cells

    ! At this point, we should have all we need per thread
    ! Example: [D] should look like this (6x6)
    ! 
    !  0  0  0  0  0  0
    !  0  1  5  9 13  0
    !  0  2  6 10 14  0
    !  0  3  7 11 15  0
    !  0  4  8 12 16  0
    !  0  0  0  0  0  0

    ! Copy [B], [C] to [D]
    aug_cells(2 : height + 1, 1) = rev_left
    aug_cells(2 : height + 1, width + 2) = rev_right

    ! At this point, we should have all we need per thread
    ! Example: [D] should look like this (6x6)
    ! 
    !  0  0  0  0  0  0
    ! 21  1  5  9 13 31
    ! 22  2  6 10 14 32
    ! 23  3  7 11 15 33
    ! 24  4  8 12 16 34
    !  0  0  0  0  0  0

    ! Copy [d]'s top and bottom row
    aug_cells(1, :) = aug_cells(height + 1, :)
    aug_cells(height + 2, :) = aug_cells(2, :)

    ! At this point, we should have all we need per thread
    ! Example: [D] should look like this (6x6)
    ! 
    ! 24  4  8 12 16 34
    ! 21  1  5  9 13 31
    ! 22  2  6 10 14 32
    ! 23  3  7 11 15 33
    ! 24  4  8 12 16 34
    ! 21  1  5  9 13 31

    if (my_rank .eq. root_rank) then
        ! Print the board
        print *, ''
        do i = 1, height + 2
            do j = 1, width + 2
                write(*, '(I3)', advance='no') aug_cells(i, j)
            enddo
            print *, ''
        enddo
        print *, ''
    endif

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

