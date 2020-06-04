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
    integer :: itag, irequest

    ! Game-of-Life related parameters

    integer, parameter :: width = 4
    integer, parameter :: height = 4
    integer, dimension(width, height) :: cells = 0
    integer, dimension(width, height) :: buffer

    integer :: i, j, k
    integer :: num_live_neighbors

    ! Parellel GoF related parameters
    integer :: left_procs, right_procs ! the process id of left and right  
    integer :: start_col, end_col ! starting/ending column index
    integer, dimension(height) :: loc_left, loc_right ! Local Ghost buffer (Sending out)
    integer, dimension(height) :: rev_left, rev_right ! Local Ghost buffer (Recieved)

    ! Start MPI
    call MPI_INIT(ierr)
    call MPI_COMM_RANK(MPI_COMM_WORLD, my_rank, ierr)
    call MPI_COMM_SIZE(MPI_COMM_WORLD, num_procs, ierr)

    ! Initialize a universe, 
    ! 
    cells(2, 1:3) = 1

    ! In fortran,  The first subscript represents row number, and the second column number.
    !    A(1,1)    A(1,2) . . .    A(1,m)
    !    A(2,1)    A(2,2) . . .    A(2,m)
    !    ......
    !    A(n,1)    A(n,2) . . .    A(n,m)
    ! if (my_rank .eq. 0) then
    !     ! temp print the universe
    !     do i = 1, height
    !         do j = 1, width
    !             if (cells(i, j) .ne. 0) then
    !                 write(*, '(A)', advance='no') "X"
    !             else
    !                 write(*, '(A)', advance='no') "O"
    !             end if
    !         end do
    !         print *, ''
    !     end do
    !     print *, ''
    ! endif

    ! call MPI_BARRIER(MPI_COMM_WORLD, ierr)

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

    ! Debug Print, remove this when finished
    print *, &
    "At rank ", my_rank , &
    ! "left_procs", left_procs, &
    ! "right_procs", right_procs, &
    ", sub-width = ", (width / num_procs), &
    ", from column ", start_col, &
    " to ", end_col

    loc_left = cells(:, start_col)
    loc_right = cells(:, end_col)

    ! print *, "loc_left:"
    ! do i = 1, height
    !     print *, loc_left(i)
    ! enddo

    ! print *, "loc_right:"
    ! do i = 1, height
    !     print *, loc_right(i)
    ! enddo

    call MPI_BARRIER(MPI_COMM_WORLD, ierr)

    ! call MPI_Sendrecv( & 
    !     loc_left, height, MPI_LOGICAL, left_procs, itag, &
    !     rev_left, height, MPI_LOGICAL, left_procs, itag, &
    !     MPI_COMM_WORLD, istat, ierr)

    ! call MPI_Sendrecv( & 
    !     loc_right, height, MPI_INTEGER, right_procs, itag, &
    !     rev_right, height, MPI_INTEGER, right_procs, itag, &
    !     MPI_COMM_WORLD, istat, ierr)

    call MPI_Isend(loc_left, height, MPI_INTEGER, left_procs, itag, MPI_COMM_WORLD, irequest, ierr)
    call MPI_Isend(loc_right, height, MPI_INTEGER, right_procs, itag, MPI_COMM_WORLD, irequest, ierr)
    call MPI_RECV(rev_left, height, MPI_INTEGER, left_procs, itag, MPI_COMM_WORLD, istat, ierr)
    call MPI_RECV(rev_right, height, MPI_INTEGER, right_procs, itag, MPI_COMM_WORLD, istat, ierr)


    ! print *, "rev_left: (", my_rank
    ! do i = 1, height
    !     print *, rev_left(i)
    ! enddo

    ! print *, "rev_right: (", my_rank
    ! do i = 1, height
    !     print *, rev_right(i)
    ! enddo

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

