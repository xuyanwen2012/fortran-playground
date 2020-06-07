program main

    ! ---------------------------------------------------------------------
    ! Yanwen Xu
    ! yxu83@ucsc.edu
    ! AM 250 
    ! Final Project - Game of Life (2D version)
    !
    ! Note: For the input, assume the board is a square 
    ! and can be divide by the number of processor
    ! ---------------------------------------------------------------------

    use mpi

    implicit none
    
    interface
        pure function tile_n2ij(n, width) result(ij)
            implicit none
            integer, intent(in) :: n
            integer, intent(in) :: width
            integer :: ij(2), i, j
        end function

        pure function tile_ij2n(ij, width) result(n)
            implicit none
            integer, intent(in) :: ij(2)
            integer, intent(in) :: width
            integer :: n
        end function

        pure function tile_round_ij(ij, width) result(rounded_ij)
            implicit none
            integer, intent(in) :: ij(2)
            integer, intent(in) :: width
            integer :: rounded_ij(2), i, j
        end function
    end interface

    ! ---------------------------------------------------------------------
    ! MPI related parameters
    ! ---------------------------------------------------------------------

    integer, parameter :: root_rank = 0
    integer :: ierr, my_rank, num_procs, num_procs_per_row
    integer :: itag, irequest
    integer :: istat(MPI_STATUS_SIZE)

    ! ---------------------------------------------------------------------
    ! Game-of-Life(GoF) related parameters
    ! ---------------------------------------------------------------------
    integer, parameter :: global_height = 9
    integer, parameter :: global_width = 9
    integer, dimension(global_height, global_width) :: global_cells
    integer :: num_live_neighbors = 0

    ! ---------------------------------------------------------------------
    ! Parellel GoF related parameters
    ! ---------------------------------------------------------------------

    ! [height], [width] is the actual board size of this **particular thread**
    integer :: height
    integer :: width

    ! [recv_buffer] is the raw 1D buffer recieved from MPI_SCATTER
    ! which is intented to be reshaped to 2D.
    !
    ! [recv_cells] should be the actual 2D board (w*h) and it is **LOCAL**
    ! which will be used twice: 
    ! 1) first we use it to recieve board from MPI_SCATTER
    ! 2) then we use it to send back to main when MPI_GATHER
    ! 
    ! [aug_cells] augmented cells, which should include Ghost cells. 2 more
    ! than the [recv_cells]. But when we do the simulation, we should only 
    ! work on the area from (2, 2) to (w+1, h+1)
    ! integer, dimension(:), allocatable :: recv_buffer
    integer, dimension(:, :), allocatable :: recv_cells

    integer, dimension(:, :), allocatable :: aug_cells

    ! [rev_left] and [rev_right] are strip (1*h), i.e. The local Ghost buffer
    ! which should be recieved from tge left, the right neighbor, respectively
    ! 
    ! [loc_left] and [loc_right] are out sending local buffer,  
    ! which are essentially: cells(1, :) and cell(:, width)
    integer, dimension(:), allocatable :: rev_left, rev_right
    integer, dimension(:), allocatable :: rev_upper, rev_lower

    integer :: rev_upper_left, rev_upper_right
    integer :: rev_lower_left, rev_lower_right

    integer, dimension(:), allocatable :: loc_left, loc_right
    integer, dimension(:), allocatable :: loc_upper, loc_lower

    integer :: loc_upper_left, loc_upper_right
    integer :: loc_lower_left, loc_lower_right

    ! For MPI use only: the process id of the neighbors
    integer :: left_procs, right_procs
    integer :: upper_procs, lower_procs
    integer :: upper_left_procs, upper_right_procs
    integer :: lower_left_procs, lower_right_procs

    integer :: num_cell_per_task
    integer :: i, j, k

    integer :: ij(2)

    ! ---------------------------------------------------------------------
    ! Code: Start MPI
    ! ---------------------------------------------------------------------

    call MPI_INIT(ierr)
    call MPI_COMM_RANK(MPI_COMM_WORLD, my_rank, ierr)
    call MPI_COMM_SIZE(MPI_COMM_WORLD, num_procs, ierr)

    itag = 1111

    if (modulo(global_width, num_procs) .ne. 0) then
        print *, ("width of the world can not divid by number of processors!")
        call exit(0)
    end if

    if (modulo(global_height, num_procs) .ne. 0) then
        print *, ("width of the world can not divid by number of processors!")
        call exit(0)
    end if

    num_procs_per_row = sqrt(real(num_procs))

    height = global_height / num_procs
    width = global_width / num_procs

    if (my_rank .eq. root_rank) then
        print *, "num_procs_per_row: ", num_procs_per_row
        print *, "height: ", height
        print *, "width: ", width
        print *, ""
    endif

    ! MPI related: Compute the neighbor ranks
    ! 
    !        Upper
    !   Left --+-- Right
    !        Lower
    !
    !    j 0   1   2
    !  i +---+---+---+
    !  0 | 0 | 3 | 6 |
    !    +---+---+---+
    !  1 | 1 | 4 | 7 |
    !    +---+---+---+
    !  2 | 2 | 5 | 8 |
    !    +---+---+---+
    !
    upper_procs = tile_ij2n(tile_round_ij( & 
        tile_n2ij(my_rank, num_procs_per_row) + [0, -1], &
        num_procs_per_row), num_procs_per_row)
    
    lower_procs = tile_ij2n(tile_round_ij( & 
        tile_n2ij(my_rank, num_procs_per_row) + [0, 1], &
        num_procs_per_row), num_procs_per_row)

    left_procs = tile_ij2n(tile_round_ij( & 
        tile_n2ij(my_rank, num_procs_per_row) + [-1, 0], &
        num_procs_per_row), num_procs_per_row)

    right_procs = tile_ij2n(tile_round_ij( & 
        tile_n2ij(my_rank, num_procs_per_row) + [1, 0], &
        num_procs_per_row), num_procs_per_row)

    upper_left_procs = tile_ij2n(tile_round_ij( & 
        tile_n2ij(my_rank, num_procs_per_row) + [-1, -1], &
        num_procs_per_row), num_procs_per_row)
    
    upper_right_procs = tile_ij2n(tile_round_ij( & 
        tile_n2ij(my_rank, num_procs_per_row) + [1, -1], &
        num_procs_per_row), num_procs_per_row)

    lower_left_procs = tile_ij2n(tile_round_ij( & 
        tile_n2ij(my_rank, num_procs_per_row) + [-1, 1], &
        num_procs_per_row), num_procs_per_row)

    lower_right_procs = tile_ij2n(tile_round_ij( & 
        tile_n2ij(my_rank, num_procs_per_row) + [1, 1], &
        num_procs_per_row), num_procs_per_row)

    if (my_rank .eq. root_rank) then
        print *, "upper_procs: ", upper_procs
        print *, "lower_procs: ", lower_procs
        print *, "left_procs : ", left_procs
        print *, "right_procs: ", right_procs
        print *, "upper_left_procs : ", upper_left_procs
        print *, "upper_right_procs: ", upper_right_procs
        print *, "lower_left_procs : ", lower_left_procs
        print *, "lower_right_procs: ", lower_right_procs
    endif

    ! ---------------------------------------------------------------------
    ! Allocate memory for all the dynamic arrays
    ! ---------------------------------------------------------------------

    allocate (rev_left(height))
    allocate (rev_right(height))
    allocate (rev_upper(width))
    allocate (rev_lower(width))

    allocate (loc_left(height))
    allocate (loc_right(height))
    allocate (loc_upper(width))
    allocate (loc_lower(width))

    allocate (recv_cells(height, width))
    allocate (aug_cells(height + 2, width + 2))

    ! ---------------------------------------------------------------------
    ! Initialize Global World  
    ! Example: [A] should look like this (4x4)
    !
    ! 1  5  9 13
    ! 2  6 10 14
    ! 3  7 11 15
    ! 4  8 12 16
    ! ---------------------------------------------------------------------

    ! global_cells = reshape((/ (i, i = 1,  global_height * global_width) /), (/global_height, global_width/))
    ! global_cells(2, 1:3) = 1
    global_cells = 0
    global_cells(2, 1) = 1
    global_cells(3, 2) = 1
    global_cells(3, 3) = 1
    global_cells(1, 3) = 1
    global_cells(2, 3) = 1

    if (my_rank .eq. 0) then
        print *, '----- Initial board ------'
        do i = 1, global_height
            do j = 1, global_width
                write(*, '(I3)', advance='no') global_cells(i, j)
            end do
            print *, ''
        end do
        print *, ''
    end if

    ! ! ---------------------------------------------------------------------
    ! ! Scatter and distribute the board to processes
    ! ! ---------------------------------------------------------------------

    ! num_cell_per_task = (global_height * global_width) / num_procs
    ! ! or maybe just width * height

    ! call MPI_SCATTER(global_cells, num_cell_per_task, MPI_INTEGER, &
    !                  recv_buffer, num_cell_per_task, MPI_INTEGER, &
    !                  root_rank, MPI_COMM_WORLD, ierr)

    ! ! Convert recieved 1D raw buffer into 2D local cells
    ! recv_cells = reshape(recv_buffer, (/height, width/))

    ! ---------------------------------------------------------------------
    ! MPI Communication: send edges to other procs as ghost cells
    ! ---------------------------------------------------------------------

    do k = 1, 4

        ! **** This is a fake [recv_cells], because I gave up using Scatter
        recv_cells = 0
        ! if (my_rank .eq. root_rank) then
        !     recv_cells = 1
        ! endif

        loc_left = recv_cells(:, 1)
        loc_right = recv_cells(:, width)
        loc_upper = recv_cells(1, :)
        loc_lower = recv_cells(width, :)

        loc_upper_left = recv_cells(1, 1)
        loc_upper_right = recv_cells(1, width)
        loc_lower_left = recv_cells(height, 1)
        loc_lower_right =  recv_cells(height, width)

        call MPI_BARRIER(MPI_COMM_WORLD, ierr)

        ! The four sides, which has either width or height integers
        call MPI_ISEND(loc_left, height, MPI_INTEGER, left_procs, & 
                       itag, MPI_COMM_WORLD, irequest, ierr)

        call MPI_ISEND(loc_right, height, MPI_INTEGER, right_procs, &
                       itag, MPI_COMM_WORLD, irequest, ierr)
        
        call MPI_ISEND(loc_upper, width, MPI_INTEGER, upper_procs, & 
                       itag, MPI_COMM_WORLD, irequest, ierr)

        call MPI_ISEND(loc_lower, width, MPI_INTEGER, lower_procs, &
                       itag, MPI_COMM_WORLD, irequest, ierr)

        ! The four corners, which is only one integer
        call MPI_ISEND(loc_upper_left, 1, MPI_INTEGER, upper_left_procs, & 
                       itag, MPI_COMM_WORLD, irequest, ierr)

        call MPI_ISEND(loc_upper_right, 1, MPI_INTEGER, upper_right_procs, &
                       itag, MPI_COMM_WORLD, irequest, ierr)
        
        call MPI_ISEND(loc_lower_left, 1, MPI_INTEGER, lower_left_procs, & 
                       itag, MPI_COMM_WORLD, irequest, ierr)

        call MPI_ISEND(loc_lower_right, 1, MPI_INTEGER, lower_right_procs, &
                       itag, MPI_COMM_WORLD, irequest, ierr)

        ! Recieving the four sides
        call MPI_RECV(rev_left, height, MPI_INTEGER, left_procs, &
                      itag, MPI_COMM_WORLD, istat, ierr)
        
        call MPI_RECV(rev_right, height, MPI_INTEGER, right_procs, & 
                      itag, MPI_COMM_WORLD, istat, ierr)

        call MPI_RECV(rev_upper, width, MPI_INTEGER, upper_procs, &
                      itag, MPI_COMM_WORLD, istat, ierr)
        
        call MPI_RECV(rev_lower, width, MPI_INTEGER, lower_procs, & 
                      itag, MPI_COMM_WORLD, istat, ierr)

        ! Recieving the four corners

        call MPI_RECV(rev_upper_left, 1, MPI_INTEGER, upper_left_procs, &
                      itag, MPI_COMM_WORLD, istat, ierr)
        
        call MPI_RECV(rev_upper_right, 1, MPI_INTEGER, upper_right_procs, & 
                      itag, MPI_COMM_WORLD, istat, ierr)

        call MPI_RECV(rev_lower_left, 1, MPI_INTEGER, lower_left_procs, &
                      itag, MPI_COMM_WORLD, istat, ierr)
        
        call MPI_RECV(rev_lower_right, 1, MPI_INTEGER, lower_right_procs, & 
                      itag, MPI_COMM_WORLD, istat, ierr)


        ! ---------------------------------------------------------------------
        ! Prepare the augmented cells
        ! ---------------------------------------------------------------------

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
        aug_cells(2 : height + 1, 1        ) = rev_left
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

        ! Copy [E], [F] to [D]
        aug_cells(1,          2 : width + 1) = rev_upper
        aug_cells(height + 2, 2 : width + 1) = rev_lower

        ! At this point, we should have all we need per thread
        ! Example: [D] should look like this (6x6)
        ! 
        !  0  4  8 12 16  0
        ! 21  1  5  9 13 31
        ! 22  2  6 10 14 32
        ! 23  3  7 11 15 33
        ! 24  4  8 12 16 34
        !  0  1  5  9 13  0

        ! Copy the four corners
        aug_cells(1,          1        ) = rev_upper_left
        aug_cells(1,          width + 2) = rev_upper_right
        aug_cells(height + 2, 1        ) = rev_lower_left
        aug_cells(height + 2, width + 2) = rev_lower_right

    ! ---------------------------------------------------------------------
    ! Do Game-of-Life Simulation logics
    !
    ! Note: work only on the area from (2, 2) to (w+1, h+1)
    ! ---------------------------------------------------------------------

        ! Perform one step simulation 
        do i = 2, height + 1
            do j = 2, width + 1

                ! Count number of live neighbors at cell (i, j)
                num_live_neighbors = sum(aug_cells(i - 1:i + 1, j - 1:j + 1))

                if (aug_cells(i, j) .ne. 0) then
                    num_live_neighbors = num_live_neighbors - 1
                end if

                ! Perform GoF simulation, update the buffer
                select case (num_live_neighbors)
                    case (3)
                        recv_cells(i - 1, j - 1) = 1
                    case (2)
                        ! Do nothing
                    case default
                        recv_cells(i - 1, j - 1) = 0
                end select

            end do
        end do

        ! Do I/O print here

    end do

    ! ! ---------------------------------------------------------------------
    ! ! Code: Collect & Gather the information
    ! ! ---------------------------------------------------------------------

    ! recv_buffer = pack(recv_cells, .true.)

    ! call MPI_GATHER(recv_buffer, num_cell_per_task, MPI_INTEGER, &
    !                 global_cells, num_cell_per_task, MPI_INTEGER, &
    !                 root_rank, MPI_COMM_WORLD, ierr)

    ! if (my_rank .eq. 0) then
    !     print *, '----- Final board ------'
    !     do i = 1, global_height
    !         do j = 1, global_width
    !             write(*, '(I3)', advance='no') global_cells(i, j)
    !         end do
    !         print *, ''
    !     end do
    !     print *, ''
    ! end if

    ! ---------------------------------------------------------------------
    ! Code: Finish MPI
    ! ---------------------------------------------------------------------

    deallocate (rev_left)
    deallocate (rev_right)
    deallocate (rev_upper)
    deallocate (rev_lower)

    deallocate (loc_left)
    deallocate (loc_right)
    deallocate (loc_upper)
    deallocate (loc_lower)

    ! deallocate (recv_buffer) 
    deallocate (recv_cells) 
    deallocate (aug_cells) 

    call MPI_FINALIZE(ierr)

end program main


pure function tile_n2ij(n, width) result(ij)
    ! Given tile index in a 1-d layout, returns the corresponding tile indices 
    ! in a 2-d layout.
    ! 
    !    j 0   1  
    !  i +---+---+
    !  0 | 0 | 2 |
    !    +---+---+
    !  1 | 1 | 3 |
    !    +---+---+
    !
    ! Examples:
    ! 
    ! * tile_n2ij(0) = [0, 0]
    ! * tile_n2ij(1) = [1, 0]
    ! * tile_n2ij(2) = [0, 1]
    ! * tile_n2ij(3) = [1, 1]
    !
    implicit none

    integer, intent(in) :: n
    integer, intent(in) :: width
    integer :: ij(2), i, j

    if (n .eq. 0) then
        ij = 0
    else 
        i = n / width
        j = modulo(n, width)
        ij = [i, j]
    end if
  
end function


pure function tile_ij2n(ij, width) result(n)
    ! Given tile index in a 2-d layout, returns the corresponding tile indices 
    ! in a 1-d layout.
    ! 
    !    j 0   1  
    !  i +---+---+
    !  0 | 0 | 2 |
    !    +---+---+
    !  1 | 1 | 3 |
    !    +---+---+
    !
    ! Examples:
    ! 
    ! * tile_n2ij([0, 0]) = 0
    ! * tile_n2ij([0, 1]) = 1
    ! * tile_n2ij([1, 0]) = 2
    ! * tile_n2ij([1, 1]) = 3
    !
    implicit none

    integer, intent(in) :: ij(2)
    integer, intent(in) :: width
    integer :: n

    n = ij(1) * width + ij(2)
  
end function

pure function tile_round_ij(ij, width) result(rounded_ij)
    ! Using modulo to round the [ij] into the correct [ij]
    ! 
    ! Example:
    ! 
    ! * tile_round_ij([-1, 0], 2) = [1, 0] 
    ! * tile_round_ij([2, 0], 2) = [0, 0]    
    ! 
    implicit none

    integer, intent(in) :: ij(2)
    integer, intent(in) :: width
    integer :: rounded_ij(2), i, j

    i = modulo(ij(1) , width)
    j = modulo(ij(2) , width)

    rounded_ij = [i, j]

end function
