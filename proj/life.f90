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
    integer :: itag = 1111
    integer :: istat(MPI_STATUS_SIZE)

    ! ---------------------------------------------------------------------
    ! Game-of-Life(GoF) related parameters
    ! ---------------------------------------------------------------------
    integer, parameter :: global_height = 5
    integer, parameter :: global_width = 20
    integer, parameter :: max_step = 80

    integer :: global_cells(global_height, global_width)
    integer :: num_live_neighbors = 0
    integer :: current_step = 0

    ! ---------------------------------------------------------------------
    ! Parellel GoF related parameters
    ! ---------------------------------------------------------------------

    ! [height], [width] is the actual board size of this **particular thread**
    integer, parameter :: height = global_height
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
    integer, allocatable :: recv_buffer(:)
    integer, allocatable :: recv_cells(:, :)
    integer, allocatable :: aug_cells(:, :)

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
    integer :: i, j, k, l

    ! ---------------------------------------------------------------------
    ! Parellel File I/O related params
    ! ---------------------------------------------------------------------

    integer :: file_bufsize
    integer, allocatable :: file_buf(:)
    integer :: thefile
    integer(kind=MPI_OFFSET_KIND) :: disp

    character(100) :: filename, step_str

    ! ---------------------------------------------------------------------
    ! Code: Start MPI
    ! ---------------------------------------------------------------------

    call MPI_INIT(ierr)
    call MPI_COMM_RANK(MPI_COMM_WORLD, my_rank, ierr)
    call MPI_COMM_SIZE(MPI_COMM_WORLD, num_procs, ierr)

    ! MPI related: Compute the neighbor ranks
    left_procs = modulo(my_rank - 1, num_procs)
    right_procs = modulo(my_rank + 1, num_procs)

    ! ---------------------------------------------------------------------
    ! Allocate memory for all the dynamic arrays
    ! ---------------------------------------------------------------------

    ! Temporary check, subject to remove
    if (modulo(global_width, num_procs) .ne. 0) then
        print *, ("width of the world can not divid by number of processors!")
        call exit(0)
    end if

    width = global_width / num_procs

    allocate (recv_buffer(width * height))
    allocate (recv_cells(height, width))
    allocate (aug_cells(height + 2, width + 2))

    ! file I/O related
    ! each row has 'width' + '\n', (total height rows)
    ! also print additional 10 character for text
    file_bufsize = height * (width + 1) ! + 1
    allocate(file_buf(file_bufsize))

    ! ----------------------------------------------------------------
    ! Initialize Global World (only initialized in root rank)
    ! Example: [A] should look like this (4x4)
    !
    ! 1  5  9 13
    ! 2  6 10 14
    ! 3  7 11 15
    ! 4  8 12 16
    ! ---------------------------------------------------------------
    
    if (my_rank .eq. root_rank) then

        ! global_cells = reshape((/ (i, i = 1,  global_height * global_width) /), (/global_height, global_width/))
        ! global_cells(2, 1:3) = 1
        global_cells = 0
        global_cells(2, 1) = 1
        global_cells(3, 2) = 1
        global_cells(3, 3) = 1
        global_cells(1, 3) = 1
        global_cells(2, 3) = 1

        if (my_rank .eq. root_rank) then
            print *, '----- Initial board ------'
            do i = 1, global_height
                do j = 1, global_width
                    write(*, '(I3)', advance='no') global_cells(i, j)
                end do
                print *, ''
            end do
            print *, ''
        end if

    end if

    ! ---------------------------------------------------------------------
    ! Scatter and distribute the board to processes
    ! ---------------------------------------------------------------------

    num_cell_per_task = (global_height * global_width) / num_procs

    ! Then distribute the cells to other thread
    call MPI_SCATTER(global_cells, num_cell_per_task, MPI_INTEGER, &
                     recv_buffer, num_cell_per_task, MPI_INTEGER, &
                     root_rank, MPI_COMM_WORLD, ierr)

    ! Convert recieved 1D raw buffer into 2D local cells
    recv_cells = reshape(recv_buffer, (/height, width/))

    ! ---------------------------------------------------------------------
    ! MPI Communication: send edges to other procs as ghost cells
    ! ---------------------------------------------------------------------

    do k = 1, max_step + 1

        loc_left = recv_cells(:, 1)
        loc_right = recv_cells(:, width)

        call MPI_BARRIER(MPI_COMM_WORLD, ierr)

        ! Send and receive ghost cells
        call MPI_SENDRECV(loc_left, height, MPI_INTEGER, left_procs, itag, &
                          rev_right, height, MPI_INTEGER, right_procs, itag, & 
                          MPI_COMM_WORLD, istat, ierr)

        call MPI_SENDRECV(loc_right, height, MPI_INTEGER, right_procs, itag, &
                          rev_left, height, MPI_INTEGER, left_procs, itag, &
                          MPI_COMM_WORLD, istat, ierr)

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

    ! ---------------------------------------------------------------------
    ! Code: Print the state of the board to the file.
    ! We check if we have reached to the 20s mile stone
    ! ---------------------------------------------------------------------

        if (any((/ 0, 20, 40, 60, 80 /) .eq. current_step)) then

            ! prepare file buffer & filename
            write(step_str, '(i0.3)') current_step
            filename = 'gof_' // trim(step_str) // '.dat'

            file_buf = 0
            l = 1
            do i = 1, height

                ! Print entire row
                do j = 1, width
                    if (recv_cells(i, j) .eq. 0) then
                        file_buf(l) = 79 ! ASCII value of 'O'
                    else
                        file_buf(l) = 88 ! ASCII value of 'X'
                    endif 
                    l = l + 1
                end do

                ! Print new line
                file_buf(l) = 10 ! ASCII value of '\n'
                l = l + 1
            end do

            ! Assuming 4-byte integers!!!!!! 
            disp = my_rank * file_bufsize * 4

            call MPI_FILE_OPEN(MPI_COMM_WORLD, filename, & 
                               MPI_MODE_WRONLY + MPI_MODE_CREATE, & 
                               MPI_INFO_NULL, thefile, ierr) 

            call MPI_FILE_SET_VIEW(thefile, disp, MPI_INTEGER, & 
                                   MPI_INTEGER, 'native', & 
                                   MPI_INFO_NULL, ierr) 

            call MPI_FILE_WRITE(thefile, file_buf, file_bufsize, MPI_INTEGER, & 
                                MPI_STATUS_IGNORE, ierr)

            call MPI_FILE_CLOSE(thefile, ierr) 

        endif

        current_step = current_step + 1

    end do

    ! ---------------------------------------------------------------------
    ! Code: Collect & Gather the information
    ! ---------------------------------------------------------------------

    recv_buffer = pack(recv_cells, .true.)

    call MPI_GATHER(recv_buffer, num_cell_per_task, MPI_INTEGER, &
                    global_cells, num_cell_per_task, MPI_INTEGER, &
                    root_rank, MPI_COMM_WORLD, ierr)

    if (my_rank .eq. root_rank) then
        print *, '----- Final board ------'
        do i = 1, global_height
            do j = 1, global_width
                write(*, '(I3)', advance='no') global_cells(i, j)
            end do
            print *, ''
        end do
        print *, ''
    end if

    ! ---------------------------------------------------------------------
    ! Code: Finish MPI
    ! ---------------------------------------------------------------------

    deallocate (recv_buffer) 
    deallocate (recv_cells) 
    deallocate (aug_cells) 
    deallocate (file_buf) 

    call MPI_FINALIZE(ierr)

end program main

