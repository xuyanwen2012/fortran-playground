program p
    implicit none
    
    ! width and height are the actual board size
    ! A should be the actual 2D board (w*h), which should be passed in from the main thread
    ! B, C is a strip (1*h) which should be send from left, right neighbor, respectively
    ! D is the augmented board which should include all ghost cells (w+2)*(h+2)
    ! But when we do the simulation, we should only work on 
    ! from (2, 2) to (w+1, h+1)
    integer, parameter :: width = 4
    integer, parameter :: height = 4
    integer, dimension(width, height) :: a 
    integer, dimension(height) :: b, c
    integer, dimension(width + 2, height + 2) :: d = 0

    integer :: i, j, k

    ! Initialize a, b, c  
    ! Example: A should look like this (4x4)
    !
    ! 1  5  9 13
    ! 2  6 10 14
    ! 3  7 11 15
    ! 4  8 12 16

    a = reshape((/ (i, i = 1, width * height) /), (/width, height/))
    b = (/ ( 20 + i, i = 1, height) /)
    c = (/ ( 30 + i, i = 1, height) /)

    ! Copy a to d
    d(2 : height + 1, 2 : width + 1) = a

    ! Copy b, c to d
    d(2 : height + 1, 1) = b
    d(2 : height + 1, width + 2) = c

    ! Copy d's top and bottom
    d(1, :) = d(height + 1, :)
    d(height + 2, :) = d(2, :)

    ! At this point, we should have all we need per thread
    ! Example: D should look like this (6x6)
    ! 
    ! 24  4  8 12 16 34
    ! 21  1  5  9 13 31
    ! 22  2  6 10 14 32
    ! 23  3  7 11 15 33
    ! 24  4  8 12 16 34
    ! 21  1  5  9 13 31


    ! Do game of life simulation from (2, 2) to (w+1, h+1)

    ! do i = 2, height + 1
    !     do j = 2, width + 1
    !         d(i, j) = d(i, j) + 80
    !     enddo
    ! enddo

    ! Print the board
    print *, ''
    do i = 1, height + 2
        do j = 1, width + 2
            write(*, '(I3)', advance='no') d(i, j)
        enddo
        print *, ''
    enddo
    print *, ''

end program p
