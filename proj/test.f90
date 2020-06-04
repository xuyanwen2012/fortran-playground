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
    a = reshape((/ (i, i = 1, width * height) /), (/width, height/))
    b = (/ ( 20 + i, i = 1, height) /)
    c = (/ ( 30 + i, i = 1, height) /)

    ! Copy a to d
    do i = 1, height
        do j = 1, width
            d(i + 1, j + 1) = a(i, j)
        enddo
    enddo

    ! Copy b, c to d
    d(2:height + 1, 1) = b
    d(2:height + 1, width + 2) = c

    ! Copy d's top and bottom
    d(1, :) = d(height + 1, :)
    d(height + 2, :) = d(2, :)

    ! Do game of life simulation from (2, 2) to (w+1, h+1)

    do i = 2, height + 1
        do j = 2, width + 1
            d(i, j) = d(i, j) + 80
        enddo
    enddo

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
