program p
    implicit none
    
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

    ! do i = 1, height
        
    ! enddo

    print *, ''

    do i = 1, height + 2
        do j = 1, width + 2
            write(*, '(I3)', advance='no') d(i, j)
        enddo
        print *, ''
    enddo
    print *, ''

end program p
