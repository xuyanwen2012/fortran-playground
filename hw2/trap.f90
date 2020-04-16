program trap
    implicit none

    ! loop counters
    integer :: i, n

    ! lower and upper limit
    real :: a, b 

    ! f(x) value
    real :: val, temp
    
    ! used for integral computation
    real :: delta, sum = 0.0
    real :: next

    ! ------- Main -----------
    print *, 'Enter lower & upper bound (two reals): '
    read (*,*) a, b
    print *, 'Enter interval N (one integer): '
    read(*,*) n

    n = 10
    delta = (b - a) / n
    next = a

    print *, 'delta = ', delta

    do i = 1, n
        call x_squared(next, val)    
        next = next + delta 
        call x_squared(next, temp)

        print *, (val + temp) / 2 * delta
        sum = sum + (val + temp) / 2 * delta
    enddo

    print *, 'sum = ', sum

end program trap


!---------------------------------------------------------------------
!
!  Subroutines
!
!---------------------------------------------------------------------
subroutine x_squared(x, y)
    implicit none

    real, intent(in) :: x
    real, intent(out) :: y

    y = x ** 2

end subroutine x_squared

subroutine sin_x(x, y)
    implicit none

    real, intent(in) :: x
    real, intent(out) :: y

    y = sin(x)

end subroutine sin_x