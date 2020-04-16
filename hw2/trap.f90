!---------------------------------------------------------------------
!
!  Main program
!
!---------------------------------------------------------------------

program trap
    implicit none

    ! loop counters
    integer :: i, n
    integer :: select

    ! lower and upper limit
    real :: a, b 

    ! f(x) value
    real :: val, temp
    
    ! used for integral computation
    real :: delta, sum = 0.0
    real :: next

    real :: selected_func
    ! x_squared, sin_x, 

    ! ------- Main -----------
    print *, 'Enter lower & upper bound (two reals): '
    read (*,*) a, b
    print *, 'Enter interval N (one integer): '
    read(*,*) n
    do while (select .ne. 1 .and. select .ne. 2)
        print *, 'Enter 1 for f(x) = x^2, enter 2 for f(x) = sin(x) (one integer either 1 or 2): '
        read(*,*) select
    enddo

    delta = (b - a) / n
    next = a
    ! print *, 'delta = ', delta

    do i = 1, n
        call do_function(select, next, val)
        next = next + delta 
        call do_function(select, next, temp)

        sum = sum + (val + temp) / 2 * delta
    enddo

    print *, 'result = ', sum

end program trap


!---------------------------------------------------------------------
!
!  Subroutines/Functions
!
!---------------------------------------------------------------------

! This subroutine is used to select which f(x) to use
subroutine do_function(select, x, result)
    implicit none
    interface
        real function x_squared(x) result(y)
            implicit none
            real, intent(in) :: x
        end function x_squared
        real function sin_x(x) result(y)
            implicit none
            real, intent(in) :: x
        end function sin_x
    end interface

    integer, intent(in) :: select
    real, intent(in) :: x
    real, intent(out) :: result
    if (select .eq. 1) then
        result = x_squared(x)
    else
        result = sin_x(x)
    endif
end subroutine do_function

! f(x) = x^2
real function x_squared(x) result(y)
    implicit none
    real, intent(in) :: x
    y = x ** 2
end function x_squared

! f(x) = sin(x)
real function sin_x(x) result(y)
    implicit none
    real, intent(in) :: x
    y = sin(x)
end function sin_x
