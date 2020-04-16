!---------------------------------------------------------------------
!
!  Main program
!
!---------------------------------------------------------------------

program ones
    implicit none
    interface
        integer function is_valid(x, y, n) result(ok)
            implicit none
            integer, intent(in) :: x, y, n
        end function is_valid
    end interface

    ! Loop counters
    integer :: x, y, i ,j
    
    ! Dimension of the array & the 2d array
    integer :: n
    integer, dimension (:,:), allocatable :: array, new_array

    ! random real between 0, 1 
    real :: rnd

    integer :: temp_sum

    ! ------- Main -----------
    call random_seed()
    
    print *, 'Enter the dimension (one integer): '
    read (*,*) n

    allocate(array(n, n))
    allocate(new_array(n, n))

    ! initialize both array 
    do x = 1, n
        do y = 1, n
            new_array(x, y) = 0

            call random_number(rnd)
            if (rnd .le. 0.5) then
                array(x, y) = 1
            else
                array(x, y) = 0
            endif
        enddo
    enddo

    ! compute the new_array
    do x = 1, n
        do y = 1, n

            ! Count neighbors by iterating through neighbors cells
            temp_sum = 0
            do i = -1, 1
                do j = -1, 1
                    ! Skip self cell
                    if (i .eq. 0 .and. j .eq. 0) then
                        cycle
                    endif

                    if (is_valid(x + i ,y + j, n) .eq. 1) then
                        ! now count
                        if (array(x + i ,y + j) .eq. 1) then
                            temp_sum = temp_sum + 1
                        endif
                    endif 
                enddo
            enddo

            ! print *, "temp_sum = ", temp_sum
            if (temp_sum .ge. 3) then
                new_array(x, y) = 1
            endif

        enddo
    enddo

    ! call count_neighbors(1, 1, n, temp)
    ! print *, "tmp = ", temp 


    ! display the result 
    print *, 'The randomly generated array looks like this: '

    do x = 1, n
        do y = 1, n
            write(*,'(1x,i0)', advance='no') array(x, y)
        enddo
        write(*, *) ''
    enddo

    print *, 'The second array looks like this: '

    do x = 1, n
        do y = 1, n
            write(*,'(1x,i0)', advance='no') new_array(x, y)
        enddo
        write(*, *) ''
    enddo

    deallocate(array)
    deallocate(new_array)

end program ones

! ! a helper function to count how many ones are around (x, y)
! subroutine count_neighbors(x, y, n, sum)
!     implicit none
!     interface
!         integer function is_valid(x, y, n) result(ok)
!             implicit none
!             integer, intent(in) :: x, y, n
!         end function is_valid
!     end interface

!     integer, intent(in) :: x, y, n
!     integer, intent(out) :: sum
!     integer :: i, j

!     do i = -1, 1
!         do j = -1, 1
!             if (i .eq. 0 .and. j .eq. 0) then
!                 cycle
!             endif

!             ! print *, x+i, ' ', y+j 
!             if (is_valid(x+i ,y+j, n) .eq. 1) then
!                 print *, x+i, ' ', y+j 
!             endif 
!         enddo
!     enddo

! end subroutine count_neighbors

! check if x is in 1..n
integer function is_valid(x, y, n) result(ok)
    implicit none
    integer, intent(in) :: x, y, n
    if (x .le. n .and. x .ge. 1 .and. y .le. n .and. y .ge. 1) then
        ok = 1
    else 
        ok = 0
    endif
end function is_valid
