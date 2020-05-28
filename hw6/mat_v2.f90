program mat_v2

    ! Yanwen Xu
    ! yxu83@ucsc.edu
    ! AM 250 
    ! Homework 6 
    ! 
    ! The program Use Fortran MATMUL and OpenMP workshare

    use omp_lib

    implicit none

    integer :: size = 3
    integer, dimension(3, 3) :: mat_a, mat_b, mat_c ! C = A*B
    integer :: i, j, k, sum

    mat_a = reshape((/ 1, 2, 3, 4, 5, 6, 7, 8, 9 /), shape(mat_a), order = (/2, 1/))
    mat_b = reshape((/ 10, 20, 30, 40, 50, 60, 70, 80, 90 /), shape(mat_a), order = (/2, 1/))

    ! Use simple do loop, which iterates each (i,j) cell and compute sum
    mat_c = matmul(mat_a, mat_b)    

    ! Print result
    do i = 1, size
        do j = 1, size
            print *, mat_c(i, j) 
        enddo
        print *, "\n"
    enddo


    !$omp parallel

        ! print *, "Hello from process: ", omp_get_thread_num()

    !$omp end parallel

end program mat_v2