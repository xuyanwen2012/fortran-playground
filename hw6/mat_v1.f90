program mat_v1

    ! Yanwen Xu
    ! yxu83@ucsc.edu
    ! AM 250 
    ! Homework 6 
    ! 
    ! This program use do loops to do the matrix multiply and OpenMP PARALLEL DO

    use omp_lib

    implicit none

    integer :: size = 1000 ! Size of all three matrix
    integer :: i, j, k
    real, dimension(1000, 1000) :: mat_a, mat_b, mat_c ! C = A*B
    double precision :: t1, t2

    ! Initialize matrix
    mat_a = 1.0
    mat_b = 1.0
    mat_c = 0.0

    t1 = omp_get_wtime()

    ! Main matrix computation
    !$omp parallel do 

        do i = 1, size
            do j = 1, size
                do k = 1, size
                    mat_c(i, j) = mat_c(i, j) + mat_a(i, k) * mat_b(k, j)
                enddo
            enddo
        enddo

    !$omp end parallel do

    t2 = omp_get_wtime()

    ! Print matrix C result and time elapsed
    ! do i = 1, size
    !     do j = 1, size
    !         print *, mat_c(i, j) 
    !     enddo
    !     print *, ''
    ! enddo

    print *, 'Walltime elapsed', t2 - t1

end program mat_v1