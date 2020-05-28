program hello

    ! Yanwen Xu
    ! yxu83@ucsc.edu
    ! AM 250 
    ! Homework 6 

    use omp_lib

    implicit none

    !$omp parallel

        print *, "Hello from process: ", omp_get_thread_num()

    !$omp end parallel

end program hello