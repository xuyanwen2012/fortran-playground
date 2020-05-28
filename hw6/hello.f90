program hello

    ! Yanwen Xu
    ! yxu83@ucsc.edu
    ! AM 250 
    ! Homework 6 

    use omp_lib

    implicit none

    integer :: thread_id

    !$omp parallel private(thread_id)

        thread_id = omp_get_thread_num()
        print *, "Hello from process: ", thread_id

    !$omp end parallel

end program hello