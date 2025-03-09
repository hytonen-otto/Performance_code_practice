program p5

    implicit none

    integer :: i, j
    real(kind=16) :: r=134142131.133423245
    real :: t1, t2, tcpu
    character(len=80) :: arg
    character(len=10) :: b(2)

    !Adjusting the iteration count from cmd line (1000 ~=1.3s)
    call get_command_argument(1,arg)
    read(arg,*) j
    
    !Getting and printing the time befre the "throttling loop"
    call date_and_time(b(1),b(2))
    print*,'Date: ',b(1)
    print*,'Time: ', b(2)
    print '(18(''-''))'
    !Saving the cpu time before loop
    call cpu_time(t1)

    !Loop that extends the runtime of the program
    do i=1, j*100000
        r=r**r
    end do
    !Saving the cpu time after the loop
    call cpu_time(t2)
    !Getting and printing the time after the "throttling loop"
    call date_and_time(b(1),b(2))
    print*,'Date: ',b(1)
    print*,'Time: ', b(2)
    !Calculating and printing time spent in the loop
    tcpu=t2-t1
    print*,'Spent cpu time: ', tcpu, 's'
end program p5