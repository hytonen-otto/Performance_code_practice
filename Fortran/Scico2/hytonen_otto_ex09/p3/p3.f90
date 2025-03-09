program p3

    !MODIFIED THE RANDOMNUMBER PROGRAM TO PRINT BETWEEN 0 AND 1

    implicit none

    integer :: i, iarg
    real :: a,b,ratio
    real,dimension(10000) :: numbers
    character(len=80) :: arg

    !Cathing the error of too many cmd line arguments
    iarg=command_argument_count()
    if (iarg/=2) then
        call get_command_argument(0,arg)
        write(0,'(a,a,a)') &
            'usage: ',trim(arg),' i x'
        stop
    end if

    !Reading the random numbers from file specifiend in cmd line
    read *,numbers

    !Getting the commad line arguments for the limits
    call get_command_argument(1,arg)
    read(arg,*) a
    call get_command_argument(2,arg)
    read(arg,*) b

    !Counting the amount of random numbers between the limits
    i=count(numbers>=a.and.numbers<=b)
    !The ratio of numbers between a and b and all numbers
    ratio=i/real(size(numbers))

    print '(a,f6.4,a,f6.4,a,f8.6)','The fraction of the numbers between ',a,' and ',b,&
    ' is ',ratio
end program