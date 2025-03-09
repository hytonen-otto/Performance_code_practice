program p2

    implicit none
    integer :: i,res,iarg
    character(len=80) :: arg

    !Catching the error of too many cmd line arguments
    iarg=command_argument_count()
    if (iarg/=1) then
        call get_command_argument(0,arg)
        write(0,'(a,a,a)') &
            'usage: ',trim(arg),' i x'
        stop
    end if

    !Getting the commad line argument
    call get_command_argument(1,arg)

    !Getting the index of the first digit in the cmd line argument
    i=scan(arg,'1234567890')
    !Reading the character from the index from last line and 
    !converting it to integer
    read(arg(i:i),*) res

    !Printing the integer and the index
    print '(a,i3,a,i2)', 'First digit of the command line &
     & argument is ',res,' at the index ',i

end program 