program p5
    
    implicit none

    integer :: i
    integer(kind=8) :: n,na !Kind 4 was not big enough for 827467313
    character(len=20) :: arg

    !getting the value of n_0 from the command line
    call get_command_argument(1,arg)
    read(arg,*) n
    na=n
    i=0
    
    !Iterating according to the conjecture until 1 is reached
    do 
        if (n==1) then
            exit
        else if (mod(n,2)==0) then
            n=n/2
        else
            n=3*n+1 
        end if
        i=i+1
    end do

    !printing the number of iterations
    print '(a,i10,a,i5,a)','Initial value ',na,' needs',i, ' iterations to reach 1.'

end program p5