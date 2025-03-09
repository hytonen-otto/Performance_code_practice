program p5

    implicit none
    integer :: i

    !It returns the number of iterations
    i = collatz()

contains

    integer(kind=16) function collatz()
    implicit none
    integer(kind=16) :: n,na,i
    character(len=20) :: arg
    !getting the value of n_0 from the command line
    call get_command_argument(1,arg)
    read(arg,*) n
    na=n
    i=0
    !Iterating according to the conjecture until 1 or one of the loops is reached
    do 
        if (n==1.or.n==0.or.n==-1.or.n==-5.or.n==-17) then
            exit
        else if (mod(n,2)==0) then
            n=n/2
        else
            n=3*n+1 
        end if
        i=i+1
    end do
    collatz = i
    !printing the number of iterations
    print '(a,i10,a,i5,a)','Initial value ',na,' needs',i, &
    ' iterations to reach one of the loops or 1.'
    return
end function collatz

end program p5