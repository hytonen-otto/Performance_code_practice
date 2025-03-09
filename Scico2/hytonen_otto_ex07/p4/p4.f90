program p4

    implicit none
    integer :: i
    
    !Wanted to use the same structure as last week and print in the function. 
    !Subroutine would have been better, but the instructions said function. 
    !Function needs to be called awkwardly like this. 
    i=collatz()

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
        collatz = i
        !printing the number of iterations
        print '(a,i10,a,i5,a)','Initial value ',na,' needs',i, &
        ' iterations to reach 1.'
        return
    end function collatz

end program p4