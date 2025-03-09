program p2

    implicit none
    real :: x,y,z

    !Calculating and printing the values of sin(1/2) according to the problem instructions.
    x=sin(0.5)
    y=mysin(0.5,2)
    z=mysin(0.5,4)
    print '(a,f10.8)', 'sin(0.5)    = ', x
    print '(a,f10.8)', 'mySin(0.5,2)= ', y
    print '(a,f10.8)', 'mySin(0.5,4)= ', z

contains 

    !Input integer n
    !Output integer, the factorial of n 
    integer function fact(n)
        implicit none
        integer :: i
        integer, intent(in) :: n
        fact=1
        do i=1,n
            fact=i*fact
        end do
        return
    end function fact

    !Input real x, the value inside sin(x)
    !Input integer m, the number of iterations
    !Output real, the calculated value of sin(x)
    real(kind=16) function mysin(x,m)
        implicit none 
        integer, intent(in) :: m
        real, intent(in) :: x
        integer :: i
        !Series expansion for sin(x)
        do i=0,m-1
            mysin=mysin+(((-1)**i)*x**(2*i+1))/fact(2*i+1)
        end do
        return
    end function mysin

end program p2