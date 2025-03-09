program p1

    implicit none
    integer :: i, j
    !Test values
    integer, dimension(4) :: a=(/0,1,5,10/)
    
    !Looping through the test values
    do i=1,4
        j=fact(a(i))
        print '(a,i2,a,i7)', 'Factorial ', a(i), '=  ', j
    end do
    

contains

    !Input integer n
    !Output intger n!
    integer function fact(n)
        implicit none
        integer, intent(in) :: n
        integer :: i
        fact=1
        !Calculating the factorial by iterating 1*1*2*3*...*n
        !if n=0, do loop does not run and the value of fact stays 1
        do i=1,n
            fact=i*fact
        end do
        return
    end function fact

end program p1