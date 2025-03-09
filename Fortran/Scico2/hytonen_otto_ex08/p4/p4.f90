program p4

    implicit none

    real, dimension(3) :: a
    integer, dimension(3) :: n
    read*,a
    read*,n

    !The test values
    print '(a,f5.2,a,i2,a,f12.6)','a=',a(1),' n= ',n(1),':',tetration(a(1),n(1))
    print '(a,f5.2,a,i2,a,f12.1)','a=',a(2),' n= ',n(2),':',tetration(a(2),n(2))
    print '(a,f5.2,a,i2,a,e12.4)','a=',a(3),' n= ',n(3),':',tetration(a(3),n(3))

    contains 
    !Input real a, the value that is tetrated
    !Input integer n, the "amount" of tetrations (iterations)
    !Output real, the tetrated value
    real function tetration(a,n)
        implicit none
        real, intent(in) :: a
        integer, intent(in) :: n
        integer :: i
        tetration=a 
        !The definition of tetration
        do i=2,n
            tetration=a**tetration            
        end do
    end function tetration

end program p4