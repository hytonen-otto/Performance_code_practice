program p6

    implicit none

    integer :: n,j
    real(kind=8) :: s, h, yla, ala
    !establishing pi
    real, parameter :: pi=4.*atan(1.0)

    !Integration limits
    ala=0
    yla=pi

    !Using do loop to find the number of iterations for the required precision
    do n=2,1000
        !Calculaing the integral using n number of divisions
        s=0
        h=(yla-ala)/n
        !The sum part of the equation
        do j=1,n-1
            s=s+sin(ala+j*h)
        end do
        !The whole equation
        s=s*h+(h/2)*(sin(ala)+sin(yla))

        !Exiting the loop when the required precision is found.
        if (s<2.00001.and.s>1.99999) then
            exit
        end if
    end do

    !Printing the number of divisions and the value of the integral
    print '(a,i4,3x,a,f16.14)','N: ',n,'Value: ',s

end program p6