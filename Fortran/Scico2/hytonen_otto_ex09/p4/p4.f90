module integrate

contains
    !Compiler didn't allow using erf as an argument in gauss_int
    !so I replaced it with this
    !The error was: Intrinsic 'erf' at (1) is not allowed as 
    !an actual argument
    real function nerf(x)
        implicit none
        real,intent(in) :: x
        nerf=erf(x)
    end function nerf

    !Calculating the gauss integral 
    !Input f=intrinsic function
    !Input a and b = real, integration limits
    !Output real, value of the integral
    real function gauss_int(f,a,b)
        implicit none
        real,intent(in) :: a,b
        real,external :: f
        gauss_int=((b-a)/2)*(&
        f(((a+b)/2)-((b-a)/(2*(3**0.5))))&
        +f(((a+b)/(2))+((b-a)/(2*(3**0.5)))))
    end function gauss_int
end module integrate

program p4
    use integrate
    implicit none

    !declaring the used intrinsic functions
    intrinsic :: sin,erf
    real :: res1, res2

    !Calculating and printing the integrals
    res1=gauss_int(sin,0.0,3.0)
    res2=gauss_int(nerf,0.0,2.0)
    print'(a,f8.6)','Integral of sin(x) from 0 to 3: ',res1
    print'(a,f8.6)','Integral of erf(x) from 0 to 2: ',res2

end program