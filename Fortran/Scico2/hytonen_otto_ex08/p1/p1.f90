program p1

    implicit none

    complex :: a=(1,2)

    !Testing different functions
    !Only bessel doesn't support comlex numbers
    !The code won't compile if you remove the comments before the bessel line.
    print*,'sin(a):'
    print*, sin(a)
    print*,'tan(a):'
    print*, tan(a)
    print*,'sinh(a):'
    print*, sinh(a)
    print*,'tanh(a):'
    print*, tanh(a)
    print*,'exp(a):'
    print*, exp(a)
    print*,'log(a):'
    print*, log(a)
    !print*,'bessel_j0(a):'
    !print*, bessel_j0(a)

end program p1