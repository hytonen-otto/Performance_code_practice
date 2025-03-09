program p5

    use series
    implicit none

    real,allocatable :: a(:), b(:)
    integer,allocatable :: c(:)
    integer :: n

    n=2
    !Allocating the input arrays
    allocate(a(n),b(n),c(n))
    !Reading from input file
    read*,a
    read*,b
    read*,c

    !Printing the test values
    print*,'Series (4.0,0.5,10)'
    print '(10f10.4)',geomseries(a(1),b(1),c(1))

    print*,'Series (2.0,-2.0,5)'
    print '(10f10.4)',geomseries(a(2),b(2),c(2))
end program p5