program p6

    implicit none

    integer :: i
    real :: a, b, c, d, e, f, x(6), p(6,6)

    print *, "Choose the initial state (1-6):"
    read *, i
    x = 0
    x(i) = 1

    !Reading and normalizing the matrix
    print *, "Give the propability matrix row by row:"

    do i = 1, 6
        print '(a,i3,a)', "Row", i,":"
        !Reading 6 variables (seperated by space or enter) 
        read *, a,b,c,d,e,f
        !Normalizing the varibles and putting them into the p array 
        p(i,:)=[a,b,c,d,e,f]/sum([a,b,c,d,e,f])
    end do

    !Doing the matrix multiplication 1 000 000 times.
    do i = 1, 1000000
        x = matmul(x,p)
    end do
    !Printing the "propability vector"
    print *, "The propability of each state is:"
    print *, x

end program p6