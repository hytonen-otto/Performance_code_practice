program p5

    implicit none

    integer :: i,n 
    real :: x, mean, variance
    !declaring memory allocatable arrays
    real, dimension(:), allocatable :: a
    
    !Reading the dimension of the array
    write(6,*) "Give the size of the array:"
    read *, n

    !Allocating the needed memory
    allocate(a(n))

    print '(a,i3,x,a)', "Give", n,"real numbers." 

    !Do loops for asking the elements of the array
    do i=1, n
        read *, x
        a(i) = x
    end do

    !Calculating the mean and variance of the array
    mean = sum(a)/n
    variance = (sum((a-mean)**2))/n 


    !Printing the mean and variance
    print '(a,f16.6)', "Mean:", mean 
    print '(a,f16.6)', "Variance:", variance

end program p5