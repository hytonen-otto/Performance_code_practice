program p2

    implicit none

    integer :: i,j,n, x
    !declaring memory allocatable arrays
    integer, dimension(:,:), allocatable :: a, b
    
    !Reading the dimension (x=y) of the array
    write(6,*) "Give the dimension of the array:"
    read *, n

    !Allocating the needed memory
    allocate(a(n,n))
    allocate(b(n,n))

    print *, "Give the elements of the matrix:"

    !Do loops for asking the elements of the array
    do i=1, n
        print '(a,i3,x,a,i3,x,a)', "Row", i, "give", n, "elements." 
        do j=1, n
            read *, x
            a(i,j) = x
        end do
    end do

    !Transposing the matrix
    b=transpose(a)
    !Printing the transposed matrix (notice the column-major order)
    print *, "Printing the transposed matrix:"
    print *,b

end program p2