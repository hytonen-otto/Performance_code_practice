program p1

    implicit none

    ! declaring 10x10 array and filling it with 0's 
    !and 1's with the modulo function
    integer :: i,j, a(10,10)=reshape([((mod(i+j,2),i=1,10),j=1,10)], [10,10])
    !Printing the array, forcing 10 elements per row, no spaces
    print '(10i0)', a

end program p1