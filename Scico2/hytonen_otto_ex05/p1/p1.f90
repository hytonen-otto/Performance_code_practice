program p1

    implicit none

    !Declaring the arrays and filling the with 0's
    integer :: a(4)=0, b(3,3)=0, c(4,4)=0

    !Using array slices to access the right 
    !elemaents and changing them to 1
    a(3:4) = 1
    b(1:3:2,1:3:2)=1
    c(2:3,2:3)=1

    !Printing the arrays
    print '(/,''A '',20(''-''))'
    print '(4i4)',a
    print '(/,''B '',20(''-''))'
    print '(3i4)',b
    print '(/,''C '',20(''-''))'
    print '(4i4)',c

end program p1