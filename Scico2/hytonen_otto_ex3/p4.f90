program p4

    implicit none

    !declaring the array
    integer, dimension(2,2) :: arr1

    !Filling the array
    arr1(1,1) = 1
    arr1(2,1) = 2
    arr1(1,2) = 3
    arr1(2,2) = 4

    !printing the array
    write(6,*) "Array:",arr1
    !replacing array elements with their squares
    arr1 = arr1**2
    !printing the squared array
    write(6,*) "Array squared:",arr1

end program p4