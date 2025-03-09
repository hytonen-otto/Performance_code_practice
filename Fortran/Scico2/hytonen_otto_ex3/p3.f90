program p3

    implicit none

    !Declaring the array
    integer, dimension(2,2,2,2,2,2,2) :: arr1
    
    !printing the desired information
    write(6,*) "Size of the array:", size(arr1)
    write(6,*) "Shape of the array:", shape(arr1)

end program p3