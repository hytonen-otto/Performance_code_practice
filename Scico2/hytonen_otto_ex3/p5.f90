program p5

    implicit none
    integer :: i
    !Declaring array with 3 rows and 4 columns
    integer, dimension(3,4) :: arr1

    !Filling the array row by row
    do i=1,4
        arr1(1,i)=i
    end do

    do i=1,4
        arr1(2,i)=i+4
    end do

    do i=1,4
        arr1(3,i)=-2
    end do

    !Printing the array row by row
    write(6,*) arr1(1,:)
    write(6,*) arr1(2,:)
    write(6,*) arr1(3,:)

end program p5