program p4

    implicit none

    integer :: i,x, a(2), b(2), c(2),d(2), e(2), table(2,5)
    !Asking input for 2 integers per array
    !There is propably a way to cycle through the arrays a to e
    print *, "Give 2 integers:"
    do i=1,2
        read *,x
        a(i) = x
    end do

    print *, "Give 2 integers:"
    do i=1,2
        read *,x
        b(i) = x
    end do

    print *, "Give 2 integers:"
    do i=1,2
        read *,x
        c(i) = x
    end do

    print *, "Give 2 integers:"
    do i=1,2
        read *,x
        d(i) = x
    end do

    print *, "Give 2 integers:"
    do i=1,2
        read *,x
        e(i) = x
    end do

    !Storing the small arrays into a bigger array
    table(:,1) = a 
    table(:,2) = b
    table(:,3) = c
    table(:,4) = d 
    table(:,5) = e

    print *, "Printing the 2x5 array:"
    !Printing the array row by row
    write(6,*) table(1,:)
    write(6,*) table(2,:)

end program p4