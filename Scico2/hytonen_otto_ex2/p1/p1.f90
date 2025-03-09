program p1
    implicit none

    integer :: i,n

    !asking for and reading an input
    write(6,*) "Give an integer n:"
    read(5,*) n

    !do-loop to print the desired output
    write(6,*) "Printing squares of all integers from 2 to n:"
    do i=2,n,1
        write(6,*) i**2
    end do
end program p1