program p4b
    implicit none

    !Declaring the integer i (and j) to be big enough to store numbers 
    !from -10^2 to 10^2  i.e. kind 1 integers
    integer, parameter :: ik=selected_int_kind(2)
    integer (kind=ik) :: i=1,j

    !Using a do-loop to increase and print the variable i to test 
    !what happens when it becames too big to fit the kind 1.
    do j=1,10
        i=3*i
        print *,j,i     
    end do
end program p4b
