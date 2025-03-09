program p2

    implicit none

    !Using the function to print the first 9 values of the sequence
    print '(10i4)',tribonacci(9)


    
contains

!Input, integer n = number of calculated values in the sequence
!Output, integer array, the tribonacci sequence to n:th term
function tribonacci(n)
    integer, intent(in) :: n
    integer :: i
    integer, dimension(n) :: tribonacci

    !Assigning the given starting values
    tribonacci(1)=1
    tribonacci(2)=1
    tribonacci(3)=1

    !The definition of the tribonacci sequence
    do i=4,n
        tribonacci(i)=tribonacci(i-1)+tribonacci(i-2)+tribonacci(i-3)
    end do
end function tribonacci


end program p2