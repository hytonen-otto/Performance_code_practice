program p6

    implicit none

    integer :: s,i
    !Declaring the array, CHANGE THE KIND HERE  
    integer(kind=8), dimension(10**7) :: a


    write(6,*) "Kind = 8"
    !Filling the array with numbers 1 to 10^7
    do i=1, size(a)
        !The mod function makes sure there is no integer overflow problem
        a(i)=mod(i,huge(a))
    end do
    !Calculating the sum of 1000 sums of array a to increase computing time
    do i=1, 1000
        s=s+sum(a)
    end do

    !Printing the sum to make sure the compiler doesn't skip the calculations.
    write(6,*) s 
end program p6