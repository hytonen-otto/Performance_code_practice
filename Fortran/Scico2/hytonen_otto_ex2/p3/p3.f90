program p3
    implicit none

    !Declaring variables 
    integer :: i,n=0

    !Using the same loop with different iteration variables.
    !Counting the number of iterations with variable n
    !Print the number of iterations and return n to 0
    do i=1,5
        n=n+1
    end do
    write(6,*) 'Iterations: ', n
    n=0

    do i=5,0,-1
        n=n+1
    end do
    write(6,*) 'Iterations: ', n
    n=0
    
    do i=10,1,-2
        n=n+1
    end do
    write(6,*) 'Iterations: ', n
    n=0
    do i=0,30,7
        n=n+1
    end do

    write(6,*) 'Iterations: ', n
end program p3