program p5
    implicit none

    !Declaring h as kind 16 variable so we can see the differences caused by 
    !the precision of the 1/n used in the sum
    integer :: m=10**8,i
    real(kind=16) :: h=0

    write(6,*) "Iterations:", m
    
    !4 identical do loops with different parameters
    !inside the loop is the equation for the harmonic sum. real(i,x) gives 
    !the solution to 1/i in kind x
    do i=1,m 
        h=h+(1/real(i,4)) 
    end do 
    write(6,*) "Real kind  4:",h
    h=0

    do i=1,m 
        h=h+(1/real(i,8)) 
     end do 
     write(6,*) "Real kind  8:",h
     h=0

    do i=1,m 
        h=h+(1/real(i,10)) 
     end do 
     write(6,*) "Real kind 10:",h
     h=0

    do i=1,m 
        h=h+(1/real(i,16)) 
     end do 
     write(6,*) "Real kind 16:",h
     h=0

end program p5