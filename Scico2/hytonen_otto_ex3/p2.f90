program p2

    implicit none

    real :: x = -1, y = 0

    !Establishing x and y as NaN using both described methods 
    x=sqrt(x)
    y=y/y

    write(6,*) "X=",x,"Y=",y

    !Demonstrating NaN is not equal to itself
    if (x/=x) then
        write(6,*) "Wow! It's NaN other than..."
    end if


end program p2