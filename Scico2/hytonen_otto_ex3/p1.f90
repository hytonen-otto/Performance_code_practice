program p1
    
    USE ieee_arithmetic

    implicit none

    real :: x=1,y=1,z=0

    !Equations to produce positive and negative infinity by calculating 1/0
    x=x/z
    y=-y/z

    write(6,*) "x:", x
    write(6,*) "y:", y

    write(6,*) "inf*(-inf)=", x*y
    write(6,*) "(-inf)*(-inf)=", y*y
end program p1
