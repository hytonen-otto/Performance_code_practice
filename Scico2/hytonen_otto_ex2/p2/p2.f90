program p2 
    implicit none

    !declarring the variables
    real :: x=2,y=3,z=4

    !Printing the results of the different calculation orders
    write(6,*) "x**y**z=", x**y**z
    write(6,*) "(x**y)**z=", (x**y)**z
    write(6,*) "x**(y**z)=",x**(y**z)
end program p2

