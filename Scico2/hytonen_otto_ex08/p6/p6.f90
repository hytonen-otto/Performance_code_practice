program p6
    implicit none
    real :: x1=15,x2=60, x3=200
    intrinsic :: sin,cos,tan
  
    !Printing the test values
    print *,'sin(',x1,') : ', trig_degree(sin,x1)
    print *,'cos(',x2,') : ', trig_degree(cos,x2)
    print *,'tan(',x3,') : ', trig_degree(tan,x3)
  
contains  
    !Input external f, external function trig function wanted
    !Input real x, angle in degrees 
    !Output real, the value of f(x)
    real function trig_degree(f,x)
        implicit none
        real,parameter :: deg2rad=2*3.1415927/360
        real,external :: f
        real,intent(in) :: x
        trig_degree=f(deg2rad*x)
    end function trig_degree
end program p6