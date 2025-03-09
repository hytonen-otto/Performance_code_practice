program avgspeed
    implicit none

    !Let's establish the variables!
    integer :: distance=100
    real ::  time=9.58, h_time, km_distance
    !Convert the variables to better units
    h_time=time/(60*60)
    km_distance=0.001*distance
    !Calculate and print the average speed.
    write(6,*) km_distance/h_time, "km/h"
end program avgspeed




