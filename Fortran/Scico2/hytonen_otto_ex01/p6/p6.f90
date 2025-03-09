program avgspeed
    implicit none

    !Establishing the variables
    integer :: distance
    real ::  time, h_time, km_distance
    !Ask user inputs for distance and time
    write(6,*) "Give a distance (in meters):"
    read(5,*) distance
    write(6,*) "Give the elapsed time (in seconds):"
    read(5,*) time
    !Convert to better units
    h_time=time/(60*60)
    km_distance=0.001*distance
    !Calculate and print the average speed.
    write(6,*) "The average speed was ", km_distance/h_time, "km/h"
end program avgspeed
