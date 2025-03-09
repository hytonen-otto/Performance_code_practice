module series

contains
    !Input real start, coeff, values for the geom series
    !Input integer n, number of elements in the series
    !Output allocatable array, the geom. series: start*coeff*i, (i=1 -> i=n)
    function geomseries(start,coeff,n)
        implicit none
        integer, intent(in) :: n
        real,intent(in) :: start, coeff
        integer :: i
        real :: r
        real,allocatable :: geomseries(:)

        r=start
        !Allocating the needed size of the array
        allocate(geomseries(n))
        do i=1,n 
            geomseries(i)=r
            r=coeff*r
        end do

    end function geomseries

end module series