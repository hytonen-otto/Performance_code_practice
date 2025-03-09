program p6

    implicit none

    integer :: i,k
    integer :: histogram(20)
    !Dividing the span of possible values to 20 bins
    real ::step=(exp(1.0)-1)/20 
    real :: numbers(100000)

    !Reading the random numbers from input file
    read *,numbers

    !do loop going through each of the 20 bins and counting the
    !number of random numbers in eah bin with the count function
    !The numbers are saved in array 'histogram'
    do i=1,20
        histogram(i)=count(numbers>=(1+(i-1)*step).and.&
        numbers<=(1+(i)*step))
    end do 

    print*,'Values for the histogram:'
    print'(10i6)',histogram
    print*,''
    !Printing a hostogram of the data in the 
    !most impractical way possible
    !--------------------------------------------------
    print*, 'Histogram of exp(u) generated with 100 000'
    print*,'random uniform values of u between [0,1)'
    print*,''
    do i=18,-1,-1
        write(*,'(i5)',advance='no') (i+1)*500
        do k=1, 20
            if(nint(histogram(k)/500.0)>i) then
                write(*,'(1x,a)',advance='no') '*'
            end if
        end do
        print *, ''
    end do
    print*,'     1      1.34      1.77      2.20       e'
    !-----------------------------------------------------------
end program 