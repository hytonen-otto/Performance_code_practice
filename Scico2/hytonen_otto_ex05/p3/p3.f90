program p3

    implicit none

    real :: inch(25), mm(25)
    integer :: n

    !Loop to fill array with the inch values
    do n=0, 24
        inch(n+1)=n*0.5
    end do
    !Converting and filling the mm array
    mm(1:25)=inch(1:25)*25.4    

    !printing the table row by row
    do n=1, 25
        print '(f5.1,a,f5.1,a)',inch(n),' inch = ',mm(n),' mm'
    end do
end program p3