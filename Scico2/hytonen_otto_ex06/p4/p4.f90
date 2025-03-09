program p4

    implicit none

    integer :: i,ios,s
    !Assuming the input doesn't have to be allocatable
    integer :: arr(30)
    integer, allocatable :: arr2(:)
    character(len=80) :: filename

    filename = 'unformatted.dat'    
    arr = [34, 20, 24, 37, 38, 98, 54, 64, 85, 18, 98, 10, 45, 94, 10,&
    30, 56, 20, 81, 89, 70, 43, 81, 85, 70, 35, 19, 79, 7, 53]

    !Deleting the previous output file with the same name to avoid errors
    open(unit=1, iostat=ios, file=filename, status='old')
    if (ios == 0) close(1, status='delete')

    !Opening the unformatted file 
    open(unit=1,file=filename,iostat=ios,status='new',&
        access='stream',form='unformatted')
    if (ios/=0) then
        print '(a,a,5x,i0)','*** Error in opening file ',&
            trim(filename),ios
        stop
    end if
    !Writing the size of array at the beginning of the unformatted file
    write(1) size(arr)
    !Writing the array after the size
    do i=1,size(arr)
        write(1) arr(i)
    end do
    close(1)


    !Opening the file again
    open(unit=1,file=filename,iostat=ios,status='old',&
    access='stream',form='unformatted')
    if (ios/=0) then
        print '(a,a,5x,i0)','*** Error in opening file ',&
        trim(filename),ios
        stop
    end if
    
    !reading the size of the array and allocating space for it
    read(1,iostat=ios) s
    allocate(arr2(s))

    !reading the unformatted data to a new array
    i=1
    do
        read(1,iostat=ios) arr2(i)
        if (ios<0) exit
        i=i+1
    end do
    close(1)

    !Printing the read data
    print '(5I4)',arr2

end program p4