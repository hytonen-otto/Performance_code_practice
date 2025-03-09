program p3

    implicit none

    integer :: i,j,k(100),ios
    real :: x(100)
    character(len=80) :: filename_from,filename_to

    !Getting the filenames from command line
    call get_command_argument(1,filename_from)
    call get_command_argument(2,filename_to)
    !Opening the file
    open(unit=1,file=filename_from,iostat=ios,status='old')
    if (ios/=0) then
        print '(a,a,5x,i0)','*** Error in opening file ',trim(filename_from),ios
        stop
    end if
    !Using do loop to read the file until the end of file is reached (ios<0)
    i=1
    do
        read(1,*,iostat=ios) x(i)
        if (ios<0) exit
        i=i+1
        end do
    close(1)
    i=i-1

    !Rounding the input values to the nearest integer
    k=nint(x)

    !Deleting the previous output file with the same name to avoid errors
    open(unit=2, iostat=ios, file=filename_to, status='old')
    if (ios == 0) close(2, status='delete')

    !Opening to the output file
    open(unit=3,file=filename_to,iostat=ios,status='new')
    if (ios/=0) then
        print '(a,a)','*** Error in opening file ',trim(filename_to)
        stop
    end if
    !Writing to the output file
    do j=1,i
        write(3,'(i3)') k(j)
    end do
    close(unit=3,status='keep')

end program p3