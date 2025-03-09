program p2

    implicit none

    integer :: i, ios
    character(len=80) :: filename,line
    
    !Getting the filename from command line
    call get_command_argument(1,filename)
    !Opening the file
    open(unit=1,file=filename,iostat=ios,status='old')
    if (ios/=0) then
        print '(a,a,5x,i0)','*** Error in opening file ',trim(filename),ios
        stop
    end if
    !Using do loop to read the file until the end of file is reached (ios<0)
    i=1
    do
        read(1,'(a)',iostat=ios) line
        if (ios<0) exit
        i=i+1
        !print *,line
        end do
    close(1)
    i=i-1
    
    print '(a,i0)','Number of lines: ', i
end program p2