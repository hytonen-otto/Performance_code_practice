program p5

    implicit none

    integer :: i, sz
    character(len=80) :: c

    print '(a,12x,a)','File:','Size:'
    !Taking in command line arguments
    do i=1,command_argument_count()
       call get_command_argument(i,c)
       !Function to get the size of the file
       inquire(file=c,size=sz)
       !Printing the name of the file and its size
       print '(a10,7x,i6,a)',trim(c), sz, ' bytes'
    end do

end program p5