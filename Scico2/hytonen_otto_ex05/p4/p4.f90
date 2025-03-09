program p4

    implicit none

    integer :: i
    character(len=80) :: c
    real :: a

    !Taking in command line arguments
    do i=1,command_argument_count()
       call get_command_argument(i,c)
       !converting c to real
       read(c,*) a
       !printing the powers of the real (limited length of a for cleaner output)
       print '(6f10.4)', [(a**i,i=1,6)]
    end do

end program p4