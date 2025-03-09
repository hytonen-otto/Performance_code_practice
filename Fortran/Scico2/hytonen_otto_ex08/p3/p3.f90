program p3

    !Defining the used module
    use cmd_line
    implicit none

    !Printing teh values of the fuctions
    print *,'1st argument converted to different kind:  ',cmd2real(1)
    print *,'2nd argument converted to integer:',cmd2int(2)

end program p3