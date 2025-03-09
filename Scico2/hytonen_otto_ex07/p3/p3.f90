program p3

    !Defining the used module
    use cmd_line
    implicit none

    !Printing teh values of the fuctions
    print '(a,f12.8)','3th argument converted to different kind:  ',cmd2real(3)
    print '(a,i6)','2th argument converted to integer:',cmd2int(2)

end program p3