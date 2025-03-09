module inout

    use boxes
    use constants

    implicit none
    real(rk) :: c,a
    integer(ik) :: n,iarg
    character(len=80) :: filename_to1, filename_to2,filename_to3,arg

contains

    !Reads in the command line arguments
    !Prints an error message if wrong number of command line arguments
    subroutine read_cmd_line()
        iarg=command_argument_count()
        !The error statement
        if (iarg/=6) then
            call get_command_argument(0,arg)
            write(0,'(a,a,a)') &
                'usage: ',trim(arg),' c, a, n, outputfile 1, outputfile 2, outputfile 3'
            stop
        end if

        !Reading in every argument
        call get_command_argument(1,arg)
        read(arg,*) c
        call get_command_argument(2,arg)
        read(arg,*) a
        call get_command_argument(3,arg)
        read(arg,*) n
        call get_command_argument(4,arg)
        read(arg,*) filename_to1
        call get_command_argument(5,arg)
        read(arg,*) filename_to2
        call get_command_argument(6,arg)
        read(arg,*) filename_to3
    end subroutine

    !Deletes old file with the same name and opens new for writing
    subroutine delete_and_openfile(filename, unit)
        implicit none
        integer,intent(in) :: unit
        character(len=80),intent(in) :: filename
        integer :: ios

        !Deleting the previous output file with the same name to avoid errors
        !MODIFY TO GIVE ERROR INSTEAD
        open(unit=unit, iostat=ios, file=filename, status='old')
        if (ios == 0) close(2, status='delete')
        
        !Opening to the output file
        open(unit=unit,file=filename,iostat=ios,status='new')
        if (ios/=0) then
            print '(a,a)','*** Error in opening file ',trim(filename)
            stop
        end if
    end subroutine 

    !Literally just a beautiful shell for the "close" line
    !Purpose is to make the algorithm module more reeadable
    subroutine closefile(unit)
        implicit none
        integer,intent(in) :: unit
        close(unit=unit,status='keep')
    end subroutine

    !Writes the iterations and footprint
    !MUST OPEN AND CLOSE THE FILE SEPARATELY OUTSIDE THIS SUBROUTINE
    subroutine writefootprint(unit,iterations)
        implicit none
        integer,intent(in) :: unit,iterations

        write(unit,'(i9,a1,f10.5)') iterations,',', area(boubox)
    end subroutine


    !Subprograms for drawing the positions of the boxes 
    !-----------------------------------------------

    !Moves a box so that the lower left corner of boundarybox is at (0,0)
    function normalize(hbox)
        implicit none
        type(box) :: hbox
        type(box) :: normalize
        real(rk) :: x,y

        x=boubox%x1
        y=boubox%y1
        normalize%x1=hbox%x1-x
        normalize%x2=hbox%x2-x
        normalize%y1=hbox%y1-y
        normalize%y2=hbox%y2-y
    end function

    !Writes coordinates of one box for drawing box outlines
    subroutine writeboxcoord(unit, hbox)
        implicit none
        integer,intent(in) :: unit
        type(box),intent(in) :: hbox

        write(unit, '(2(f7.3,2x))') hbox%x1,hbox%y1
        write(unit, '(2(f7.3,2x))') hbox%x2,hbox%y1
        write(unit, '(2(f7.3,2x))') hbox%x2,hbox%y2
        write(unit, '(2(f7.3,2x))') hbox%x1,hbox%y2
        write(unit, '(2(f7.3,2x))') hbox%x1,hbox%y1
        write(unit, '(a4)') 'next'
    end subroutine

    !Writes coordinates of the box for drawing solid color rectangles
    subroutine var_writeboxcoord(unit, hbox)
        implicit none
        integer,intent(in) :: unit
        type(box),intent(in) :: hbox

        write(unit, '(a9,2x,i3,2x,4(f7.3))') 'RECTANGLE',igrnd(16,143),hbox%x1,hbox%y1,hbox%x2,hbox%y2
    end subroutine
    
    !Writes all positions
    subroutine writepositions(filename,unit)
        implicit none
        integer,intent(in) :: unit
        integer(ik) :: i
        character(len=80),intent(in) :: filename

        call delete_and_openfile(filename,unit)

        write(unit,'(a)') 'thickness = 2.5' 
        do i=1, size(allbxs)
            call writeboxcoord(unit,normalize(allbxs(i)))
            !call var_writeboxcoord(unit,normalize(allbxs(i)))
        end do

        write(unit,'(a)') 'color = 3'
        call writeboxcoord(unit,normalize(boubox))
        
        call closefile(unit)
    end subroutine

end module