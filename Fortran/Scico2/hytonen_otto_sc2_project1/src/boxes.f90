module constants
    implicit none
    integer,parameter :: ik=selected_int_kind(8), rk=selected_real_kind(10,40)
end module constants

module boxes
    use constants
    use mtmod

    implicit none
    type :: box
        real(rk) :: x1,y1,x2,y2 !Left lower and right upper corners
    end type box
    type(box) :: boubox !boundary box
    type(box),allocatable :: allbxs(:) !List of all boxes 
    integer,dimension(4),private :: boundaryindexes
    
    !VARIABLES FOR REVERTING MOVES 
    type(box),dimension(3) :: rememberpos !Stores the position before move
    !Stores the indexes of the remembered boxes
    integer,dimension(2),private :: rememberindex 

contains

    !Makes a box. The center is at x,y, h=height, w=width
    function makebox(x,y,w,h)
        implicit none
        real(rk),intent(in) :: x,y,w,h
        type(box) :: makebox
        makebox=box(x-w/2,y-h/2,x+w/2,y+h/2)
    end function makebox

    !Takes in the number of boxes, returns array of boxes placed
    !so far apart (1  from center to center) that 
    !max sized boxes (1x1) don't overlap
    subroutine setinitialstate(i)
        implicit none
        integer,intent(in) :: i
        integer(ik) :: x,y,l
    
        !Allocates the array of all boxes
        allocate(allbxs(i))
        !The side length of the square to fit all boxes
        l=ceiling(i**0.5)
        !Making the boxes row at a time
        do y=1,l
            do x=1,l
            if ((l*(y-1)+x)>i) exit !exit after i number of boxes
            !Make a random sized box (max 1x1) at the center of 1x1 square
            allbxs((y-1)*l+x)=makebox((x-0.5d0),(y-0.5d0),grnd(),grnd())
            end do
        end do
        !Making the first boubox
        call checkboundary
        !Storing the indexes of the boxes defining the boubox
        boundaryindexes = [minloc(allbxs(:)%x1),minloc(allbxs(:)%y1),&
        maxloc(allbxs(:)%x2),maxloc(allbxs(:)%y2)]
    end subroutine

    !Updates the boundary after a move
    !Inefficient used only when the optimized version does not work
    subroutine checkboundary()
        implicit none
        boubox=box(minval(allbxs(:)%x1),minval(allbxs(:)%y1),&
        maxval(allbxs(:)%x2),maxval(allbxs(:)%y2))
    end subroutine


    !More efficient way to check the boundary
    !Goes through all the boxes only if the moved box was one
    !of the boxes defining the boundarybox
    !Program runs at least 25% faster
    subroutine opt_checkboundary(i)
        integer(ik),intent(in) :: i

        !If the moved box defined the boundary, use the inefficient method
        if(any(boundaryindexes==i)) then
            call checkboundary
            boundaryindexes = [minloc(allbxs(:)%x1),minloc(allbxs(:)%y1),&
            maxloc(allbxs(:)%x2),maxloc(allbxs(:)%y2)]
            return
        !Otherwise only check if the moved box increases the boundary
        else
            if(boubox%x1>allbxs(i)%x1) then
                boubox%x1=allbxs(i)%x1
                boundaryindexes(1)=i
            end if
            if(boubox%y1>allbxs(i)%y1) then
                boubox%y1=allbxs(i)%y1
                boundaryindexes(2)=i
            end if
            if(boubox%x2<allbxs(i)%x2) then
                boubox%x2=allbxs(i)%x2
                boundaryindexes(3)=i
            end if
            if(boubox%y2<allbxs(i)%y2) then
                boubox%y2=allbxs(i)%y2
                boundaryindexes(4)=i
            end if
        end if
    end subroutine
    
    !Checks for box overlap, false if no overlap
    logical function overlap(hbox,ignore)
        implicit none 
        type(box),intent(in) :: hbox
        integer,intent(in) :: ignore
        type(box) ::  tbox
        integer(ik) :: i
        overlap = .false.
        allbxs_loop: do i=1, size(allbxs)
            tbox=allbxs(i) !only ty make the if statement a bit shorter
            !Check if overlap and not the box to ignore
            if(tbox%x1<=hbox%x2.and.tbox%x2>=hbox%x1.and.&
            tbox%y2>=hbox%y1.and.tbox%y1<=hbox%y2.and.i/=ignore)then
                overlap=.true.
                exit allbxs_loop
            end if
        end do allbxs_loop
    end function

    !returns the xywh form (xy=center, w=width, h=height) of a box 
    !xywh is useful when moving a box
    function getxywh(hbox)
        implicit none
        type(box),intent(in) ::hbox
        real(rk),dimension(4) :: getxywh
        getxywh(1)=(hbox%x2+hbox%x1)/2
        getxywh(2)=(hbox%y2+hbox%y1)/2
        getxywh(3)=hbox%x2-hbox%x1
        getxywh(4)=hbox%y2-hbox%y1
    end function

    !Remebers the position of the box(es) and boundarybox
    !in case the move is not accepted.
    !GIVE b2=0 if only 1 box moves
    subroutine remember_position(b1,b2)
        implicit none
        integer,intent(in) :: b1,b2
        rememberindex=[b1,b2]
        if(b2==0)then
            rememberpos(1)=allbxs(b1)
            rememberpos(3)=boubox
        else 
            rememberpos(1)=allbxs(b1)
            rememberpos(2)=allbxs(b2)
            rememberpos(3)=boubox
        end if
    end subroutine

    !Restores the position that was before the move
    subroutine restore_position()
        implicit none
        integer :: b1, b2
        b1=rememberindex(1)
        b2=rememberindex(2)
        if (b2==0) then !one box was moved
            allbxs(b1)=rememberpos(1)
            boubox=rememberpos(3)
        else !Two boxes moved
            allbxs(b1)=rememberpos(1)
            allbxs(b2)=rememberpos(2)
            boubox=rememberpos(3)
        end if
    end subroutine
    
    !Returns the area of a box
    real(rk) function area(hbox)
        implicit none
        type(box),intent(in) :: hbox
        area=(hbox%x2-hbox%x1)*(hbox%y2-hbox%y1)
    end function    

    !Area of all the boxes combined
    real(rk) function boxesarea()
    implicit none    
    integer :: i
        do i=1, size(allbxs)
            boxesarea=boxesarea+area(allbxs(i))
        end do
    end function    

end module boxes