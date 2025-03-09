module movement

    use boxes
    use constants
    use mtmod
    
    implicit none

contains 

    !Rotates box b in the list allbxs
    !Takes in integer, the index of the box moved
    !Returns bool=true if move accepted
    subroutine move_rotate(b,bool)
        implicit none 
        integer,intent(in) :: b
        logical,intent(out) :: bool
        type(box) :: tbox
        real(rk),dimension(4) :: xywh
        
        bool=.false.
        !Makes a duplicate box 'tbox' in a new place
        xywh=getxywh(allbxs(b)) 
        tbox = makebox(xywh(1),xywh(2),xywh(4),xywh(3))
        !Checking if the move overlaps with existing boxes
        !(Except b)
        if(.not.opt_overlap(tbox,b)) then 
            !Remember the position and boubox before move
            call remember_position(b,0)
            !The chosen box (index b) is the moved duplicate box
            allbxs(b)=tbox 
            !The return boolean
            bool=.true.
            !Sets a new boubox
            call opt_checkboundary(b)
        end if
    end subroutine

    !Swaps the places of 2 boxes
    !Takes in 2 integer, indexes of the boxes
    !Returns bool=true if move accepted
    !To undestand the structure check comments
    !in the move_rotate subroutine
    
    subroutine move_swap(b1,b2,bool)
        implicit none
        integer,intent(in) :: b1,b2
        logical,intent(out) :: bool
        type(box) :: tbox1, tbox2
        real(rk),dimension(4) :: xywh1, xywh2

        bool=.false.
        xywh1=getxywh(allbxs(b1))
        xywh2=getxywh(allbxs(b2))

        tbox1 = makebox(xywh2(1),xywh2(2),xywh1(3),xywh1(4))
        tbox2 = makebox(xywh1(1),xywh1(2),xywh2(3),xywh2(4))

        if(.not.opt_overlap(tbox2,b1).and..not.opt_overlap(tbox1,b2)) then
            call remember_position(b1,b2)
            allbxs(b1)=tbox1
            allbxs(b2)=tbox2
            bool=.true.
            call opt_checkboundary(b1)
            call opt_checkboundary(b2)
        end if
    end subroutine

    !Generates new coordinates for the actual move_move subroutine
    !Takes in the index of the box
    !Returns a box with  or y coordinates different form box b
    function generate_move_move(b)
        implicit none
        integer,intent(in) :: b
        type(box) :: generate_move_move
        integer :: xory
        real(rk) :: grand

        !Selecting movement direction
        xory=igrnd(1,2)
        !Selectiong the movement length
        grand=gaussrnd()
        !The box to be moved
        generate_move_move=allbxs(b)
        if(xory==1) then
            generate_move_move%x1=allbxs(b)%x1+grand
            generate_move_move%x2=allbxs(b)%x2+grand
        else if(xory==2) then
            generate_move_move%y1=allbxs(b)%y1+grand
            generate_move_move%y2=allbxs(b)%y2+grand
        end if
    end function

    !Moves a box in x or y direction by gaussian random value
    !Takes in the index of the box
    !Returns true if move accepted
    !To undestand the structure check comments
    !in the move_rotate subroutine
    
    subroutine move_move(b,bool)
        implicit none
        integer,intent(in) :: b
        logical,intent(out) :: bool
        type(box) :: tbox

        bool=.false.
        tbox = generate_move_move(b)

        if(.not.opt_overlap(tbox,b)) then
            call remember_position(b,0)
            allbxs(b)=tbox
            bool=.true.
            call opt_checkboundary(b)
        end if
    end subroutine

end module movement