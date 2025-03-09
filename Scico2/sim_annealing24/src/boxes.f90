module constants
    implicit none
    integer,parameter :: ik=selected_int_kind(8), rk=selected_real_kind(10,40)
end module constants

module boxes
    use constants
    use mtmod

    implicit none
    type :: box
        real(rk) :: x1,y1,x2,y2 !Left lower and upper right corners
    end type box
    type(box) :: boubox !boundary box
    type(box),allocatable :: allbxs(:) !List of all boxes 
    integer,dimension(4),private :: boundaryindexes
    
    !VARIABLES FOR REVERTING MOVES 
    type(box),dimension(3) :: rememberpos !Stores the position before move
    !Stores the indexes of the remembered boxes
    integer,dimension(2),private :: rememberindex

    !####################################
    !Variables for optimizing overlap check
    real(rk),dimension(2) :: ovlgrid_origo, ovlsqr_size
    integer, allocatable :: ovl_list(:,:)
    integer,dimension(20) :: ovl_outside
    integer(ik) :: tot = 0,disag = 0
    !#####################################
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
        integer(ik) :: x,y,l,n
    
        !Allocates the array of all boxes
        allocate(allbxs(i))
        !The side length of the square to fit all boxes
        l=ceiling(i**0.5)
        !Making the boxes row at a time
        !########################
        n = (ceiling((size(allbxs)**0.5))/2)
        allocate(ovl_list((n**2),20))
        !########################
    
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

    !#################################
    !Finds the index of the first zero in the list
    function firststzeroind(list)
        implicit none
        integer(ik), intent(in) :: list(:)
        integer(ik) :: i,firststzeroind
        loop1: do i=1, size(list)
            if (list(i) == 0) then
                firststzeroind = i
                return 
            end if
        end do loop1
        !call resetovlgrid
    end function firststzeroind

    !Finds the index of the overlapbox in which the xy-coordinates are
    function findgridindex(x,y)
        implicit none
        real(rk), intent(in) :: x,y
        real (rk) :: xa, ya, xmax, ymax
        integer(ik) :: findgridindex
        xa = x-ovlgrid_origo(1)
        ya = y-ovlgrid_origo(2)
        xmax = boubox%x2-boubox%x1
        ymax = boubox%y2-boubox%y1
        if (xmax<xa .or. xa<0) then
            findgridindex = -1
            return
        else if (ymax<ya .or. ya<0) then
            findgridindex = -1
            return
        end if
        findgridindex = (floor(ya/ovlsqr_size(2)))*(int(size(ovl_list,1)**0.5))&
         + 1+floor(xa/ovlsqr_size(1))

    end function findgridindex

    !Takes in the index of a box, calculates overlaps for each of its corners
    subroutine addgridovl(i)
        implicit none
        integer(ik), intent(in) :: i
        integer(ik) :: j
        j = findgridindex(allbxs(i)%x1,allbxs(i)%y1)
        if (.not.any(ovl_list(j,:) == i)) then
            ovl_list(j,firststzeroind(ovl_list(j,:))) = i
        end if
        j = findgridindex(allbxs(i)%x1,allbxs(i)%y2)
        if (.not.any(ovl_list(j,:) == i)) then
            ovl_list(j,firststzeroind(ovl_list(j,:))) = i
        end if
        j = findgridindex(allbxs(i)%x2,allbxs(i)%y1)
        if (.not.any(ovl_list(j,:) == i)) then
            ovl_list(j,firststzeroind(ovl_list(j,:))) = i
        end if
        j = findgridindex(allbxs(i)%x2,allbxs(i)%y2)
        if (.not.any(ovl_list(j,:) == i)) then
            ovl_list(j,firststzeroind(ovl_list(j,:))) = i
        end if
    end subroutine


    !Calcuates a new overlapgrid
    subroutine resetovlgrid()
        implicit none 
        integer(ik) :: i,n        
        n = (ceiling((size(allbxs)**0.5))/2)
        ovl_list = 0
        call checkboundary()
        ovlgrid_origo = (/boubox%x1-0.001,boubox%y1-0.001/)
        ovlsqr_size = (/(boubox%x2-boubox%x1+0.01)/(n),(boubox%y2-boubox%y1+0.01)/(n)/)
        
        allbxs_loop: do i=1, size(allbxs)
            call addgridovl(i)
        end do allbxs_loop
    end subroutine
    !Does the actual overlap comparison
    logical function grid_loop(hbox,j,ignore)
        implicit none
        type(box),intent(in) :: hbox
        integer(ik),intent(in) :: j, ignore
        integer(ik) :: i
        type(box) ::  tbox
        if (j == -1) then
            grid_loop = overlap(hbox, ignore)
            return
        end if
        grid_loop=.false.
        loop1: do i=1, size(ovl_list(j,:))
            tbox=allbxs(ovl_list(j,i)) !only to make the if statement a bit shorter
            !Check if overlap and not the box to ignore
            if(tbox%x1<=hbox%x2.and.tbox%x2>=hbox%x1.and.&
            tbox%y2>=hbox%y1.and.tbox%y1<=hbox%y2.and.i/=ignore)then
                grid_loop=.true.
                exit loop1
            end if
        end do loop1
    end function

    !PROBLEMS WITH EVERY KIND OF MOVES

    !RAJATAPAUKSENA: OVERLAPGRIDIN PIENETYESSÄ LAATIKKO VOI 
    !PERIAATTEESSA OLLA YLI 4SSÄ GRIDIN RUUDUSSA
    !Is supposed to check the overlap
    logical function opt_overlap(hbox, ignore) 
        implicit none
        type(box),intent(in) :: hbox
        integer,intent(in) :: ignore
        integer(ik) :: j
        integer(ik), dimension(3) :: checkedsqr=0
        logical :: actual

        tot = tot+1
        call resetovlgrid()
        opt_overlap = .false.
        do while (opt_overlap.eqv..false.)
            j = findgridindex(hbox%x1,hbox%y1)
            checkedsqr(1)=j
            opt_overlap = grid_loop(hbox,j,ignore)

            j = findgridindex(hbox%x1,hbox%y2)
            if (.not.any(checkedsqr(:)==j)) then
                checkedsqr(2)=j
                opt_overlap = grid_loop(hbox,j,ignore)
            end if
            j = findgridindex(hbox%x2,hbox%y1)
            if (.not.any(checkedsqr(:)==j)) then
                checkedsqr(3)=j
                opt_overlap = grid_loop(hbox,j,ignore)
            end if
            j = findgridindex(hbox%x2,hbox%y2)
            if (.not.any(checkedsqr(:)==j)) then
                opt_overlap = grid_loop(hbox,j,ignore)
            end if
            exit
        end do
        actual = overlap(hbox,ignore)
        if (actual.neqv.opt_overlap) then
            opt_overlap = actual
            disag = disag+1
        end if
    end function
    !################################


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
        implicit none
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
            tbox=allbxs(i) !only to make the if statement a bit shorter
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