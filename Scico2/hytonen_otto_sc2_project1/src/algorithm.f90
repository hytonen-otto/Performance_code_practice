module algorithm
    use mtmod
    use boxes
    use constants
    use movement
    use inout

    implicit none 
    integer(ik) :: seed

contains
    !Initializes the random number generator
    subroutine new_rndgenerator()
        seed=getseed()                                          
        call sgrnd(seed)
    end subroutine

    !Makes a individual (accepted) move
    !The do loop works until a move is accepted
    subroutine move
        implicit none
        integer :: no, b1,b2
        logical :: acc_move

        acc_move=.false.
        do while (acc_move.eqv..false.)
            no=igrnd(1,3)
            b1 = igrnd(1,size(allbxs))
            if(no==1) then
                call move_rotate(b1,acc_move)
            else if(no==2) then
                b2 = igrnd(1,size(allbxs))
                call move_swap(b1,b2,acc_move)
            else if(no==3) then
                call move_move(b1,acc_move)
            end if
        end do
    end subroutine move

    !Combines all the subroutines to run the algorithm
    !Takes in 2 reals, control parameter c and cooling
    !parameter a
    subroutine sa_algorithm(c,a)
        implicit none
        real(rk),intent(in) :: c,a
        integer :: i,j,n, iter_scalar
        real(rk) :: d

        n=size(allbxs)
        call delete_and_openfile(filename_to3,2)

        iter_scalar=400 !Scales iterations with the number of boxes
        d=c
        do j=1,60
            call writefootprint(2,iter_scalar*(j-1))
        do i=1, iter_scalar*n
            call move
            !The accept condition equation
            if(grnd()>exp(-(area(boubox)-area(rememberpos(3)))/d)) then
                call restore_position()
            end if
        end do
        d=a*d  !Decreases the control parameter 
        end do
        call closefile(2)
    end subroutine
end module algorithm