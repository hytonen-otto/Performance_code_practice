
!
! Solution to problem 6 of exercise 4.
!
! Matrix p and initial vector x vector readin from stdin.
! Example of input (remember the implicit type conversion from integer to real):
!     1 3 2 4 7 2 3 4 2 6 4 2 7 8 4 2
!     2 3 5 2
!
! Iteration count is given on command line.
! It is best to write the input into a file and redirect sttdin to it when running:
!     ./a.out 100 < input.dat
!

program p1
    implicit none
    integer,parameter :: rk=selected_real_kind(10,40) ! This gives double precision
    integer :: i,iter
    real(rk),allocatable :: p(:,:),copyp(:,:),x(:),copyx(:)
    character(len=80) :: arg,fmt='(i6,3x,100f8.4)'
  
    ! Size of the matrix. Fixed in the code. Not good coding style.
    integer,parameter :: n=4
    
    ! Iteration count given on command line.
    call get_command_argument(1,arg); read(arg,*) iter
  
    allocate(p(n,n),copyp(n,n),x(n))
    read *,p
    read *,x
    x=x/sum(x) ! Normalize x
    call normalize_columns(p,n) ! Normalize p
    copyp = p   !Copies of p and x. These don't change during the iteration.
    copyx = x

    print *,'Printing the number of iterations and vector x:'
    print '(47(''-''))'
    print fmt,0,x

    !printing the first iteration
    x=matmul(p,copyx)
    print fmt,1,x

    !calculating the powers of p and mutiplying each of them with x 
    do i=2,iter
        p=matmul(p,copyp)
        x=matmul(p,copyx)
        print fmt,i,x
    end do

    !Printing p and x after the iterations
    print '(/,a,i5,a)','Printing the matrix p after ',iter,' iterations.'
    print '(4f7.4)',p

    print '(/,a,i5,a)','Printing the vector x after ',iter,' iterations.'
    print '(4f7.4)',x

    contains
  
    subroutine normalize_columns(a,n)
        implicit none
        integer :: n
        real(rk) :: a(n,n)
        integer :: i
        do i=1,n
            a(:,i)=a(:,i)/sum(a(:,i))
        end do
        return
    end subroutine normalize_columns
   
end program p1