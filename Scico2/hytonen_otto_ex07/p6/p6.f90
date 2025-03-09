program p6

    implicit none
    integer,parameter :: rk=selected_real_kind(10,40) ! This gives double precision
    integer :: i,power
    real(rk),allocatable :: z(:,:),copyz(:,:),f(:,:)
    character(len=80) :: arg
  
    ! Size of the matrix. Fixed in the code. Not good coding style.
    integer,parameter :: n=4
    
    ! Iteration count given on command line.
    call get_command_argument(1,arg); read(arg,*) power
  
    !Allocating the used matrices and reading the matrix from a file 
    allocate(z(n,n),copyz(n,n),f(n,n))
    read *,z
    copyz=z

    !The algorithm
    !----------------------------------------------
    i=0
    do while (btest(power,i).eqv..false.)
        z=matmul(z,copyz)
        copyz=z
        i=i+1
    end do
    f=z
    i=i+1
    do i=i, bit_size(power)-1
        z=matmul(z,copyz)
        copyz=z
        if (btest(power,i).eqv..true.) then
            f=matmul(f,z)
        end if
    end do
    !-------------------------------------------------

    !Printing the final matrix
    print '(a,i3,a)','Printing the ', power, 'th power of the matrix.'
    print '(4f10.4)',f

end program p6