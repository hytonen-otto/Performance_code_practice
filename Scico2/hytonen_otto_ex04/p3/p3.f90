program p3

    implicit none

    integer,parameter :: n=5
    real :: a(n,n)
  
    ! --- For the RNG
    integer :: s
    integer,allocatable :: seed(:)
  
    ! ---- Start of RNG setup
  
    ! Set up the seed
    call random_seed(size=s)
    allocate(seed(s))
    seed=1277345
    call random_seed(put=seed)
    ! Fill array a with random numbers
    ! in the interval [0,1[
    call random_number(a)
  
    ! ---- End of RNG setup

    ! Your additions come here.
    
    !Printing the initial array
    print '("The initial array:",/,25(''-''))' 
    print '(5f5.2)',a

    !Modifying the array according to the instructions
    where (a < 0.5)
        a = 0
    elsewhere 
    !if 0.5<=a<=0.75 -> a=0.75, if a<0.75 -> a=a
        a = max(a,0.75) 
    end where

    !Printing the new array
    print '(/,"Modified array:",/,25(''-''))'
    print '(5f5.2)',a

end program p3