program p2
    implicit none
    integer,parameter :: n=4
    integer :: i,j,b(n),c(n,n),d(n,n)
   
    ! ---------- Do not change these loops 
    do i=1,n
       b(i)=i
       do j=1,n
          c(j,i)=10*i+j
       end do
    end do
    ! ----------

    !Array d = array c+1 in each element
    d(1:n,1:n)=c(1:n,1:n)+1
  
    !Begining from index (2,2) to (n,n), array c = array d 
    ! from index (1,1) to (n-1,n-1)
    c(2:n,2:n)=d(1:n-1,1:n-1)

    !Array c odd indices from (1,1) to (n,n) are multiplied by 10 
    c(1:n:2,1:n:2)=10*c(1:n:2,1:n:2)
    
    ! Intrinsic function sum() gives the sum of all array elements.
    ! Check that the results printed here is the same as in your modified version.
    print *, 'answer=',sum(c)
  
end program p2
  