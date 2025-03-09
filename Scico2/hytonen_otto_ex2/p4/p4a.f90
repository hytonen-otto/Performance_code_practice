program p4a
    implicit none 
    integer :: i,ki

    !Iterating selected_real_kind(i,5) to test what real kinds the compiler supports. 
    !i is the decimal precision and 5 is the exponent range
    do i=1,50 
       ki=selected_real_kind(i,5) 
       print *,i,ki
    end do

    
end program p4a
  

