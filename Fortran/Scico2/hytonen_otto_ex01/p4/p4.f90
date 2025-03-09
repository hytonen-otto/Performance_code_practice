program testintkind 
  implicit none 
  integer :: i,ki
  do i=1,50 
     ki=selected_int_kind(i) 
     print *,i,ki
  end do
end program testintkind
