program numbers
  implicit none
  integer :: a=5, b=3
  real :: x=2.2, y=3.5
  
  write(6,*) (a+b)/3.0_16
  write(6,*) (a+b)/3
  write(6,*) (x+y)/3.0
  write(6,*) (x+y)/3
  
end program numbers
