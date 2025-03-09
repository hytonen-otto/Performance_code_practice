module cmd_line

  implicit none
  integer,parameter :: rk=selected_real_kind(10,20)
  integer,parameter :: MAXBUF=200
  character(len=MAXBUF),private :: argu
  
contains
  
  !Real function of type real(rk) that reads the ith command line
  !argument, converts it to real(rk) and returns this value
  real(kind=rk) function cmd2real(i) 
    integer, intent(in) :: i
    real(kind=rk) :: r
    call get_command_argument(i,argu)
    read(argu,*) r
    cmd2real = r
  end function cmd2real
    
  ! Fuction of type integer that reads the ith command line
  ! argument, converts it to integer and returns this value
  integer function cmd2int(i) 
    integer, intent(in) :: i
    real :: r
    integer :: j
    call get_command_argument(i,argu)
    read(argu,*) r
    j=nint(r)
    cmd2int = j
  end function cmd2int

end module cmd_line
  