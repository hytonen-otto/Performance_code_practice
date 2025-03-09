module cmd_line

  implicit none
  integer,parameter :: rk=selected_real_kind(10,20)
  integer,parameter :: MAXBUF=200
  character(len=MAXBUF),private :: argu

contains

  !Real function of type real(rk) that reads the ith command line
  !argument, converts it to real(rk) and returns this value
  !If there is an error reading from command line, the function outputs
  !the largest real value of the kind 
  real(rk) function cmd2real(i)
    implicit none
    integer :: ios=0
    integer,intent(in) :: i
    call get_command_argument(i,argu)
    read(argu,*,iostat=ios) cmd2real
    if (ios /=0) then
      cmd2real=huge(cmd2real)
    end if
  end function cmd2real

  ! Fuction of type integer that reads the ith command line
  ! argument, converts it to integer and returns this value
  !If there is an error reading from command line, the function outputs
  !the largest integer of the kind
  integer function cmd2int(i)
    implicit none
    integer :: ios=0
    real :: in
    integer,intent(in) :: i
    call get_command_argument(i,argu)
    read(argu,*,iostat=ios) in
    cmd2int=nint(in)
    if (ios /=0) then
      cmd2int=huge(cmd2int)
    end if
  end function cmd2int

end module cmd_line