program randomnumber
  implicit none
  integer,allocatable :: seed(:)      ! RNG seed array
  real,allocatable :: r(:)            ! Array to store generated RNs
  integer :: nseed,n
  character(len=80) :: argu

  ! --- Check that command line is right
  if (command_argument_count()/=2) then
     call get_command_argument(0,argu)
     write(0,'(/,a,/)') 'usage: '//trim(argu)//' nr seed'
     stop
  end if

  ! --- Get the number of generated RNs (n) and seed (array seed)
  !     and initilize RNG
  call get_command_argument(1,argu)
  read(argu,*) n
  allocate(r(n))
  call random_seed(size=nseed)
  allocate(seed(nseed))
  call get_command_argument(2,argu)
  read(argu,*) seed(1)
  seed=seed(1)
  call random_seed(put=seed)

  ! --- Put n RN's to array r (uniformly distributed in [0,1[)
  call random_number(r)

  !print *,r
  write(10,'(g20.10)') r
end program randomnumber

  
