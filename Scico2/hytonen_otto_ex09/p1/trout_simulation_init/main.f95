program main
use trout_population_functions
implicit none


integer,parameter :: kind= selected_real_kind(10,40)
integer(kind) ::n,i,f
real(kind) ::pi,xxx,population, food
character(len = 80) :: arg



!Program requires input from command line
! Try these values
! ./a.out 100000 10000 23.3 10

call get_command_argument(1,arg)  !Number of iterations
	write(*,*) trim(arg)
	read(arg,*) n           

call get_command_argument(2,arg)  !Initial trout population
	write(*,*) trim(arg)
	read(arg,*) xxx  

call get_command_argument(3,arg)  !Amount of food given trouts each year
	write(*,*) trim(arg)
	read(arg,*) food  

call get_command_argument(4,arg)  !Number of fishers each year
	write(*,*) trim(arg)
	read(arg,*) f  




! write results to a file and start the iterations
open(unit=1,file='output.dat',form='formatted',status='old')


!The loop calls the x1 subroutine to calculate the values of the parameters 
!after each iteration
!It writes the population value to the file 'output.dat'
!If the population reaches 0, it end the iteration and prints a message
do i=1,n
	call x1(xxx,food,f,population)
	pi = population
	write(1,'(i0,a,i0)') i,' ',nint(population)

	if(nint(population) <= 0) then
		print*,'All the trouts died!  :('
		
		exit 
	endif
	xxx = pi
end do

close(unit=1)



end program main
