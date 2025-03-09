program main
use trout_population_functions
implicit none


integer,parameter :: kind= selected_real_kind(10,40)
integer(kind) ::n,i,fishermen,ios,iarg
real(kind) ::population, food
character(len = 80) :: arg



!Program requires input from command line
! Try these values
! ./a.out 100000 10000 23.3 10

iarg=command_argument_count()
if (iarg/=4) then
call get_command_argument(0,arg)
write(0,'(a,a,a)') &
'usage: ',trim(arg),' i x'
stop
end if

call get_command_argument(1,arg)  !Number of iterations
	arg=trim(arg)
	read(arg,*) n           

call get_command_argument(2,arg)  !Initial trout population
	arg=trim(arg)
	read(arg,*) population  

call get_command_argument(3,arg)  !Amount of food given trouts each year
	arg=trim(arg)
	read(arg,*) food  

call get_command_argument(4,arg)  !Number of fishers each year
	arg=trim(arg)
	read(arg,*) fishermen




! write results to a file and start the iterations
open(unit=1,file='output.dat',iostat=ios,form='formatted',status='old')

if (ios/=0) then
	print '(a)','*** Error in opening file output.dat'
	stop
end if


!The loop calls the new_population subroutine to calculate the values of the parameters 
!after each iteration
!It writes the population value to the file 'output.dat'
!If the population reaches 0, it end the iteration and prints a message
do i=1,n
	call new_population(population,food,fishermen,population)
	write(1,'(i0,a,i0)') i,' ',nint(population)

	if(nint(population) <= 0) then
		print*,'All the trouts died!  :('
		exit 
	endif
end do

close(unit=1)



end program main
