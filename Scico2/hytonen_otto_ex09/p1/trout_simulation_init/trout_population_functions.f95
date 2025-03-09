module trout_population_functions
use trout_functions
implicit none
integer,parameter :: ik = selected_real_kind(10,40)

contains

!Subroutine x1 takes in the initial conditions before each iteration
!It uses the functions from 'trout_functions' to calculate the population 
!after the iteration 
!interestingly the order of iteration is, f2,f1,f3:
!Gain/loss due to food, g/l due to fishters, g/l due to asteroid

subroutine x1(initial_trouts,food,fishers,population)

	real(ik),intent(in) :: initial_trouts,food
	integer(ik),intent(in) :: fishers
	real(ik),intent(out) :: population
		
	population = f2(initial_trouts,food)
	population = f1(population,fishers)
	population = f3(population)	
		
end subroutine x1


end module trout_population_functions

