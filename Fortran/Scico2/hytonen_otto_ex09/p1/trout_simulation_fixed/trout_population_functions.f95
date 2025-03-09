module trout_population_functions
use trout_functions
implicit none
integer,parameter :: ik = selected_real_kind(10,40)

contains

!Subroutine takes in the initial conditions before each iteration
!It uses the functions from 'trout_functions' to calculate the population 
!after the iteration 

subroutine new_population(initial_trouts,food,fishers,population)

	real(ik),intent(in) :: initial_trouts,food
	integer(ik),intent(in) :: fishers
	real(ik),intent(out) :: population
		
	population = natural_growth(initial_trouts,food)
	population = fishing_loss(population,fishers)
	population = asteroid_loss(population)	
		
end subroutine new_population


end module trout_population_functions

