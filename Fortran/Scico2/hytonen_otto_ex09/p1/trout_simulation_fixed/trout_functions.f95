module trout_functions
implicit none
integer,parameter :: seed = 0, rk = selected_real_kind(10,40)
real(rk) :: rndm
 

contains

!Calculates population loss due to fishers in a year
!Input: real, population = the trout population
!Input: integer, fishers = the amount of fishers
!Output: real, the new fish population
!Each fisher decreases the population by rnd+0.2, in which rnd
!is random floating point number between [0,1)

real(rk) function fishing_loss(population,fishers)
	implicit none 
	real(rk),intent(in) :: population
	integer(rk),intent(in) :: fishers
	call random_number(rndm) ! give value between [0,1)
	fishing_loss = population -  (rndm+0.2)*real(fishers)
end function fishing_loss

!Calculates the yearly population gain/loss due to 
!the size of the population and food given to the population per year
!Input: real population = the trout population
!Input: real food = the amount of food given in a year
!Output: real, the new fish population 
!Population gain/loss is food*(rndm-0.2), in which rndm
!random floating point number between [0,1)

real(rk) function natural_growth(population,food)
	implicit none
	real(rk),intent(in) :: population,food
	call random_seed()
	call random_number(rndm)
	natural_growth = population + food*(rndm-0.2)
end function natural_growth


!The function decreases the population by 70% if a random number is between decided values.
!The change of this is stated to be 1/100 000
!Input: real, population = trout population
!Output real, the new population 
!The new population is either unchanged or in the event of asteroid hit 0.3*population

! Gives a small chance to a asteroid to hit the Earth. This will affet the trout population
real(rk) function asteroid_loss(population)
	implicit none	
	real(rk),intent(in) :: population
	call random_seed()
	call random_number(rndm)
	
	if (rndm.le.0.00001) then ! Gives 1/100 000 chance for an asteroid hit
		asteroid_loss = population*0.3
		print*, 'Asteroid hit the Earth!'
	else 
		asteroid_loss = population
	end if
	end function asteroid_loss



end module trout_functions
