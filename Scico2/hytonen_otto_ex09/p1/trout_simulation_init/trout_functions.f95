module trout_functions
implicit none
integer,parameter :: seed = 0, rk = selected_real_kind(10,40)

 

contains

!FUNCTION F1, Calculates population loss due to fishers in a year
!Input: real, population = the trout population
!Input: integer, fishers = the amount of fishers
!Output: real, the new fish population
!Each fisher decreases the population by rnd+0.2, in which rnd
!is random floating point number between [0,1)

function f1(population,fishers)   ! affect of fishers to trouts
	implicit none 
	real(rk),intent(in) :: population
	integer(rk),intent(in) :: fishers
	real(rk) :: rndm,f1,gaussrnd
	call random_seed()
	call random_number(rndm) ! give value between [0,1)
	f1 = population -  (rndm+0.2)*real(fishers)
	return

end function f1

!FUNCTION F2, calculates the yearly population gain/loss due to 
!the size of the population and food given to the population per year
!Input: real p1 = the trout population
!Input: real food = the amount of food given in a year
!Output: real, the new fish population 
!Population gain/loss is food*(rndm-0.2), in which rndm
!random floating point number between [0,1)

function f2(p1,food) 	! Normal growth of the trout population
	implicit none
	real(rk),intent(in) :: p1,food
	real(rk) :: rndm,f2
	call random_seed()
	call random_number(rndm)

	f2 = p1 + food*(rndm-0.2)
	return
end function f2


!FUNCTION F3 decreases the population by 70% if a random number is between decided values.
!The change of this is stated to be 1/100 000
!Input: real, population = trout population
!Output real, the new population 
!The new population is either unchanged or in the event of asteroid hit 0.3*population

! Gives a small chance to a asteroid to hit the Earth. This will affet the trout population
function f3(population)
	implicit none	
	real(rk),intent(in) :: population
	real(rk) :: rndm,f3,x
	call random_seed()
	call random_number(rndm)
	
	

	if (rndm > 1.0) then
		x = rndm

	else if (rndm > 0.35663.and. rndm < 0.35665) then ! Gives 1/100 000 chance for an asteroid hit
		f3 = population*0.3
		print*, 'Asteroid hit the Earth!'
	else 
		f3 = population
	end if
		return
	end function f3



end module trout_functions
