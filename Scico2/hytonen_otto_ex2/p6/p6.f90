program p6
implicit none

    !Declaring wariables with specific real kinds
    real(kind=4) :: r4
    real(kind=8) :: r8
    real(kind=10) :: r10
    real(kind=16) :: r16

    !Exercise 6a
    write(6,*) "a)"
    write(6,*) " "

    !Testing the functions with different real kinds
    write(6,*) "Kind 4:"
    write(6,*) "Largest number: ",huge(r4)
    write(6,*) "Smallest number: ",tiny(r4)
    write(6,*) " "

    write(6,*) "Kind 8:"
    write(6,*) "Largest number: ",huge(r8)
    write(6,*) "Smallest number: ",tiny(r8)
    write(6,*) " "

    write(6,*) "Kind 10:"
    write(6,*) "Largest number: ",huge(r10)
    write(6,*) "Smallest number: ",tiny(r10)
    write(6,*) " "

    write(6,*) "Kind 16:"
    write(6,*) "Largest number: ",huge(r16)
    write(6,*) "Smallest number: ",tiny(r16)
    write(6,*) " "


    !Exercise 6b
    write(6,*) "b)"
    write(6,*) " "

    !I assume I only have to do this to one real kind. 
    write(6,*) "1+epsilon(r4)= ", 1+epsilon(r4)
    write(6,*) "1+epsilon(r4)/2= ", 1+epsilon(r4)/2
    write(6,*) "1-epsilon(r4)= ", 1-epsilon(r4)

end program p6