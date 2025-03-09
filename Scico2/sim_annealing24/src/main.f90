program main 

use constants        
use boxes                                        
use algorithm
use inout

implicit none
!Reads in c,a,n, file1, file2, file3
call read_cmd_line

!Initializes the random number generator
call new_rndgenerator

!Sets initial state of the boxes
call setinitialstate(n)

!####################
call resetovlgrid()
print*, size(ovl_list)
print*, ovlgrid_origo
print*, ovlsqr_size
print*, ovl_list(1,:)
!#####################


!Writing the initial positions to the first output file
call writepositions(filename_to1,2)

!Runs the algorithm
call sa_algorithm(c,a)

!Writing the final positions to the 2nd file
call writepositions(filename_to2,2)

print*, 'Empty space percentage of the total area:',&
 100*(area(boubox)-boxesarea())/area(boubox) 

print*,tot
print*,disag
end program 