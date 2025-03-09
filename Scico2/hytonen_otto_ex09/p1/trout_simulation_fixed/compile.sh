#! /bin/bash


rm *.o
rm *.mod

gfortran -c trout_functions.f95
gfortran -c trout_population_functions.f95
gfortran -c main.f95
gfortran main.o trout_population_functions.o trout_functions.o 


