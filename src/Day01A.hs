module Day01A where

fuelRequirement mass = mass `div` 3 - 2

sumFuelRequirements = sum . map fuelRequirement 

