module Day01B where

fuelRequirement mass = mass `div` 3 - 2

sumFuelRequirements = sum . map fuelRequirement
