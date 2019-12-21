module Day01B where

fuelRequirement mass | mass <= 0 = 0
fuelRequirement mass = fuel + fuelRequirement fuel
    where fuel = max 0 (mass `div` 3 - 2)


sumFuelRequirements = sum . map fuelRequirement
