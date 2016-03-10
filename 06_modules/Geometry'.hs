module Geometry
( sphereVolume
, sphereArea
, cubeVolume
, cubeArea
, cuboidVolume
, cuboidArea
) where

sphereVolume :: Float -> Float
sphereVolume radius = (4.0 / 3.0) * pi * (radius ^3)

sphereArea :: Float -> Float
sphereArea radius = 4 * pi * (radius^2)

cubeVolume :: Float -> Float
cubeVolume side = CuboidVolume side side side

cubeArea :: Float -> Float
cubeArea side = CuboidArea side side side

cuboidArea :: Float -> Float -> Float -> Float
cuboidArea a b c = rectangleArea a b * 2 + rectangleArea a c * 2 + rectangleArea c b *2

rectangleArea :: Float -> Float -> Float
rectangleArea a b = a * b
