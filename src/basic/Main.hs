module Main where

-------------
-- Imports --
import FRP.Spice.Math.Vector
import FRP.Spice.Graphics
import FRP.Spice

import Data.Default

----------
-- Code --

-- Some directions
up, down, left, right :: Vector Float
up    = Vector ( 0) ( 1)
down  = Vector ( 0) (-1)
left  = Vector (-1) ( 0)
right = Vector ( 1) ( 0)

-- The speed in which pos should move
speedCfg :: Float
speedCfg = 0.05

-- The size rendered squares should take
size :: Float
size = 0.05

-- Applying the speed to a vector
speed :: Vector Float -> Vector Float
speed vec = vec * Vector speedCfg speedCfg

-- A basic game type
data BasicGame = BasicGame { pos :: Vector Float }

-- Updating the game
updatePos :: DeltaTime -> Input -> BasicGame -> BasicGame
updatePos dt input game =
  move dt game [ (keyboard input ! CharKey 'W', speed up   )
               , (keyboard input ! CharKey 'S', speed down )
               , (keyboard input ! CharKey 'A', speed left )
               , (keyboard input ! CharKey 'D', speed right)
               ]
  where move :: DeltaTime -> BasicGame -> [(Bool, Vector Float)] -> BasicGame
        move dt game                 []              = game
        move dt game@(BasicGame pos) ((True , d):xs) = move dt (game { pos = pos + d }) xs
        move dt game                 ((False, _):xs) = move dt game xs

updateGame :: DeltaTime -> Input -> BasicGame -> BasicGame
updateGame dt input game = updatePos dt input game

-- Rendering the game
renderGame :: BasicGame -> IO ()
renderGame (BasicGame pos) = do
  renderSquare pos size

-- Giving BasicGame a Game instance.
instance Game BasicGame where
  update dt input game = updateGame dt input game
  render          game = renderGame          game

-- Starting the game
main :: IO ()
main = startEngine defaultWindowConfig $ BasicGame def
