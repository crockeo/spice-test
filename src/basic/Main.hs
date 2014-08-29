module Main where

-------------
-- Imports --
import FRP.Spice.Graphics
import FRP.Spice.Math
import FRP.Spice

import Control.Applicative
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
speed :: Float
speed = 0.5

-- The size rendered squares should take
size :: Float
size = 0.05

-- Applying the speed to a Float
applySpeed :: DeltaTime -> Float -> Float
applySpeed dt n = n * speed * dt

-- Applying the speed to a vector
vApplySpeed :: DeltaTime -> Vector Float -> Vector Float
vApplySpeed dt vec = pure (applySpeed dt) <*> vec

-- A basic game type
data BasicGame = BasicGame { pos      :: Vector Float
                           , mousePos :: Vector Float
                           }

-- Updating the game
updatePos :: DeltaTime -> Input -> BasicGame -> BasicGame
updatePos dt input game =
  move dt game [ (keyboard input ! CharKey 'W', vApplySpeed dt up   )
               , (keyboard input ! CharKey 'S', vApplySpeed dt down )
               , (keyboard input ! CharKey 'A', vApplySpeed dt left )
               , (keyboard input ! CharKey 'D', vApplySpeed dt right)
               ]
  where move :: DeltaTime -> BasicGame -> [(Bool, Vector Float)] -> BasicGame
        move dt game                   []              = game
        move dt game@(BasicGame pos _) ((True , d):xs) = move dt (game { pos = pos + d }) xs
        move dt game                   ((False, _):xs) = move dt game xs

-- Updating the mouse position
updateMousePosition :: Input -> BasicGame -> BasicGame
updateMousePosition input game@(BasicGame _ mousePos) =
  game { mousePos = mousePosition input }

updateGame :: DeltaTime -> Input -> BasicGame -> BasicGame
updateGame dt input game = updateMousePosition input $ updatePos dt input game

-- Rendering the game
renderGame :: Assets -> BasicGame -> Scene
renderGame _ (BasicGame pos mousePos) = do
  bindColor $ color3i 0 0 255
  renderRectangle ((mousePos * Vector 0 1) - (Vector 1 0)) $ Vector 2 2

  bindColor $ color3i 255 0 0
  renderSquare pos      size

  bindColor $ color3i 0 255 0
  renderSquare mousePos size

-- Giving BasicGame a Game instance.
instance Game BasicGame where
  update       = updateGame
  render       = renderGame
  loadAssets _ = return ()

-- Starting the game
main :: IO ()
main = startEngine defaultWindowConfig $ BasicGame def def
