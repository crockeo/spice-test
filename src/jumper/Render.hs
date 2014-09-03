module Render where

--------------------
-- Global Imports --
import Data.Monoid
import FRP.Spice

-------------------
-- Local Imports --
import Jumper

----------
-- Code --

grey :: Color
grey = color3i 127 127 127

-- Updating the game state
renderJumper :: Assets -> Jumper -> Scene
renderJumper _ jumper = do
  let (Vector _ y) = pos jumper
      h            = groundLevel jumper
  putStrLn $ mconcat ["Y: ", show y, ", H: ", show h]
  bindColor white
  renderRectangle (pos jumper) (size jumper)

  bindColor grey
  renderRectangle (Vector (-1) $ groundLevel jumper) (Vector 2 (-2))
