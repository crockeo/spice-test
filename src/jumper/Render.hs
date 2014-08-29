module Render where

--------------------
-- Global Imports --
import FRP.Spice.Graphics
import FRP.Spice.Math
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
  bindColor white
  renderRectangle (pos jumper) (size jumper)

  bindColor grey
  renderRectangle (Vector (-1) $ groundLevel jumper) (Vector 2 (-2))
