module Render where

--------------------
-- Global Imports --
import FRP.Spice.Graphics
import FRP.Spice.Math

-------------------
-- Local Imports --
import Jumper

----------
-- Code --

grey :: Color
grey = color3i 127 127 127

-- Updating the game state
renderJumper :: Jumper -> Scene
renderJumper jumper = do
  bindColor white
  renderRectangle (pos jumper) (size jumper)

  bindColor grey
  renderRectangle (Vector (-1) $ groundLevel jumper) (Vector 2 (-2))
