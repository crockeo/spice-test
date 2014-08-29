module Game where

--------------------
-- Global Imports --
import FRP.Spice

-------------------
-- Local Imports --
import Jumper
import Render
import Update

instance Game Jumper where
  update       = updateJumper
  render       = renderJumper
  loadAssets _ = return ()
