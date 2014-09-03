module Main where

--------------------
-- Global Imports --
import Data.Default
import FRP.Spice

-------------------
-- Local Imports --
import Jumper
import Game

----------
-- Code --

-- The initial state for Jumper
initState :: Jumper
initState =
  Jumper { pos         = Vector (0.016) ( 0.064)
         , dir         = Vector (0    ) ( 0    )
         , size        = Vector (0.032) (-0.064)
         , groundLevel = 0
         }

-- Custom configuration of the WindowConfig
windowConfig :: WindowConfig
windowConfig =
  def { getWindowTitle = "Jumper - Spice-Test" }

-- The main function
main :: IO ()
main = startEngine windowConfig initState
