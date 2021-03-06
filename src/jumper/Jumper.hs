module Jumper where

--------------------
-- Global Imports --
import FRP.Spice

----------
-- Code --

-- The Jumper datatype
data Jumper = Jumper { pos         :: Vector Float
                     , dir         :: Vector Float
                     , size        :: Vector Float
                     , groundLevel :: Float
                     }
