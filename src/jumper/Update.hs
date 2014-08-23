module Update where

--------------------
-- Global Imports --
import FRP.Spice.Math
import FRP.Spice
import Debug.Trace

-------------------
-- Local Imports --
import Jumper

----------
-- Code --

-- The initial speed of a jump
jumpSpeed :: Float
jumpSpeed = 1.5

-- The speed of gravity
gravity :: Float
gravity = 0.2

-- The speed of the player
speed :: Float
speed = 0.5

-- Checking if the player is on the ground.
onGround :: Jumper -> Bool
onGround jumper =
  (y + h) <= ground
  where (Vector _ y) = pos  jumper
        (Vector _ h) = size jumper
        ground       = groundLevel jumper

-- Checking if the player is in the air.
inAir :: Jumper -> Bool
inAir = not . onGround

-- Updating the gravity on the player
updateGravity :: DeltaTime -> Jumper -> Jumper
updateGravity dt jumper =
  if onGround jumper
    then jumper { pos = Vector x (ground - h)
                , dir = Vector dx 0 }

    else jumper { dir = Vector dx (dy - (gravity * speed)) }
  where (Vector  x  y) = pos jumper
        (Vector dx dy) = dir jumper
        (Vector  _  h) = size jumper
        ground         = groundLevel jumper

-- Jumping if the player can
jumpIfCan :: Input -> Jumper -> Jumper
jumpIfCan input jumper =
  if inAir jumper
    then jumper
    else if keyboard input ! CharKey ' '
      then jumper { dir = Vector dx jumpSpeed }
      else jumper
  where (Vector dx _) = dir jumper

-- Changing horizontal speed
lrInput :: Input -> Jumper -> Jumper
lrInput input jumper
  | left && right = jumper { dir = Vector 0        dy }
  | left          = jumper { dir = Vector (-speed) dy }
  | right         = jumper { dir = Vector ( speed) dy }
  | otherwise     = jumper { dir = Vector 0        dy }
  where (Vector _ dy) = dir jumper
        left          = keyboard input ! CharKey 'A'
        right         = keyboard input ! CharKey 'D'

-- Moving the speed
move :: DeltaTime -> Jumper -> Jumper
move dt jumper =
  jumper { pos = pos jumper + (scalar (dir jumper) dt) }

logPos :: Jumper -> Jumper
logPos jumper = traceShow (pos jumper) jumper

-- Updating the game state
updateJumper :: DeltaTime -> Input -> Jumper -> Jumper
updateJumper dt input jumper =
  applyAll jumper [ updateGravity dt
                  , jumpIfCan input
                  , lrInput input
                  , move dt
                  , logPos
                  ]
  where applyAll :: a -> [a -> a] -> a
        applyAll a []     = a
        applyAll a (x:xs) = applyAll (x a) xs