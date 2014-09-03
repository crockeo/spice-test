module Main where

--------------------
-- Global Imports --
import Data.Default
import FRP.Spice

----------
-- Code --

data SpriteGame = SpriteGame (Vector Float)

speed :: Float
speed = 1.0

-- Updating the game
updateGame :: DeltaTime -> Input -> SpriteGame -> SpriteGame
updateGame dt input game =
  move dt game [ (key input ! CharKey 'W', up)
               , (key input ! CharKey 'S', down)
               , (key input ! CharKey 'A', left)
               , (key input ! CharKey 'D', right)
               ]
  where move :: DeltaTime -> SpriteGame -> [(Bool, Vector Float)] -> SpriteGame
        move dt game             []                = game
        move dt game             ((False, _  ):xs) = move dt game xs
        move dt (SpriteGame pos) ((True , dir):xs) = move dt (SpriteGame $ pos ^+ dir ^*> speed ^*> dt) xs

-- Rendering the game
renderGame :: Assets -> SpriteGame -> Scene
renderGame assets (SpriteGame pos) = do
  renderSprite (sprites assets ! "res/test.jpg") pos

-- Loading the assets required
loadAssetsGame :: SpriteGame -> LoadAssets
loadAssetsGame game = loadSpriteAsset "res/test.jpg"

instance Game SpriteGame where
  update     = updateGame
  render     = renderGame
  loadAssets = loadAssetsGame

windowConfig :: WindowConfig
windowConfig = def { getWindowTitle = "Sprite - spice-test" }

main :: IO ()
main = startEngineDefault $ SpriteGame def
