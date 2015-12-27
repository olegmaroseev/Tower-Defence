module MainMenu where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import GUI
import parseLevel
import Graphics.Gloss
import Codec.BMP
import System.Environment
import GameLogic
import Data.Maybe

level1min = "pic/level1.bmp"
level2min = "pic/level2.bmp"
level3min = "pic/level3.bmp"
level4min = "pic/level4.bmp"
level5min = "pic/level5.bmp"
butQuitMin = "pic/but.bmp"

mainMenu :: IO()
mainMenu = do 
    level1ico@(Bitmap _ _ _ _) <- loadBMP level1min
    butQuit@(Bitmap _ _ _ _) <- loadBMP butQuitMin
    runGUI (InWindow "Tower Defence Main Menu" (600, 400) 
     (100,  100))
     (greyN 0.25) 
     30
     [("ButtonQuit", GUIElem (IconButton (0, -120) 100 50 butQuit False)),
      ("Button1", GUIElem (IconButton (0, 120) 90 90 level1ico False)),
      ("Button2", GUIElem (IconButton (-130, 120) 90 90 level1ico False)), 
      ("Button3", GUIElem (IconButton (130, 120) 90 90 level1ico False)),
      ("Button4", GUIElem (IconButton (70, 0) 90 90 level1ico False)),
      ("Button5", GUIElem (IconButton (-70, 0) 90 90 level1ico False))]
      (\_ -> id)
      renderMainMenu

renderMainMenu :: Float -> [(String, GUIElem)] -> [(String, GUIElem)]
renderMainMenu time xs = xs