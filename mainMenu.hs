module MainMenu where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import GUI

level1min = "pic/level1.bmp"
level2min = "pic/level1.bmp"
level3min = "pic/level1.bmp"
level4min = "pic/level1.bmp"
level5min = "pic/level1.bmp"
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
      ("Button1", GUIElem (IconButton (0, 120) 50 50 level1ico False)),
      ("Button2", GUIElem (IconButton (-130, 120) 50 50 level1ico False)), 
      ("Button3", GUIElem (IconButton (130, 120) 50 50 level1ico False)),
      ("Button4", GUIElem (IconButton (70, 0) 50 50 level1ico False)),
      ("Button5", GUIElem (IconButton (-70, 0) 50 50 level1ico False))]
      (\_ -> id)
      renderMainMenu

renderMainMenu :: Float -> [(String, GUIElem)] -> [(String, GUIElem)]
renderMainMenu time xs = xs