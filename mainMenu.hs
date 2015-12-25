{-# LANGUAGE  ExistentialQuantification, ScopedTypeVariables #-}

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import GUI

level1min = "pic/level1.bmp"
level2min = "pic/level1.bmp"
level3min = "pic/level1.bmp"
level4min = "pic/level1.bmp"
level5min = "pic/level1.bmp"



testMainMenu :: IO()
testMainMenu = do
        level1ico@(Bitmap _ _ _ _) <- loadBMP level1min
        runGUI 
         (InWindow "Tower Defence" 
         (600, 400) 
         (100,  100))
         (greyN 0.25) 
         30
         [("TestButton1", GUIElem (TextButton (0, 0) 100 50 (greyN 0.5) "START" False)), ("TestButton2", GUIElem (TextButton (200, 0) 100 50 (greyN 0.5) "QUIT" False)),
         ("TestButton3", GUIElem (IconButton (100, 0) 150 150 level1ico))]
         (\_ -> id)
         updateAll--(\_ -> id)
