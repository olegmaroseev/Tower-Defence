{-# LANGUAGE  ExistentialQuantification #-}

module GUI where

import Data.Typeable
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Codec.BMP
import Config

type EventHandler a = Event -> a -> a

defaultHandler :: EventHandler a
defaultHandler _ = id

class GUIObject a where
  renderObject :: a -> Picture
  updateObject :: Float -> a -> a
  updateObject _ = id
  eventHandler :: EventHandler a
  eventHandler = defaultHandler

data GUIElem = forall a. (Typeable a, GUIObject a) => GUIElem a

instance GUIObject GUIElem where
  renderObject (GUIElem a) = renderObject a
  updateObject time (GUIElem a) = GUIElem $ updateObject time a
  eventHandler event (GUIElem a) = GUIElem $ eventHandler event a

data WindowParams = WindowParams { windowWidth::Int, windowHeight::Int, scaleWidth::Float, scaleHeight::Float }
  
runGUI :: Display -> Color -> Int -> WindowParams -> [(String, GUIElem)] ->
                          (Event -> [(String, GUIElem)] -> [(String, GUIElem)]) -> 
                          (Float -> [(String, GUIElem)] -> [(String, GUIElem)]) -> IO ()
runGUI display backColor simResolution wp
        objects handleEvents updateWorld = 
        play display backColor simResolution (wp, objects) renderGUI
        handleAllGUIEvents
        (\time (wp, xs) -> (wp, ((updateWorld time) . (updateGUI time)) xs))
     where
      handleAllGUIEvents :: Event -> (WindowParams, [(String, GUIElem)]) -> (WindowParams, [(String, GUIElem)])
      handleAllGUIEvents (EventResize (w, h)) (wp, xs) = (wp{scaleWidth = (fromIntegral w) / (fromIntegral $ windowWidth wp), scaleHeight = (fromIntegral h) / (fromIntegral $ windowHeight wp)}, xs)
      handleAllGUIEvents (EventKey k  ks m (x, y)) (wp, xs) = basicHandleAllGUIEvents (EventKey k  ks m (x / scaleWidth wp, y / scaleHeight wp)) (wp, xs)
      handleAllGUIEvents (EventMotion (x, y)) (wp, xs) = basicHandleAllGUIEvents (EventMotion (x / scaleWidth wp, y / scaleHeight wp)) (wp, xs)
      basicHandleAllGUIEvents event (wp, xs) = (wp, ((handleEvents event) . (handleGUIEvents event)) xs)



renderGUI :: (WindowParams, [(String, GUIElem)]) -> Picture
renderGUI (wp, objects) = Scale (scaleWidth wp) (scaleHeight wp) $ Pictures $ map (renderObject.snd) objects
        
handleGUIEvents :: Event -> [(String, GUIElem)] -> [(String, GUIElem)]
handleGUIEvents event objects = map (\(n, a) -> (n, eventHandler event a)) objects
        
updateGUI :: Float -> [(String, GUIElem)] -> [(String, GUIElem)]
updateGUI time objects = map (\(n, a) -> (n, updateObject time a)) objects


unpackCast :: (GUIObject a, Typeable a) => GUIElem -> Maybe a
unpackCast (GUIElem a) = cast a
         
data TextButton = TextButton Point   --centerPoint
                             Integer --width
                             Integer --height
                             Float   --scale factor
                             Color   --color
                             String  --text
                             Bool -- Highlighted
                             deriving (Show, Typeable)

data IconButton = IconButton Point   --centerPoint
                             Integer --width
                             Integer --height
                             Picture --icon
                             Bool -- Highlighted
                             deriving (Show, Typeable)
                             

data SpecialIconButton a = SpecialIconButton Point   --centerPoint
                             Integer --width
                             Integer --height
                             Picture --icon
                             Bool -- Highlighted
                             a
                             deriving (Show, Typeable)
data SpecialTextButton a = SpecialTextButton Point   --centerPoint
                             Integer --width
                             Integer --height
                             Float   --scale factor
                             Color   --color
                             String  --text
                             Bool -- Highlighted
                             a
                             deriving (Show, Typeable)
data TextBox = TextBox Point   --centerPoint
                       Integer --width
                       Integer --height
                       Color   --color
                       Float   --strHeight
                       [String]  --contents
                       deriving (Show, Typeable)
contains :: Point -> (Integer, Integer) -> Point -> Bool
contains (x, y) (w, h) (tx, ty) = (ty >= bottom) && (ty <= top) && (tx >= left) && (tx <= right)
  where
    halfW = (fromIntegral w) / 2
    halfH = (fromIntegral h) / 2
    bottom = y - halfH
    left = x - halfW
    top = y + halfH
    right = x + halfW
                       
instance GUIObject TextButton where
  renderObject = renderTButton
  eventHandler (EventMotion ep) tb@(TextButton p w h sc c t hl)
    | (not hl) && contains p (w,h) ep = TextButton p w h sc c t True
    | hl && (not $ contains p (w,h) ep) = TextButton p w h sc c t False
    | otherwise = tb
  eventHandler _ o = o

instance GUIObject (SpecialTextButton a) where
  renderObject = renderSTButton
  eventHandler (EventMotion ep) tb@(SpecialTextButton p w h sc c t hl a)
    | (not hl) && contains p (w,h) ep = SpecialTextButton p w h sc c t True a
    | hl && (not $ contains p (w,h) ep) = SpecialTextButton p w h sc c t False a
    | otherwise = tb
  eventHandler _ o = o
  
instance GUIObject IconButton where
  renderObject = renderIButton
  eventHandler (EventMotion ep) tb@(IconButton p w h t hl)
    | (not hl) && contains p (w,h) ep = IconButton p w h t True
    | hl && (not $ contains p (w,h) ep) = IconButton p w h t False
    | otherwise = tb
  eventHandler _ o = o

instance GUIObject (SpecialIconButton a) where
  renderObject = renderSIButton
  eventHandler (EventMotion ep) tb@(SpecialIconButton p w h t hl a)
    | (not hl) && contains p (w,h) ep = SpecialIconButton p w h t True a
    | hl && (not $ contains p (w,h) ep) = SpecialIconButton p w h t False a
    | otherwise = tb
  eventHandler _ o = o
  
instance GUIObject TextBox where
  renderObject = renderTextBox
  
                       
renderTButton :: TextButton -> Picture
renderTButton (TextButton (x, y) w h sc c txtStr hl) = 
    let halfW = fromIntegral w / 2
        halfH = fromIntegral h / 2
        fr = 3
    in Translate x y $ Pictures $ [Color c $ Polygon [(-halfW - fr, -halfH - fr), 
                                                      (halfW + fr, -halfH - fr), 
                                                      (halfW + fr, halfH + fr), 
                                                      (-halfW - fr, halfH + fr)], 
                                   Color c $ Polygon [(-halfW, -halfH), 
                                                      (halfW, -halfH), 
                                                      (halfW, halfH), 
                                                      (-halfW, halfH)],
                                      Translate (-halfW) (-(halfH / 2)) 
                                      $ Scale sc sc
                                        $ Color black $ text txtStr
                                   ] 
                                   ++ (if hl then
                                     [Color (makeColor 1 1 1 0.5) $ 
                                       Polygon [(-halfW - fr, -halfH - fr), 
                                                (halfW + fr, -halfH - fr), 
                                                (halfW + fr, halfH + fr), 
                                                (-halfW - fr, halfH + fr)]]
                                        else [])
                                        
renderSTButton :: SpecialTextButton a -> Picture
renderSTButton (SpecialTextButton (x, y) w h sc c txtStr hl _) = renderTButton (TextButton (x, y) w h sc c txtStr hl)
    
    
renderIButton :: IconButton -> Picture
renderIButton (IconButton (x, y) w h icon hl) = do
     let halfW = fromIntegral w / 2
         halfH = fromIntegral h / 2
     Pictures $ [Translate x y $ scale 1 1 icon] ++ (if hl then
                                     [Translate x y $ Color (makeColor 1 1 1 0.5) $ 
                                       Polygon [(-halfW, -halfH), 
                                                (halfW, -halfH), 
                                                (halfW, halfH), 
                                                (-halfW, halfH)]]
                                        else [])
                                        
renderSIButton :: SpecialIconButton a -> Picture
renderSIButton (SpecialIconButton (x, y) w h icon hl _) = do
     let halfW = fromIntegral w / 2
         halfH = fromIntegral h / 2
     Pictures $ [Translate x y $ scale 1 1 icon] ++ (if hl then
                                     [Translate x y $ Color (makeColor 1 1 1 0.5) $ 
                                       Polygon [(-halfW, -halfH), 
                                                (halfW, -halfH), 
                                                (halfW, halfH), 
                                                (-halfW, halfH)]]
                                        else [])

renderTextBox :: TextBox -> Picture
renderTextBox (TextBox (x, y) w h c sh strs) = do
     let halfW = fromIntegral w / 2
         halfH = fromIntegral h / 2
         borderH = 4 :: Float
         scaleRate = (sh / 100)
       in Translate x y $ Pictures $ [Color c $ Polygon [(-halfW, -halfH), 
                                                               (halfW, -halfH), 
                                                               (halfW, halfH), 
                                                               (-halfW, halfH)]] ++ 
        [Translate (-(halfW)) (halfH - ((sh + borderH) * (fromIntegral (curHStep + 1)))) $ 
           Scale scaleRate scaleRate $ 
              Color black $ text (strs !! curHStep) |  curHStep <- [0..((length strs) - 1)]]

