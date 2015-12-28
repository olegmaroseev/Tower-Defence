module Config where

picBase = [("Tower1-1", "pic/tower1-1.bmp")
          ,("Tower1-2", "pic/tower1-2.bmp")
          ,("Tower1-3", "pic/tower1-3.bmp")
          ,("Tower2-1", "pic/tower2-1.bmp")
          ,("Tower2-2", "pic/tower2-2.bmp")
          ,("Tower2-3", "pic/tower2-3.bmp")
          ,("Tower3-1", "pic/tower3-1.bmp")
          ,("Tower3-2", "pic/tower3-2.bmp")
          ,("Tower3-3", "pic/tower3-3.bmp")
          ,("Enemy1", "pic/enemy1.bmp")
          ,("Enemy2", "pic/enemy2.bmp")
          ,("Enemy3", "pic/enemy3.bmp")
          ,("Enemy4", "pic/enemy4.bmp")
          ,("Background1", "pic/background.bmp")
          ,("Bullet1", "pic/bullet1.bmp")
          ,("Bullet2", "pic/bullet2.bmp")
          ,("Bullet3", "pic/bullet3.bmp")
          ,("Upgrade", "pic/upgrade.bmp")
          ,("Sell", "pic/sell.bmp")
          ]

--Main Window params
controlPanelHeight = 80 :: Int
menuPanelHeight = 0 :: Int
width = 1280 :: Int
height = 720 :: Int
hpHalfWidth = 100 :: Float
hpHalfHeight = 10 :: Float
hpTranslate = -50 :: Float
hpMargin = 2 :: Float
towerRadius = 40 :: Float
enemyRadius = 30 :: Float