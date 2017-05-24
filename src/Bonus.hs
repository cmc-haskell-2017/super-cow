module Bonus where

import Graphics.Gloss.Interface.Pure.Game
import Type
import Const

instance Obstacle Cow where
    getPosition = cowPosition

    getSize cow = currentCowSize (cowSize cow) (cowBonus cow)

    setPosition cow position = cow { cowPosition = position }

    setSize cow size = cow { cowSize = size }

    getHeight = cowPictureSizeHeight

    getWidth = cowPictureSizeWidth
 
     
intToBonusType :: Int -> BonusType
intToBonusType 1 = Inv
intToBonusType 2 = SizeChange
intToBonusType 3 = BirdSpeed
intToBonusType 4 = Gun

intToBonusType _ = Inv

initBonusItem :: (Position, BonusType, Int) -> BonusItem
initBonusItem (p, bt, h) = BonusItem { bonusItemPosition = p, bonusItemType = bt, bonusItemSize = defaultBonusItemSize, hidden = h == 1 }
 
updateBonus :: Float -> Bonus -> Bonus
updateBonus _ (InvincibleBonus i)
  | invincibleTime i > 1 = InvincibleBonus i { invincibleTime = invincibleTime i - 1 }
updateBonus _ (CowSizeChangeBonus csb)
  | sizeChangeTime csb > 1 = CowSizeChangeBonus csb { sizeChangeTime = sizeChangeTime csb - 1 }
updateBonus _ (BirdSpeedChangeBonus bsc)
  | birdSpeedChangetime bsc > 1 = BirdSpeedChangeBonus bsc { birdSpeedChangetime = birdSpeedChangetime bsc - 1 }
updateBonus _ (DonutGunBonus dg)
  | donutGuntime dg > 1 = DonutGunBonus dg { donutGuntime = donutGuntime dg - 1 }
updateBonus _ _ = NoBonus

updateDonutPositions :: Float -> Position -> [Donut] -> Speed -> Float -> Score -> [Donut]
updateDonutPositions dt cowpos ds speed time score
  | mod score (truncate time) == 0 = 
    map (\d -> setPosition d (coordX d + dx, coordY d)) ([initDonut cowpos] ++ ds)
  | otherwise = 
    map (\d -> setPosition d (coordX d + dx, coordY d)) ds
  where
    coordX = fst . getPosition
    coordY = snd . getPosition
    dx = dt * speed
 
-- updateCow (updateBonus f) u


initDonut :: Position -> Donut
initDonut position = Donut
  { donutPosition = position
  , donutSize = defaultDonutSize
  }


drawBonusItem :: Images -> BonusItem -> BonusType -> Picture
drawBonusItem i bi _ 
  | hidden bi = draw (imageRandomStar i) bi
drawBonusItem i bi Inv = draw (imageInvincibleStar i) bi
drawBonusItem i bi SizeChange = draw (imageEnlargeStar i) bi
drawBonusItem i bi BirdSpeed = draw (imageFasterStar i) bi
drawBonusItem i bi Gun = draw (imageDonutStar i) bi


drawDonuts :: Images -> Bonus -> Picture
drawDonuts images (DonutGunBonus dgb) = 
  pictures (map (draw (imageDonut images)) (allDonuts dgb))
drawDonuts _ _ = blank
  
-- addBonus :: Universe -> Bonus -> Universe
-- addBonus

-- | Нарисовать препятствие
draw :: Obstacle o => Picture -> o -> Picture
draw image o = translate x y (scale r r image)
  where
    (x, y) = getPosition o
    r = getSize o
 
initBonus :: BonusType -> Universe -> Bonus
initBonus Inv u = InvincibleBonus Invincible { invincibleTime = 200, invincibleLife = universeLife u }
initBonus SizeChange _ = CowSizeChangeBonus CowSizeChange { sizeChangeTime = 200, sizeMultiplier = 1.5 }
initBonus BirdSpeed _ = BirdSpeedChangeBonus BirdSpeedChange
  { goodBirdSpeedMultiplier = 3
  , badBirdSpeedMultiplier = 3
  , cloverSpeedMultiplier = 3
  , bonusSpeedMultiplier = 3
  , birdSpeedChangetime = 200
  }
initBonus Gun _ = DonutGunBonus DonutGun
  { donutSpeed = defaultDonutSpeed
  , allDonuts = []
  , donutGuntime = 1000
  , timeBetweenDonuts = 50
  , damage = 1
  }
  
tryAddInvincibleBonus :: Bonus -> Int -> Bonus  
tryAddInvincibleBonus (InvincibleBonus i) _ = InvincibleBonus i
tryAddInvincibleBonus _ n = InvincibleBonus Invincible
    { invincibleTime = defaultCollapseTime
    , invincibleLife = n
    }

currentCowSize :: Size -> Bonus -> Size
currentCowSize s (CowSizeChangeBonus csb) = s * (sizeMultiplier csb)
currentCowSize s _ = s
