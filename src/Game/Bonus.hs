module Game.Bonus where

import Type

data CowSizeChange = CowSizeChange
  { sizeMultiplier    :: Size
  , sizeChangeTime :: Float
  } deriving Eq

data BirdSpeedChange = BirdSpeedChange
  { goodBirdSpeedMultiplier :: Float
  , badBirdSpeedMultiplier :: Float
  , cloverSpeedMultiplier :: Float
  , bonusSpeedMultiplier :: Float
  , birdSpeedChangetime :: Float
  } deriving Eq

data DonutGun = DonutGun
  { donutSpeed :: Speed
  , allDonuts :: [Donut]
  , donutGuntime :: Float
  , timeBetweenDonuts :: Float
  , damage :: Float
  } deriving Eq

data Invincible = Invincible
  { invincibleTime :: Float
  , invincibleLife :: Life
  } deriving Eq
  
data BonusItem = BonusItem
  { bonusItemPosition :: Position
  , bonusItemSize     :: Size
  , bonusItemType     :: BonusType
  , hidden            :: Bool
  }

data Donut = Donut
  { donutPosition :: Position
  , donutSize     :: Size
  } deriving Eq

data BonusType = Inv | SizeChange | BirdSpeed | Gun

data Bonus = InvincibleBonus Invincible
  | CowSizeChangeBonus CowSizeChange
  | BirdSpeedChangeBonus BirdSpeedChange
  | DonutGunBonus DonutGun
  | RandomBonus
  | NoBonus
  deriving Eq


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
  
tryAddInvincibleBonus :: Bonus -> Int -> Bonus  
tryAddInvincibleBonus (InvincibleBonus i) _ = InvincibleBonus i
tryAddInvincibleBonus _ n = InvincibleBonus Invincible
    { invincibleTime = defaultCollapseTime
    , invincibleLife = n
    }


defaultCollapseTime :: Float
defaultCollapseTime = 200

defaultBonusItemSize :: Size
defaultBonusItemSize = 1.0
