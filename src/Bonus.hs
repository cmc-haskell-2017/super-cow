module Bonus where

import Type
import Const
 
intToBonusType :: Int -> BonusType
intToBonusType 1 = Inv
intToBonusType 2 = SizeChange
intToBonusType 3 = BirdSpeed
intToBonusType _ = Inv

initBonusItem :: (Position, BonusType) -> BonusItem
initBonusItem (p, bt) = BonusItem { bonusItemPosition = p, bonusItemType = bt, bonusItemSize = defaultBonusItemSize }

 
updateBonus :: Float -> Bonus -> Bonus
updateBonus _ (InvincibleBonus i)
  | invincibleTime i > 1 = InvincibleBonus i { invincibleTime = invincibleTime i - 1 }
updateBonus _ (CowSizeChangeBonus csb)
  | sizeChangeTime csb > 1 = CowSizeChangeBonus csb { sizeChangeTime = sizeChangeTime csb - 1 }
updateBonus _ (BirdSpeedChangeBonus bsc)
  | birdSpeedChangetime bsc > 1 = BirdSpeedChangeBonus bsc { birdSpeedChangetime = birdSpeedChangetime bsc - 1 }
updateBonus _ _ = NoBonus

-- updateCow (updateBonus f) u

applyBonus :: Universe -> Universe
applyBonus u = case cowBonus (universeCow u) of
  InvincibleBonus i -> u { universeLife = invincibleLife i }
  _ -> u



-- addBonus :: Universe -> Bonus -> Universe
-- addBonus

 
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
  
tryAddInvincibleBonus :: Bonus -> Int -> Bonus  
tryAddInvincibleBonus (InvincibleBonus i) _ = InvincibleBonus i
tryAddInvincibleBonus _ n = InvincibleBonus Invincible
    { invincibleTime = defaultCollapseTime
    , invincibleLife = n
    }

currentCowSize :: Size -> Bonus -> Size
currentCowSize s (CowSizeChangeBonus csb) = s * (sizeMultiplier csb)
currentCowSize s _ = s
