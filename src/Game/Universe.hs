-- | Игровая вселенная
module Game.Universe where

import Game.Player
import Game.Obstacle
import Type
import Game.Bonus

----------------------------------
-- * Модель игровой вселенной
----------------------------------

-- | Игровая вселенная
data Universe = Universe
  { universeMap        :: Map        -- ^ Препятствия игровой вселенной
  , universeCow        :: Cow        -- ^ Корова
  , universeScore      :: Score      -- ^ Cчет
  , universeLife       :: Life       -- ^ Жизни
  , universeStop       :: Bool       -- ^ Флаг остановки игры
  , universeGameOver   :: Bool       -- ^ Флаг окончания игры
  , universeBackground :: Background -- ^ Задний фон
  , universeMode       :: Mode       -- ^ Режим игры
  }
  
-- | Убрать пончики, которые столкнулись
removeCollidedDonuts :: Map -> Universe -> Universe
removeCollidedDonuts m u = case cowBonus $ universeCow u of
  DonutGunBonus dg -> u
    { universeCow = (universeCow u) 
      { cowBonus   = DonutGunBonus (dg
        { allDonuts = filter (\d -> 
          not ((collisionMulti d badBirds) || (collisionMulti d goodBirds))) 
          (allDonuts dg) 
        })  
      }
    }
  _                -> u
  where 
    badBirds  = mapBadBirds m
    goodBirds = mapGoodBirds m

-- | Проверить пончики на столкновения с другими обьектами
collideDonuts :: [Donut] -> Universe -> Universe
collideDonuts [] u     = u
collideDonuts (d:ds) u = case cowBonus $ universeCow u of 
  DonutGunBonus _ -> collideDonuts ds (u 
    { universeMap = 
      badCollisionHandle (universeMap u) d (cowBonus $ universeCow u)
    })
  _               -> u

-- | Обновить положения пончиков
updateDonuts :: Float -> Universe -> Universe
updateDonuts dt u = u 
  { universeCow = cow 
    { cowBonus    = case cowbonus of 
      DonutGunBonus dg -> DonutGunBonus (dg 
        { allDonuts = cropInsideScreen 
          (updateDonutPositions dt (cowPosition cow) (allDonuts dg) 
          (donutSpeed dg) (timeBetweenDonuts dg) (universeScore u)) 
        })
      _                -> NoBonus
    } 
  }
  where 
    cow      = universeCow u
    cowbonus = cowBonus cow  
  
-- | Обновить скорость движения коровы
updateSpeedCow :: (Cow -> Cow) -> Universe -> Universe
updateSpeedCow f u = u 
  { universeCow = f $ universeCow u 
  }

-- | Применить бонус ко вселенной
applyBonus :: Float -> Universe -> Universe
applyBonus dt u = case cowBonus $ universeCow (updateDonuts dt u) of
  InvincibleBonus i -> u 
    { universeLife = invincibleLife i 
    }
  DonutGunBonus dg  -> collideDonuts (allDonuts dg) (updateDonuts dt u)
  _                 -> u

-- | Инифиализировать бонус во вселенной по его типу
initBonus :: BonusType -> Universe -> Bonus
initBonus Inv u        = InvincibleBonus Invincible 
  { invincibleTime = 200
  , invincibleLife = universeLife u
  }
initBonus SizeChange _ = CowSizeChangeBonus CowSizeChange 
  { sizeChangeTime = 200
  , sizeMultiplier = 1.5
  }
initBonus BirdSpeed _  = BirdSpeedChangeBonus BirdSpeedChange
  { goodBirdSpeedMultiplier = 3
  , badBirdSpeedMultiplier  = 3
  , cloverSpeedMultiplier   = 3
  , bonusSpeedMultiplier    = 3
  , birdSpeedChangetime     = 200
  }
initBonus Gun _        = DonutGunBonus DonutGun
  { donutSpeed        = defaultDonutSpeed
  , allDonuts         = []
  , donutGuntime      = 1000
  , timeBetweenDonuts = 50
  , damage            = 1
  }
