-- | Бонус
module Game.Bonus where

import Type

-----------------------
-- * Модель Бонуса
-----------------------

-- | Бонус - изменение размера коровы
data CowSizeChange = CowSizeChange
  { sizeMultiplier    :: Size  -- ^ Коэфицент изменение размера
  , sizeChangeTime    :: Float -- ^ Время действия
  } deriving Eq

-- | Бонус - изменение скорости птичек
data BirdSpeedChange = BirdSpeedChange
  { goodBirdSpeedMultiplier :: Float -- ^ Коэфицент изменения скорости хорошей птички
  , badBirdSpeedMultiplier  :: Float -- ^ Коэфицент изменения скорости плохой птички
  , cloverSpeedMultiplier   :: Float -- ^ Коэфицент изменения скорости клевера
  , bonusSpeedMultiplier    :: Float -- ^ Коэфицент изменения скорости бонуса 
  , birdSpeedChangetime     :: Float -- ^ Время действия
  } deriving Eq

-- | Бонус - пончиковый бластер
data DonutGun = DonutGun
  { donutSpeed        :: Speed   -- ^ Скорость пончиков
  , allDonuts         :: [Donut] -- ^ Все пончики
  , donutGuntime      :: Float   -- ^ Время действия
  , timeBetweenDonuts :: Float   -- ^ Время между пончиками
  , damage            :: Float   -- ^ Урон 1 пончика
  } deriving Eq

-- | Бонус - неуязвимость
data Invincible = Invincible
  { invincibleTime :: Float -- ^ Время действия
  , invincibleLife :: Life  -- ^ Текущий уровень жизней
  } deriving Eq
  
-- | Обьект бонуса
data BonusItem = BonusItem
  { bonusItemPosition :: Position  -- ^ Координаты
  , bonusItemSize     :: Size      -- ^ Размер
  , bonusItemType     :: BonusType -- ^ Тип бонуса
  , hidden            :: Bool      -- ^ Показывать-ли тип бонуса?
  }

-- | Обьект пончика
data Donut = Donut
  { donutPosition :: Position -- ^ Координаты
  , donutSize     :: Size     -- ^ Размер
  } deriving Eq

-- | Тип бонуса 
data BonusType = Inv -- ^ Неуязвимость
  | SizeChange       -- ^ Изменение размера игрока
  | BirdSpeed        -- ^ Изменение скорости объектов
  | Gun              -- ^ Пончиковый бластер

-- | Бонус
data Bonus = InvincibleBonus Invincible  -- ^ Неуязвимость
  | CowSizeChangeBonus CowSizeChange     -- ^ Изменение размера игрока
  | BirdSpeedChangeBonus BirdSpeedChange -- ^ Изменение скорости обьектов
  | DonutGunBonus DonutGun               -- ^ Пончиковы бластер
  | RandomBonus                          -- ^ Случайный бонус
  | NoBonus                              -- ^ Без бонуса
  deriving Eq

-- | Нумеровка бонусов для случайной их генерации
intToBonusType :: Int -> BonusType
intToBonusType 1 = Inv
intToBonusType 2 = SizeChange
intToBonusType 3 = BirdSpeed
intToBonusType 4 = Gun
intToBonusType _ = Inv

-- | Инициализировать новый объект бонус
initBonusItem :: (Position, BonusType, Int) -- ^ (Позиция, тип бонуса, скрывать?) 
              -> BonusItem
initBonusItem (p, bt, h) = BonusItem 
  { bonusItemPosition = p
  , bonusItemType     = bt
  , bonusItemSize     = defaultBonusItemSize
  , hidden            = h == 1 
  }

-- | Обновить бонус
updateBonus :: Float -> Bonus -> Bonus
updateBonus _ (InvincibleBonus i)
  | invincibleTime i > 1        = InvincibleBonus i 
    { invincibleTime = invincibleTime i - 1 
    }
updateBonus _ (CowSizeChangeBonus csb)
  | sizeChangeTime csb > 1      = CowSizeChangeBonus csb 
    { sizeChangeTime = sizeChangeTime csb - 1 
    }
updateBonus _ (BirdSpeedChangeBonus bsc)
  | birdSpeedChangetime bsc > 1 = BirdSpeedChangeBonus bsc 
    { birdSpeedChangetime = birdSpeedChangetime bsc - 1
    }
updateBonus _ (DonutGunBonus dg)
  | donutGuntime dg > 1         = DonutGunBonus dg 
    { donutGuntime = donutGuntime dg - 1 
    }
updateBonus _ _                 = NoBonus
  
-- | Нужно-ли добавлять бонус неуязвимости?
tryAddInvincibleBonus :: Bonus -> Int -> Bonus  
tryAddInvincibleBonus (InvincibleBonus i) _ = InvincibleBonus i
tryAddInvincibleBonus _ n = InvincibleBonus Invincible
    { invincibleTime = defaultCollapseTime
    , invincibleLife = n
    }
    
-------------------
-- * Константы 
-------------------

-- | Сколько времени длится бонус неузявимости после столкновения
defaultCollapseTime :: Float
defaultCollapseTime = 200

-- | Размер объекта бонуса 
defaultBonusItemSize :: Size
defaultBonusItemSize = 1.0
