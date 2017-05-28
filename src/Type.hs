module Type where

import Graphics.Gloss.Interface.Pure.Game
  
-- | Структуры данных
-- | Высота и Положение объектов
type Height   = Float            -- ^ Высота обьекта
type Offset   = Float            -- ^ Сдвиг обьекта
type Position = (Offset,Height)  -- ^ Координаты обьекта
type Life     = Int              -- ^ Жизни (изначально 3)
type Score    = Int              -- ^ Счет (изменяется постоянно)
type Size     = Float            -- ^ Размер обьекта
type Speed    = Float            -- ^ Скорость обьекта
  
-- | Объекты игровой вселенной
-- | Клевер - добавляет одну жизнь
data Clover = Clover
  { cloverPosition :: Position
  , cloverSize     :: Size
  }

-- | Плохая птичка - снимает 2 жизни
data BadBird = BadBird
  { badBirdPosition :: Position
  , badBirdSize     :: Size
  }

-- | Хорошая птичка - снимает 1 жизни
data GoodBird = GoodBird
  { goodBirdPosition :: Position
  , goodBirdSize     :: Size
  }

data BonusItem = BonusItem
  { bonusItemPosition :: Position
  , bonusItemSize     :: Size
  , bonusItemType     :: BonusType
  }

data Donut = Donut
  { donutPosition :: Position
  , donutSize     :: Size
  }

-- | Карта препятствий
data Map = Map
  { mapGoodBirds  :: [GoodBird]
  , mapBadBirds   :: [BadBird]
  , mapClovers    :: [Clover]
  , mapBonusItems      :: [BonusItem]
  , obstacleSpeedGoodBird :: Speed
  , obstacleSpeedBadBird :: Speed
  , obstacleSpeedClover :: Speed
  , obstacleSpeedBonusItem :: Speed
  }

data BackgroundPicture = BackgroundPicture
  { backgroundPicturePosition :: Position
  -- , backgroundPictureSize :: Size
  }

data Background = Background
  { mapBackgroundPicture :: [BackgroundPicture]
  , backgroundPictureSpeed :: Speed
  }

-- | Корова
data Cow = Cow
  { cowPosition  :: Position
  , cowSize      :: Size
  , cowSpeedUp   :: Speed  -- ^ Cкорость по вертикали
  , cowSpeedLeft :: Speed  -- ^ Cкорость по горизонтали
  , cowAngel     :: Float  -- ^ Угол наклона
  , cowSpeedAngel :: Float
  , cowPushed    :: Int
  , cowBonus     :: Bonus
  }

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
  }

data Invincible = Invincible
  { invincibleTime :: Float
  , invincibleLife :: Life
  } deriving Eq

data Mode = NightmareMode Float | OrdinaryMode Float

data BonusType = Inv | SizeChange | BirdSpeed

data Bonus = InvincibleBonus Invincible
  | CowSizeChangeBonus CowSizeChange
  | BirdSpeedChangeBonus BirdSpeedChange
  | NoBonus
  deriving Eq

data Boss = Boss
  { bossPosition :: Position
  , bossActivity :: Int
  }

-- | Игровая вселенная
data Universe = Universe
  { universeMap        :: Map    -- ^ Препятствия игровой вселенной
  , universeCow        :: Cow    -- ^ Корова
  , universeScore      :: Score  -- ^ Cчет
  , universeLife       :: Life   -- ^ Жизни
  , universeStop       :: Bool   -- ^ Флаг остановки игры
  , universeGameOver   :: Bool   -- ^ Флаг окончания игры
  , universeBackground :: Background
  -- , universeCowBonus   :: Bonus
  , universeMode       :: Mode
  , universeBoss       :: Boss   -- ^ Босс
  }

-- | Изображения объектов
data Images = Images
  { imageCow             :: Picture   -- ^ Изображение коровы.
  , imageCowBlurred      :: Picture   -- ^ Изображение размытой коровы.
  , imageClover          :: Picture   -- ^ Изображение клевера
  , imageGoodBirdUp      :: Picture   -- ^ Изображение GrayBirdUp.
  , imageGoodBirdDown    :: Picture   -- ^ Изображение GrayBirdDown.
  , imageBadBirdUp       :: Picture   -- ^ Изображение BlueBirdUp.
  , imageBadBirdDown     :: Picture   -- ^ Изображение BlueBirdDown.
  , imageSkyWithGrass    :: Picture   -- ^ Изображение Неба.
  , imageGameOver        :: Picture   -- ^ Изображение конца игры.
  , imageDonut           :: Picture
  , imageDonutStar       :: Picture
  , imageFasterStar      :: Picture
  , imageInvincibleStar  :: Picture
  , imageRandomStar      :: Picture
  , imageEnlargeStar     :: Picture
  , imageBoss            :: Picture
  , imageBomb            :: Picture
  }


-- | Реализация класса типов - препятствие
class Obstacle o where
  -- | Получение позиции препятствия
  getPosition :: o -> Position
  -- | Получние размера препятствия
  getSize :: o -> Size
  -- | Установка позиции препятствия
  setPosition :: o -> Position -> o
  -- | Установка размера препятствия
  setSize :: o -> Size -> o
  -- | Высота рисунка препятствия
  getHeight :: o -> Size
  -- | Ширина рисунка препятствия
  getWidth :: o -> Size
  
  
  
-- | Препятствие - клевер
instance Obstacle Clover where
    getPosition = cloverPosition

    getSize = cloverSize

    setPosition clover position = clover { cloverPosition = position }

    setSize clover size = clover { cloverSize = size }

    getHeight _ = 50

    getWidth _ = 50

instance Obstacle BonusItem where
    getPosition = bonusItemPosition

    getSize = bonusItemSize

    setPosition bonusItem position = bonusItem { bonusItemPosition = position }

    setSize bonusItem size = bonusItem { bonusItemSize = size }

    getHeight _ = 68

    getWidth _ = 68

-- | Препятствие - плохая птичка
instance Obstacle BadBird where
  getPosition = badBirdPosition

  getSize = badBirdSize

  setPosition badBird position = badBird { badBirdPosition = position }

  setSize badBird size = badBird { badBirdSize = size }

  getWidth _ = 81

  getHeight _ = 42

-- | Препятствие - хорошая птичка
instance Obstacle GoodBird where
  getPosition = goodBirdPosition

  getSize = goodBirdSize

  setPosition goodBird position = goodBird { goodBirdPosition = position }

  setSize goodBird size = goodBird { goodBirdSize = size }

  getWidth _ = 67

  getHeight _ = 36

instance Obstacle Donut where
  getPosition = donutPosition

  getSize = donutSize

  setPosition donut position = donut { donutPosition = position }

  setSize donut size = donut { donutSize = size }

  getWidth _ = 67

  getHeight _ = 36
