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

data BackgroundPicture = BackgroundPicture
  { backgroundPicturePosition :: Position
  -- , backgroundPictureSize :: Size
  }

data Background = Background
  { mapBackgroundPicture :: [BackgroundPicture]
  , backgroundPictureSpeed :: Speed
  }

data Boss = Boss
  { bossHealth :: Float
  , bossDamage :: Float
  , bossHardness :: Float 
  }
 
data Mode = BossMode Boss | NightmareMode Float | OrdinaryMode Float 
  | NoBonusMode Float


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
  }
