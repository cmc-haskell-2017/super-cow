module Type where

import Graphics.Gloss.Interface.Pure.Game

--------------------------------------------------
-- * Основные общие типы, используемые в игре
--------------------------------------------------

-- | Высота (координата Х)
type Height   = Float     

-- | Ширина (Координата Y)
type Offset   = Float 

-- | Координаты обьекта в пространстве
type Position = (Offset,Height)

-- | Жизни
type Life     = Int              

-- | Счет
type Score    = Int              

-- | Размер
type Size     = Float            

-- | Скорость
type Speed    = Float            

-- | Картинка заднего фона
data BackgroundPicture = BackgroundPicture 
  { backgroundPicturePosition :: Position -- ^ Положение картинки 
  }

-- | Фон
data Background = Background
  { mapBackgroundPicture   :: [BackgroundPicture] -- ^ Множество картинок, составляющих фон
  , backgroundPictureSpeed :: Speed               -- ^ Скорость движения фона
  }

-- | Босс
data Boss = Boss    
  { bossHealth   :: Life   -- ^ Жизни 
  , bossDamage   :: Float  -- ^ Урон
  , bossHardness :: Float  -- ^ Сложность
  }

-- | Режим игры
data Mode = BossMode Boss       -- ^ Режим Босса
          | NightmareMode Float -- ^ Режим без бонусов и клеверов
          | OrdinaryMode Float  -- ^ Обычный режим
          | NoBonusMode Float   -- ^ Режим без бонусов

-- | Изображения объектов игровой вселенной
data Images = Images
  { imageCow             :: Picture   -- ^ Изображение коровы
  , imageCowBlurred      :: Picture   -- ^ Изображение размытой коровы
  , imageClover          :: Picture   -- ^ Изображение клевера
  , imageGoodBirdUp      :: Picture   -- ^ Изображение Хорошей птички 
  , imageGoodBirdDown    :: Picture   -- ^ Изображение Хорошей птички 2
  , imageBadBirdUp       :: Picture   -- ^ Изображение Плохой птички
  , imageBadBirdDown     :: Picture   -- ^ Изображение Плохой птички
  , imageSkyWithGrass    :: Picture   -- ^ Изображение Неба
  , imageGameOver        :: Picture   -- ^ Изображение конца игры
  , imageDonut           :: Picture   -- ^ Изображение пончика
  , imageDonutStar       :: Picture   -- ^ Изображение бонуса, пончикового бластера
  , imageFasterStar      :: Picture   -- ^ Изображение бонуса, ускорителя
  , imageInvincibleStar  :: Picture   -- ^ Изображение бонуса, неуязвимости
  , imageRandomStar      :: Picture   -- ^ Изображение случайного бонуса
  , imageEnlargeStar     :: Picture   -- ^ Изображение бонуса, увеличения
  }
