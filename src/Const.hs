module Const where

import Type
 
-- | Константы, параметры игры
-- | Экран
-- | Ширина экрана
screenWidth :: Int
screenWidth = 800
 
-- | Высота экрана
screenHeight :: Int
screenHeight = 450

-- | Положение правого края экрана
screenRight :: Offset
screenRight = fromIntegral screenWidth / 2
 
-- | Положение левого края экрана
screenLeft :: Offset
screenLeft = - fromIntegral screenWidth / 2

-- | Положение верхнего края экрана
screenTop :: Height
screenTop = fromIntegral screenHeight / 2

-- | Положение нижнего края экрана
screenBottom :: Height
screenBottom = - fromIntegral screenHeight / 2

-- | Препятствия
-- | Размер клевера
defaultCloverSize :: Size
defaultCloverSize = 1.0

-- | Размер плохой птички
defaultBadBirdSize :: Size
defaultBadBirdSize = 1.0

-- | Размер хорошей птички
defaultGoodBirdSize :: Size
defaultGoodBirdSize = 1.0

defaultBonusItemSize :: Size
defaultBonusItemSize = 1.0

-- | Диапазон высот препятствий.
obstacleHeightRange :: (Height, Height)
obstacleHeightRange = (screenBottom, screenTop)

typeRange :: (Int, Int)
typeRange = (1, 3)

-- | Расстояние между препятствиями
defaultOffset :: Offset
defaultOffset = screenRight * 1.5

-- | Диапазон, для вариации расстояния между препятсвтиями
obstacleOffsetRange :: (Offset, Offset)
obstacleOffsetRange = (-(defaultOffset / 2), defaultOffset / 2)

-- | Скорость фона
backgroundSpeed:: Speed
backgroundSpeed = 20

backgroundPictureSizeWidth :: Float
backgroundPictureSizeWidth = 5000

-- | Скорость игры
-- | Изначальная скорость движения игрока по вселенной - абсолютное изменение
gameSpeed :: Speed
gameSpeed = 100

originSpeedGoodBird :: Speed
originSpeedGoodBird = 100

originSpeedBadBird :: Speed
originSpeedBadBird = 200

originSpeedClover :: Speed
originSpeedClover = 10

originSpeedBonusItem :: Speed
originSpeedBonusItem = 150

-- | Величина ускорения игры
speedIncrease :: Speed
speedIncrease = 0.1

-- | Корова
-- | Размер коровы
defaultCowSize :: Size
defaultCowSize = 1.0

-- | Мзменение высоты коровы при нажатии на клавиши (в пикселях)
cowSpeed :: Float
cowSpeed = 200

cowAngelDefault :: Float
cowAngelDefault = -100

maxAngle :: Float
maxAngle = 10

minAngle :: Float
minAngle = -10

-- | Положение коровы по горизонтали
cowInitOffset :: Offset
cowInitOffset = screenLeft + (fromIntegral screenWidth / 10)

-- | Положение коровы по вертикали
cowInitHeight :: Height
cowInitHeight = 0

-- | Ширина картинки коровы
cowPictureSizeWidth :: Cow -> Size
cowPictureSizeWidth _ = 133

-- | Высота картинки коровы
cowPictureSizeHeight :: Cow -> Size
cowPictureSizeHeight _ = 68

defaultCollapseTime :: Float
defaultCollapseTime = 200

-- | Конфигурация босса
bossPictureWidth :: Boss -> Size
bossPictureWidth _ = 170

bossPictureHeight :: Boss -> Size
bossPictureHeight _ = 170

bossSpeed :: Boss -> Float
bossSpeed _ = 200
