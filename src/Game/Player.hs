module Game.Player where

import Game.Obstacle
import Type
import Const
import Game.Bonus
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


instance Obstacle Cow where
    getPosition = cowPosition

    getSize cow = currentCowSize (cowSize cow) (cowBonus cow)

    setPosition cow position = cow { cowPosition = position }

    setSize cow size = cow { cowSize = size }

    getHeight = cowPictureSizeHeight

    getWidth = cowPictureSizeWidth


-- | Инициализировать корову
initCow :: Cow
initCow = Cow
  { cowPosition = (cowInitOffset, cowInitHeight)
  , cowSize = defaultCowSize
  , cowSpeedUp = 0
  , cowSpeedLeft = 0
  , cowAngel = 0
  , cowSpeedAngel = 0
  , cowPushed = 0
  , cowBonus = InvincibleBonus Invincible { invincibleTime = 200, invincibleLife = 5 }
  }
  
  
  
  -- | Обновление коровы
updateCow :: Float -> Cow -> Cow
updateCow dt c = c
  { cowPosition = ((max screenLeft (min screenRight (coordX - dx)))
  ,(max screenBottom (min screenTop (coordY - dy))))
  , cowBonus = updateBonus dt (cowBonus c)
  , cowAngel = max minAngle (min maxAngle (cowAngel c + da))
  }
  where
    coordX = fst (cowPosition c)
    coordY = snd (cowPosition c)
    dx  = dt * (cowSpeedLeft c)
    dy = dt * (cowSpeedUp c)
    da = dt * (cowSpeedAngel c)

currentCowSize :: Size -> Bonus -> Size
currentCowSize s (CowSizeChangeBonus csb) = s * (sizeMultiplier csb)
currentCowSize s _ = s

-- | Передвижение коровы вверх и вниз, если можно.
goCowUpDown :: (Speed -> Speed) -> (Float -> Float) -> Int -> Cow -> Cow
goCowUpDown f fAngel flagPushed cow = cow { cowSpeedUp = f $ cowSpeedUp cow
                               , cowSpeedAngel = fAngel $ cowSpeedAngel cow
                               , cowPushed = flagPushed
                               }

-- | Передвижение коровы влево и вправо, если можно.
goCowLeftRight :: (Speed -> Speed) -> Cow -> Cow
goCowLeftRight f cow = cow { cowSpeedLeft = f $ cowSpeedLeft cow }


-- | Ширина картинки коровы
cowPictureSizeWidth :: Cow -> Size
cowPictureSizeWidth _ = 133

-- | Высота картинки коровы
cowPictureSizeHeight :: Cow -> Size
cowPictureSizeHeight _ = 68

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
