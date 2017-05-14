module SuperCow where

import System.Random
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Juicy

-- | Запустить игру «Super Cow»
runSuperCow :: Images -> IO ()
runSuperCow images = do
  g <- newStdGen
  play display bgColor fps (initUniverse g) (drawUniverse images) 
    handleUniverse updateUniverse
  where
    display = InWindow "Super Cow" (screenWidth, screenHeight) (200, 200)
    bgColor = white   -- цвет фона
    fps     = 60     -- кол-во кадров в секунду

-- | Загрузка изображений
loadImages :: IO Images
loadImages = do
  Just cowPicture              <- loadJuicyPNG "images/cow.png"
  Just cloverPicture           <- loadJuicyPNG "images/clover.png"
  Just goodBirdUpPicture       <- loadJuicyPNG "images/GrayBirdUp.png"
  Just goodBirdDownPicture     <- loadJuicyPNG "images/GrayBirdDown.png"
  Just badBirdUpPicture        <- loadJuicyPNG "images/BlueBirdUp.png"
  Just badBirdDownPicture      <- loadJuicyPNG "images/BlueBirdDown.png"
  Just skyWithGrassPicture     <- loadJuicyJPG "images/SkyWithGrass.jpg"
  Just gameOver                <- loadJuicyPNG "images/GameOver.png"
  Just cowBlurredPicture       <- loadJuicyPNG "images/cowBlurred.png"
  Just donutPicture            <- loadJuicyPNG "images/donut.png"
  Just donutStarPicture        <- loadJuicyPNG "images/DonutStar.png"
  Just fasterStarPicture       <- loadJuicyPNG "images/FasterStar.png"
  Just invincibleStarPicture   <- loadJuicyPNG "images/InvincibleStar.png"
  Just randomStarPicture       <- loadJuicyPNG "images/RandomStar.png"
  Just enlargeStarPicture      <- loadJuicyPNG "images/EnlargeStar.png"

  return Images
    { imageCow               = scale 1.0 1.0 cowPicture
    , imageCowBlurred        = scale 1.0 1.0 cowBlurredPicture
    , imageClover            = scale 1.0 1.0 cloverPicture
    , imageGoodBirdUp        = scale 1.0 1.0 goodBirdUpPicture
    , imageGoodBirdDown      = scale 1.0 1.0 goodBirdDownPicture
    , imageBadBirdUp         = scale 1.0 1.0 badBirdUpPicture
    , imageBadBirdDown       = scale 1.0 1.0 badBirdDownPicture
    , imageSkyWithGrass      = scale 1.0 1.0 skyWithGrassPicture
    , imageGameOver          = scale 1.0 1.0 gameOver
    , imageDonut             = scale 1.0 1.0 donutPicture
    , imageDonutStar         = scale 0.1 0.1 donutStarPicture
    , imageFasterStar        = scale 0.1 0.1 fasterStarPicture
    , imageInvincibleStar    = scale 0.1 0.1 invincibleStarPicture
    , imageRandomStar        = scale 0.1 0.1 randomStarPicture
    , imageEnlargeStar       = scale 0.1 0.1 enlargeStarPicture
    }


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
  { birdSpeedMultiplier :: Size 
  , birdSpeedChangetime :: Float
  }

data DonutGun = DonutGun
  { donutSpeed :: Speed
  , allDonuts :: [Donut] 
  , donutGuntime :: Float
  , damage :: Float
  }

data Invincible = Invincible
  { invincibleTime :: Float
  , invincibleLife :: Life
  } deriving Eq
  
data Boss = Boss 
  { bossHealth :: Float
  , bossDamage :: Float
  , bossHardness :: Float }
  
data Mode = BossMode Boss | NightmareMode Float | OrdinaryMode Float

data BonusType = Inv | SizeChange

data Bonus = InvincibleBonus Invincible 
  | CowSizeChangeBonus CowSizeChange
  | NoBonus 
  deriving Eq
    
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

-- | Отрисовка игровой вселенной
-- | Отобразить игровую вселенную 
drawUniverse :: Images -> Universe -> Picture
drawUniverse images universe = pictures
  [ drawBackground (imageSkyWithGrass images) (universeBackground universe)
  , drawObstacles images (universeMap universe)
  , drawCow images (universeCow universe) 
  , drawScore (universeScore universe)
  , drawLife (universeLife universe)
  , drawGameOver (imageGameOver images) (universeGameOver universe)
  ]

-- | Отобразить одну картинку фона
drawBackgroundPicture :: Picture -> BackgroundPicture -> Picture
drawBackgroundPicture image backgroundPicture = translate x y (scale 1.0 1.0 image)
  where 
    (x, y) = backgroundPicturePosition backgroundPicture

-- | Отобразить фон
drawBackground :: Picture -> Background -> Picture
drawBackground image background = pictures (map 
              (drawBackgroundPicture image)
    (cropBackgroundInsideScreen (mapBackgroundPicture background)))

-- drawBackground :: Picture -> Background -> Picture
-- drawBackground image background = translate (-w) h (scale 1.0 1.0 image)
--   where 
--     w = fromIntegral screenWidth  / 2
--     h = fromIntegral screenHeight / 2
    
-- | Отобразить все препятствия игровой вселенной, вмещающихся в экран 
drawObstacles :: Images -> Map -> Picture
drawObstacles images obstacles = pictures
  [ pictures (map (draw (imageGoodBirdUp  images)) 
    (cropInsideScreen (mapGoodBirds obstacles)))
  , pictures (map (draw (imageBadBirdUp images)) 
    (cropInsideScreen (mapBadBirds obstacles)))
  , pictures (map (draw (imageClover images)) 
    (cropInsideScreen (mapClovers obstacles)))
  , pictures (map (\bi -> drawBonusItem images bi (bonusItemType bi)) 
    (cropInsideScreen (mapBonusItems obstacles)))
  ]

-- | Нарисовать корову 
drawCow :: Images -> Cow -> Picture
drawCow images cow 
  | cowBonus cow == NoBonus = translate x y (rotate (cowAngel cow) (scale r r (imageCow images)))
  | otherwise = drawCowWithBonus images cow (cowBonus cow)
    where
        (x, y) = cowPosition cow
        r = cowSize cow

uncurry :: (a -> b -> c) -> (a, b) -> 
drawCowWithBonus :: Images -> Cow -> Bonus -> Picture
drawCowWithBonus images cow (InvincibleBonus i)
  = translate (fst (cowPosition cow)) (snd (cowPosition cow)) (rotate (cowAngel cow) (scale (cowSize cow) (cowSize cow) (imageCowBlurred images)))
drawCowWithBonus images cow (CowSizeChangeBonus i)
  = translate (fst (cowPosition cow)) (snd (cowPosition cow)) (rotate (cowAngel cow) (scale newCowSize newCowSize (imageCow images)))
    where 
      newCowSize = (cowSize cow) * (sizeMultiplier i)
      

-- | Нарисовать счёт в левом верхнем углу экрана 
drawScore :: Score -> Picture
drawScore score = translate (-w) h (scale 30 30 (pictures
  [ translate 2 (-1.5) (scale 0.01 0.01 (color red (text (show score)))) ]))
  where
    w = fromIntegral screenWidth  / 2
    h = fromIntegral screenHeight / 2

-- | Нарисовать жизни в правом верхнем углу экрана 
drawLife :: Life -> Picture
drawLife life = translate w h (scale 30 30 (pictures
  [ translate (-2) (-1.5) (scale 0.01 0.01 (color red (text (show life)))) ]))
  where
    w = fromIntegral screenWidth  / 2
    h = fromIntegral screenHeight / 2

-- | Нарисовать препятствие    
draw :: Obstacle o => Picture -> o -> Picture
draw image o = translate x y (scale r r image)
  where
    (x, y) = getPosition o
    r = getSize o
    
drawBonusItem :: Images -> BonusItem -> BonusType -> Picture
drawBonusItem i bi Inv = draw (imageInvincibleStar i) bi 
drawBonusItem i bi SizeChange = draw (imageEnlargeStar i) bi 

drawGameOver :: Picture -> Bool -> Picture
drawGameOver _ False = blank
drawGameOver image True = translate (-w) h (scale 1.0 1.0 image)
  where 
    w = 0
    h = screenTop / 2

-- | Инициализация вселенной 
-- | Инициализировать игровую вселенную, используя генератор случайных значений
initUniverse :: StdGen -> Universe
initUniverse g = Universe
  { universeMap = moveObstacles (initMap g) 1000
  , universeCow = initCow
  , universeScore = 0
  , universeLife  = 5
  , universeStop = False
  , universeGameOver = False
  , universeBackground = initBackground 
  , universeMode = OrdinaryMode 1
  -- , universeCowBonus = NoBonus
  }
                
-- | Инициализировать клевер
initClover :: Position -> Clover
initClover position = Clover 
  { cloverPosition = position
  , cloverSize = defaultCloverSize
  }

-- | Инициализировать плохую птичку 
initBadBird :: Position -> BadBird
initBadBird position = BadBird
  { badBirdPosition = position
  , badBirdSize = defaultBadBirdSize
  }
    
-- | Инициализировать хорошую птичку
initGoodBird :: Position -> GoodBird
initGoodBird position = GoodBird
  { goodBirdPosition = position 
  , goodBirdSize = defaultGoodBirdSize
  }

intToBonusType :: Int -> BonusType 
intToBonusType 1 = Inv
intToBonusType 2 = SizeChange

initBonusItem :: (Position, BonusType) -> BonusItem 
initBonusItem (p, bt) = BonusItem { bonusItemPosition = p, bonusItemType = bt, bonusItemSize = defaultBonusItemSize }

initBackgroundPicture :: Offset -> BackgroundPicture
initBackgroundPicture offset = BackgroundPicture
  { backgroundPicturePosition = (offset, screenTop)
  -- , backgroundPictureSize = (2560, 1440)
  }

initBackground :: Background
initBackground = Background
  { mapBackgroundPicture = map initBackgroundPicture backgroundPicturePositions
  , backgroundPictureSpeed = backgroundSpeed
  }
  where
    backgroundPicturePositions = [screenLeft, screenLeft + 800 ..]

-- | Инициализировать карту препятствий 
initMap :: StdGen -> Map
initMap g = Map 
  { mapGoodBirds = map initGoodBird goodBirdPositions
  , mapClovers = map initClover cloverPositions
  , mapBadBirds = map initBadBird badBirdPositions
  , mapBonusItems = map initBonusItem bonusItemTypePositions
  , obstacleSpeedGoodBird = originSpeedGoodBird
  , obstacleSpeedBadBird = originSpeedBadBird
  , obstacleSpeedClover = originSpeedClover
  , obstacleSpeedBonusItem = originSpeedBonusItem
  }
  where
    (g1, g2) = split g
    (g3, g4) = split g1
    (g5, g6) = split g2
    (g7, g8) = split g3
    (g9, g10) = split g4
    (g11, g12) = split g5
    (g13, g14) = split g6
    goodBirdPositions = zip (zipWith (+) [screenLeft, screenLeft + defaultOffset..] 
      (randomRs obstacleOffsetRange g14)) (randomRs obstacleHeightRange g7)
    cloverPositions = zip (zipWith (+) [screenLeft, screenLeft + defaultOffset..] 
      (randomRs obstacleOffsetRange g8)) (randomRs obstacleHeightRange g9)
    badBirdPositions = zip (zipWith (+) [screenLeft, screenLeft + defaultOffset..] 
      (randomRs obstacleOffsetRange g10)) (randomRs obstacleHeightRange g11)
    bonusItemTypePositions = zip 
      (zip (zipWith (+) [screenLeft, screenLeft + defaultOffset..] 
      (randomRs obstacleOffsetRange g12)) (randomRs obstacleHeightRange g13))
      (map intToBonusType (randomRs typeRange g))

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

-- | Взаимодействия c игровой вселенной        
-- | Обработчик событий игры
handleUniverse :: Event -> Universe -> Universe
handleUniverse (EventKey (SpecialKey KeyUp) Down _ _) = updateSpeedCow 
  (goCowUpDown (subtract cowSpeed) (+cowAngelDefault) 1)
handleUniverse (EventKey (SpecialKey KeyDown) Down _ _) = updateSpeedCow 
  (goCowUpDown (+cowSpeed) (subtract cowAngelDefault) 2)
handleUniverse (EventKey (SpecialKey KeyUp) Up _ _) = updateSpeedCow 
  (goCowUpDown (+cowSpeed) (subtract cowAngelDefault) 0)
handleUniverse (EventKey (SpecialKey KeyDown) Up _ _) = updateSpeedCow 
  (goCowUpDown (subtract cowSpeed) (+cowAngelDefault) 0)
handleUniverse (EventKey (SpecialKey KeyLeft) Down _ _) = updateSpeedCow 
  (goCowLeftRight (+cowSpeed))
handleUniverse (EventKey (SpecialKey KeyRight) Down _ _) = updateSpeedCow 
  (goCowLeftRight (subtract cowSpeed))
handleUniverse (EventKey (SpecialKey KeyLeft) Up _ _) = updateSpeedCow 
  (goCowLeftRight (subtract cowSpeed))
handleUniverse (EventKey (SpecialKey KeyRight) Up _ _) = updateSpeedCow 
  (goCowLeftRight (+cowSpeed))
handleUniverse (EventKey (SpecialKey KeySpace) Down _ _) = toggleGame
handleUniverse _ = id

-- | Передвижение коровы вверх и вниз, если можно.
goCowUpDown :: (Speed -> Speed) -> (Float -> Float) -> Int -> Cow -> Cow
goCowUpDown f fAngel flagPushed cow = cow { cowSpeedUp = f $ cowSpeedUp cow
                               , cowSpeedAngel = fAngel $ cowSpeedAngel cow
                               , cowPushed = flagPushed
                               }

-- | Передвижение коровы влево и вправо, если можно.
goCowLeftRight :: (Speed -> Speed) -> Cow -> Cow
goCowLeftRight f cow = cow { cowSpeedLeft = f $ cowSpeedLeft cow }

-- | Обновление игровой вселенной
-- | Обновить состояние игровой вселенной 
updateUniverse :: Float -> Universe -> Universe
updateUniverse dt u
  | gameStopped == True = u
  | negativeLifeBalance u = toggleGame u
  | otherwise = applyBonus (cowBonus (universeCow u)) (updateLife dt (u
    { universeMap  = updateMap dt (universeMap u)
    , universeCow = updateCow dt (universeCow u)
    , universeScore  = updateScore dt (universeScore u)
    , universeBackground = updateBackground dt (universeBackground u)
    }))
  where
    gameStopped = (universeStop u)


updateBonus :: Float -> Bonus -> Bonus 
updateBonus dt (InvincibleBonus i) 
  | invincibleTime i > 1 = InvincibleBonus i { invincibleTime = invincibleTime i - 1 }
updateBonus dt (CowSizeChangeBonus csb)
  | sizeChangeTime csb > 1 = CowSizeChangeBonus csb { sizeChangeTime = sizeChangeTime csb - 1 }
updateBonus dt b = NoBonus

updateCow (updateBonus f) u

applyBonus :: Universe -> Universe
applyBonus u = case cowBonus (cow u) of
  InvincibleBonus i -> ...
  
applyBonus _ u = u
  
-- addBonus :: Universe -> Bonus -> Universe
-- addBonus 
      
toggleGameHardness :: Universe -> Int -> Universe
toggleGameHardness u _ = u

toggleObstacleHardness :: Obstacle o => [o] -> (o -> o) -> [o]
toggleObstacleHardness os _ = os

-- | Проверка на конец игры
negativeLifeBalance :: Universe -> Bool
negativeLifeBalance u = life <= 0
  where
    life = (universeLife u)

-- | Изменить активность игры
toggleGame :: Universe -> Universe
toggleGame u 
  | universeLife u > 0 = u 
    { universeStop = not stopFlag
    , universeGameOver = False
    }
  | otherwise = u
    { universeStop = not stopFlag
    , universeLife = 5
    , universeScore = 0
    , universeGameOver = not gameOverFlag
    , universeMap = moveObstacles m 
    { obstacleSpeedGoodBird = originSpeedGoodBird 
    , obstacleSpeedBadBird = originSpeedBadBird
    , obstacleSpeedClover = originSpeedClover
    } 1000
    , universeCow = cow 
    { cowPosition = (cowInitOffset, cowInitHeight)
    , cowBonus = InvincibleBonus Invincible { invincibleLife = 5, invincibleTime = defaultCollapseTime }                
    }
    }
  where
    stopFlag = (universeStop u)
    m = (universeMap u)
    cow = (universeCow u)
    gameOverFlag = (universeGameOver u)
  
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


-- | Обновить карту игровой вселенной 
updateMap :: Float -> Map -> Map
updateMap dt obstacleMap = obstacleMap
  { mapGoodBirds = updateObstacles dt (mapGoodBirds obstacleMap) 
    (obstacleSpeedGoodBird obstacleMap)
  , mapBadBirds = updateObstacles dt (mapBadBirds obstacleMap) 
    (obstacleSpeedBadBird obstacleMap)
  , mapClovers = updateObstacles dt (mapClovers obstacleMap) 
    (obstacleSpeedClover obstacleMap)
  , mapBonusItems = updateObstacles dt (mapBonusItems obstacleMap) 
    (obstacleSpeedBonusItem obstacleMap)
  , obstacleSpeedGoodBird = obstacleSpeedGoodBird obstacleMap + speedIncrease
  , obstacleSpeedBadBird = obstacleSpeedBadBird obstacleMap + speedIncrease
  , obstacleSpeedClover = obstacleSpeedClover obstacleMap + speedIncrease
  , obstacleSpeedBonusItem = obstacleSpeedBonusItem obstacleMap + speedIncrease
  }

updateBackground :: Float -> Background -> Background
updateBackground dt background = background
  { mapBackgroundPicture = updateBackgrounds dt (mapBackgroundPicture background)
                            (backgroundPictureSpeed background)
  }

-- | Обновить фон
updateBackgrounds :: Float -> [BackgroundPicture] -> Speed -> [BackgroundPicture] 
updateBackgrounds _ [] _ = []
updateBackgrounds dt backgrounds speed = 
  dropWhile (\o -> fst (backgroundPicturePosition o) < screenLeft) 
    (map (\o -> o { backgroundPicturePosition = (coordX o - dx, coordY o)}) backgrounds)
  where
    coordX = fst . backgroundPicturePosition
    coordY = snd . backgroundPicturePosition
    dx  = dt * speed

-- | Обновить препятствия игровой вселенной 
updateObstacles :: Obstacle o => Float -> [o] -> Speed -> [o]
updateObstacles _ [] _ = []
updateObstacles dt obstacles speed = 
  dropWhile (\o -> fst (getPosition o) < screenLeft) 
    (map (\o -> setPosition o (coordX o - dx, coordY o)) obstacles)
  where
    coordX = fst . getPosition
    coordY = snd . getPosition
    dx  = dt * speed

-- | Обновить счет 
updateScore :: Float -> Score -> Score
updateScore _ score = score + 1

-- | Обновить жизни
updateLife :: Float -> Universe -> Universe
updateLife _ u
  | collisionMulti cow (mapClovers obstacleMap) = u 
  { universeLife = checkLife life
  , universeMap = collisionHandle obstacleMap cow
  }
  | collisionMulti cow (mapGoodBirds obstacleMap) = u 
    { universeLife = (life - 1) 
    , universeMap = collisionHandle obstacleMap cow
    , universeCow = cow { cowBonus = tryAddInvincibleBonus (cowBonus cow) 1 }
    }
  | collisionMulti cow (mapBadBirds obstacleMap) = u 
    { universeLife = (life - 2) 
    , universeMap = collisionHandle obstacleMap cow
    , universeCow = cow { cowBonus = tryAddInvincibleBonus (cowBonus cow) 2 }
    }
  | collisionMulti cow (mapBonusItems obstacleMap) = u 
    { universeMap = collisionHandle obstacleMap cow
    , universeCow = cow { cowBonus = initBonus (bonusItemType (getCollisionObstacle cow (mapBonusItems obstacleMap))) u }
    }
  | otherwise = u
  where
    life = (universeLife u)
    cow = (universeCow u)
    obstacleMap = (universeMap u)
    checkLife 5 = 5
    checkLife l = l + 1
    tryAddInvincibleBonus (InvincibleBonus i) _ = InvincibleBonus i
    tryAddInvincibleBonus _ n = InvincibleBonus Invincible 
      { invincibleTime = defaultCollapseTime 
      , invincibleLife = (life - n)
      }
    
initBonus :: BonusType -> Universe -> Bonus
initBonus Inv u = InvincibleBonus Invincible { invincibleTime = 200, invincibleLife = universeLife u }
initBonus SizeChange u = CowSizeChangeBonus CowSizeChange { sizeChangeTime = 200, sizeMultiplier = 1.5 }

-- | Обновить скорость движения коровы
updateSpeedCow :: (Cow -> Cow) -> Universe -> Universe
updateSpeedCow f u = u { universeCow = f $ universeCow u }


-- | Оставить только те препятствия, которые входят в экран 
cropInsideScreen :: (Obstacle o) => [o] -> [o]
cropInsideScreen obs = dropWhile (\o -> pos o < screenLeft) $ 
  takeWhile (\o -> pos o < screenRight) obs
  where 
    pos = fst . getPosition

-- | Оставить только те картинки фона, которые входят в экран 
cropBackgroundInsideScreen :: [BackgroundPicture] -> [BackgroundPicture]
cropBackgroundInsideScreen background = dropWhile (\o -> pos o < screenLeft) $ 
  takeWhile (\o -> pos o < screenRight) background
  where 
    pos = fst . backgroundPicturePosition

-- | Препятствия, которые не входят в экран
isOutsideScreen :: Obstacle o => o -> Bool
isOutsideScreen = not . isInsideScreen

-- | Препятствия, которые входят в экран        
isInsideScreen :: Obstacle o => o -> Bool
isInsideScreen o = pos o < screenRight && pos o > screenLeft 
    where
        pos = fst . getPosition

-- | Столкновения
-- | Сталкивается ли корова с любыми препятствиями  
collisionMulti :: Obstacle o => Cow -> [o] -> Bool
collisionMulti cow os = or (map (collides cow) (cropInsideScreen os)) 


getCollisionObstacle :: Obstacle o => Cow -> [o] -> o
getCollisionObstacle cow os = (filter (collides cow) (cropInsideScreen os)) !! 0 
 
-- | Сталкивается ли корова с препятствием?
collides :: Obstacle o => Cow -> o -> Bool
collides cow o 
  | crux >= oldx && cruy >= oldy && crdx >= oldx && crdy <= oldy && 
    clux <= oldx && cluy >= oldy = True
  | crdx >= olux && crdy <= oluy && crux >= olux && cruy >= oluy && 
    cldx <= olux && cldy <= oluy = True
  | crdx >= oldx && crdy >= oldy && crux >= olux && cruy <= oluy && 
    clux <= olux && cluy >= oluy = True
  | otherwise = False
  where
    (x1,y1) = cowPosition cow
    (x2,y2) = getPosition o
    s1 = currentCowSize (cowSize cow) (cowBonus cow)
    s2 = getSize o
    (clux, cluy) = (x1 - (cowPictureSizeWidth cow) / 2 * s1, y1 + 
      (cowPictureSizeHeight cow) / 2 * s1)
    (cldx, cldy) = (x1 - (cowPictureSizeWidth cow) / 2 * s1, y1 - 
      (cowPictureSizeHeight cow) / 2 * s1)
    (crux, cruy) = (x1 + (cowPictureSizeWidth cow) / 2 * s1, y1 + 
      (cowPictureSizeHeight cow) / 2 * s1)
    (crdx, crdy) = (x1 + (cowPictureSizeWidth cow) / 2 * s1, y1 - 
      (cowPictureSizeHeight cow) / 2 * s1)
    (olux, oluy) = (x2 - (getWidth o) / 2 * s2, y2 + (getHeight o) / 2 * s2)
    (oldx, oldy) = (x2 - (getWidth o) / 2 * s2, y2 - (getHeight o) / 2 * s2)
    -- (orux, oruy) = (x2 + (getWidth o) / 2 * s2, y2 + (getHeight o) / 2 * s2)
    -- (ordx, ordy) = (x2 + (getWidth o) / 2 * s2, y2 - (getHeight o) / 2 * s2)

currentCowSize :: Size -> Bonus -> Size 
currentCowSize s (CowSizeChangeBonus csb) = s * (sizeMultiplier csb)
currentCowSize s _ = s 

-- |  Удаления обьекта, с которым столкнулись
collisionHandle :: Map -> Cow -> Map 
collisionHandle m c = m 
  { mapClovers = filter (collidesVary (cowBonus c)) (cropInsideScreen (mapClovers m)) ++ 
   dropWhile isInsideScreen (mapClovers m) 
  , mapBadBirds = filter (collidesVary (cowBonus c)) (cropInsideScreen (mapBadBirds m)) ++ 
   dropWhile isInsideScreen (mapBadBirds m) 
  , mapGoodBirds = filter (collidesVary (cowBonus c)) (cropInsideScreen (mapGoodBirds m)) ++ 
   dropWhile isInsideScreen (mapGoodBirds m) 
  , mapBonusItems = filter (not . collides c) (cropInsideScreen (mapBonusItems m)) ++ 
   dropWhile isInsideScreen (mapBonusItems m) 
  }
  where 
      collidesVary (InvincibleBonus i) = (\_ -> True)
      collidesVary _ = not . collides c
     

moveObstacles :: Map -> Float -> Map
moveObstacles m count = m 
    { mapClovers = map (\o -> setPosition o (newPosition (getPosition o) count)) (mapClovers m) 
    , mapGoodBirds = map (\o -> setPosition o (newPosition (getPosition o) count)) (mapGoodBirds m)
    , mapBadBirds = map (\o -> setPosition o (newPosition (getPosition o) count)) (mapBadBirds m)
    }
    where
      newPosition (x, y) count = (x + count, y)
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
typeRange = (1, 2)

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
