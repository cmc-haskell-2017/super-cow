module SuperCow where

import System.Random
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Line
import Graphics.Gloss.Interface.Pure.Game

-- | Запустить игру «Super Cow»
runSuperCow :: IO ()
runSuperCow = do
  g <- newStdGen
  play display bgColor fps (initUniverse g) drawUniverse handleUniverse updateUniverse
  where
    display = InWindow "Super Cow" (screenWidth, screenHeight) (200, 200)
    bgColor = white   -- цвет фона
    fps     = 60      -- кол-во кадров в секунду


-- Структуры данных
-- Высота и Положение объектов
type Height = Float -- Высота обьекта
type Offset = Float -- Сдвиг обьекта
type Position = (Offset,Height)  -- Координаты обьекта
type Life = Int -- Жизни (изначально 3)
type Score = Int -- Счет (изменяется постоянно)

-- Объекты игровой вселенной
data Clover = Clover  -- Клевер - добавляет одну жизнь
  { cloverPosition :: Position
  , cloverSize :: Float
  }
     
data BadBird = BadBird -- Плохая птичка - снимает 2 жизни 
  { badBirdPosition :: Position
  , badBirdSize     :: Float
  }
  H
data GoodBird = GoodBird -- Хорошая птичка - снимает 1 жизни
  { goodBirdPosition :: Position
  , goodBirdSize     :: Float
  }
  
data Map = Map 
  { mapGoodBirds :: [GoodBird]
  , mapBadBirds  :: [BadBird]
  , mapClovers   :: [Clover]
  }

-- Корова
data Cow = Cow
  { cowPosition :: Position
  , cowSize     :: Float
  }

-- Игровая вселенная
data Universe = Universe
  { universeMap       :: Map   -- Препятствия игровой вселенной
  , universeCow       :: Cow   -- Корова
  , universeScore     :: Score    --  Cчет
  , universeLife      :: Life    --  Жизни
  }

-- Инициализация вселенной (Дана)
-- Инициализировать игровую вселенную, используя генератор случайных значений
initUniverse :: StdGen -> Universe
initUniverse g = Universe
  { universeMap  = initMap g 
  , universeCow = initCow
  , universeScore  = 0
  , universeLife  = 3
  }

-- Реализация препятствий
class Obstacle o where 
    -- Нарисовать одно препятствие (Ралина)
    draw :: Picture -> o -> Picture
    -- Сталкивается ли корова с препятствием? (Денис)
    collides :: Cow -> o -> Bool
    -- Получение позиции
    getPosition :: o -> Position
    -- Получние размера
    getSize :: o -> Float
    
    
instance Obstacle Clover where    
    draw image clover = translate x y (scale r r image)
        where
            (x, y) = cloverPosition clover
            r = cloverSize clover
            
    collides (Cow (x1,y1)) clover 
        | x1 == x2 && y1 == y2 = True
        | otherwise = False
        where
            (x2,y2) = cloverPosition clover
    
    getPosition clover = cloverPosition clover
    
    getSize clover = cloverSize clover
    
instance Obstacle BadBird where
    draw image badbird = translate x y (scale r r image)
        where
            (x, y) = badBirdPosition badbird
            r = badBirdSize badbird
    
    collides (Cow (x1,y1)) badbird 
        | x1 == x2 && y1 == y2 = True
        | otherwise = False
        where
            (x2,y2) = badBirdPosition badbird
        
    getPosition badbird = badBirdPosition badbird
    
    getSize badbird = badBirdSize badbird
        
instance Obstacle GoodBird where 
    draw image goodbird = translate x y (scale r r image)
        where
            (x, y) = goodBirdPosition goodbird
            r = goodBirdSize goodbird
    
    collides (Cow (x1,y1)) goodbird 
        | x1 == x2 && y1 == y2 = True
        | otherwise = False
        where
            (x2,y2) = goodBirdPosition goodbird
            
    getPosition goodbird = goodBirdPosition goodbird
    
    getSize goodbird = goodBirdSize goodbird

-- Инициализировать клевер
initClover :: Position -> Clover
initClover p = Clover 
    { cloverPosition = p
    , cloverSize = defaultCloverSize
    }

-- Инициализировать плохую птичку 
initBadBird :: Position -> BadBird
initBadBird p = BadBird
    { badBirdPosition = p 
    , badBirdSize = defaultBadBirdSize
    }
    
-- Инициализировать хорошую птичку
initGoodBird :: Position -> GoodBird
initGoodBird p = GoodBird
    { goodBirdPosition = p 
    , goodBirdSize = defaultGoodBirdSize
    }

-- Инициализировать карту препятствий (Дана)
--TODO: можно изменить defaultOffset для препятствий разного типа
initMap :: StdGen -> Map
initMap g = Map 
  { mapGoodBirds = map initGoodBirdBird positions_1
    mapClovers = map initClover positions_2
    mapBadBirds = map initBadBird positions_3
  }
  where
    positions_1 = zip [screenLeft, screenLeft + defaultOffset .. ] (randomRs ObstacleHeightRange g)
    positions_2 = zip [screenLeft, screenLeft + defaultOffset .. ] (randomRs ObstacleHeightRange g)
    positions_3 = zip [screenLeft, screenLeft + defaultOffset .. ] (randomRs ObstacleHeightRange g)

-- Инициализировать корову (Дана)
initCow :: Cow
initCow = Cow 
    { cowPosition = (cowInitHeight, cowInitOffset)
    , cowSize = defaultCowSize
    }

-- Отрисовка игровой вселенной
-- Отобразить игровую вселенную (Ралина)
drawUniverse :: Universe -> Picture

-- Нарисовать корову (Ралина)
drawCow :: Cow -> Picture

-- Нарисовать счёт в левом верхнем углу экрана (Ралина)
drawScore :: Score -> Picture

-- Нарисовать счёт в левом верхнем углу экрана (Ралина)
drawLife :: Life -> Picture

-- Отобразить все препятствия игровой вселенной, вмещающихся в экран (Ралина)
drawObstacles :: Obstacle o => [o] -> Picture

-- Оставить только те препятствия, которые входят в экран 
cropInsideScreen :: Obstacle o => [o] -> [o]

-- | Обработчик событий игры(Дана)
handleUniverse :: Event -> Universe -> Universe
handleUniverse (EventKey (SpecialKey KeySpace) Down _ _) = goUp
handleUniverse (EventKey (SpecialKey KeySpace) Up _ _) = goDown
handleUniverse _ = id

-- | Передвижение коровы вверх, если можно.
goUp :: Universe -> Universe
goUp u = u
  { universeCow = Cow 
        {cowPosition = updatePositions cowPosition universeCow u
        , cowSize = cowSize universeCow u
        }
  }
  where
    updatePositions (offset, height) = (offset, min h (height + gameSpeed))
    h = fromIntegral screenHeight / 2

-- | Передвижение коровы вниз, если можно.
goDown :: Universe -> Universe
goDown u = u
  { universeCow = Cow 
        {cowPosition = updatePositions cowPosition universeCow u
        , cowSize = cowSize universeCow
        }
  }
  where
    updatePositions (offset, height) = (offset, min -h (height - gameSpeed))
    h = fromIntegral screenHeight / 2


-- Сталкивается ли корова с любыми препятствиями (Денис 
collisionMulti :: Obstacle o => Cow -> [o] -> Bool
collisionMulti cow os = foldr1 (&&) (map (collides cow) (cropInsideScreen os)) 
    
-- Обновление игровой вселенной
-- Обновить состояние игровой вселенной (Валера)
updateUniverse :: Float -> Universe -> Universe

-- Обновить состояние коровы (Валера)
-- updateCow :: Float -> Cow -> Cow

-- Изменить положение коровы, если можно (Дана)
-- заменено на функции goUp и goDown, которые непосредственно меняют cowPositions
-- moveCow :: Universe -> Universe

-- Обновить карту игровой вселенной (Валера)
updateMap :: Float -> Map -> Map

-- Обновить препятствия игровой вселенной (Валера)
updateObstacles :: Obstacle o => Float -> [o] -> [o]

-- Обновить счет (Валера)
updateScore :: Float -> Score -> Score

-- Обновить жизни (Валера)
updateLife :: Float -> Life -> Life

-- Текущая скорость движения игрока по вселенной (троится по времени и изначальной скорости)
сurrentSpeed :: Float -> Float -> Float


-- Константы, параметры игры
-- Ширина экрана
screenWidth :: Int
screenWidth = 800

-- Высота экрана
screenHeight :: Int
screenHeight = 450

-- Положение правого края экрана
screenRight :: Offset
screenRight = fromIntegral screenWidth / 2

-- Положение левого края экрана
screenLeft :: Offset
screenLeft = - fromIntegral screenWidth / 2

-- Положение верхнего края экрана
screenTop :: Height
screenTop = fromIntegral screenHeight / 2

-- Положение нижнего края экрана
screenBottom :: Height
screenBottom = - fromIntegral screenHeight / 2

-- Расстояние между препятствиями
defaultOffset :: Offset
defaultOffset = 300

defaultCloverSize :: Float
defaultCloverSize = 1

defaultBadBirdSize :: Float
defaultBadBirdSize = 1

defaultGoodBirdSize :: Float
defaultGoodBirdSize = 1

defaultCowSize :: Float
defaultCowSize = 1

-- | Диапазон высот препятствий.
obstacleHeightRange :: (Height, Height)
obstacleHeightRange = (-h, h)
  where
    h = (fromIntegral screenHeight) / 2 --необходимо чекнуть

-- Изначальная скорость движения игрока по вселенной - абсолютное изменение 
-- изменение высоты игрока при нажатии на клавиши (в пикселях)
gameSpeed :: Float
gameSpeed = 100

-- Положение коровы по горизонтали
cowInitOffset :: Offset
cowInitOffset = screenLeft + 100

-- Положение коровы по вертикали
cowInitHeight :: Height
cowInitHeight = screenBottom + 200
