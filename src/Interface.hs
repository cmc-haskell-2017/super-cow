module Interface where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Juicy 
import Game.Player
import Game.Bonus
import Game.Obstacle
import Type
import Const
import Game.Universe

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
    , imageDonut             = scale 0.1 0.1 donutPicture
    , imageDonutStar         = scale 0.1 0.1 donutStarPicture
    , imageFasterStar        = scale 0.1 0.1 fasterStarPicture
    , imageInvincibleStar    = scale 0.1 0.1 invincibleStarPicture
    , imageRandomStar        = scale 0.1 0.1 randomStarPicture
    , imageEnlargeStar       = scale 0.1 0.1 enlargeStarPicture
    }

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
  , drawDonuts images (cowBonus $ universeCow universe)
  ]

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

-- | Нарисовать корову
drawCow :: Images -> Cow -> Picture
drawCow images cow = case cowBonus cow of
  BirdSpeedChangeBonus _ -> translate x y (rotate (cowAngel cow) (scale r r (imageCow images)))
    where
      (x, y) = cowPosition cow
      r = cowSize cow
  InvincibleBonus _ -> drawCowWithBonus images cow (cowBonus cow)
  CowSizeChangeBonus _ -> drawCowWithBonus images cow (cowBonus cow)
  _ -> translate x y (rotate (cowAngel cow) (scale r r (imageCow images)))
    where
      (x, y) = cowPosition cow
      r = cowSize cow

-- uncurry :: (a -> b -> c) -> (a, b) ->
drawCowWithBonus :: Images -> Cow -> Bonus -> Picture
drawCowWithBonus images cow (InvincibleBonus _)
  = translate (fst (cowPosition cow)) (snd (cowPosition cow)) (rotate (cowAngel cow) (scale (cowSize cow) (cowSize cow) (imageCowBlurred images)))
drawCowWithBonus images cow (CowSizeChangeBonus i)
  = translate (fst (cowPosition cow)) (snd (cowPosition cow)) (rotate (cowAngel cow) (scale newCowSize newCowSize (imageCow images)))
  where
    newCowSize = (cowSize cow) * (sizeMultiplier i)
drawCowWithBonus images cow _  = drawCow images cow
 

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
 
drawGameOver :: Picture -> Bool -> Picture
drawGameOver _ False = blank
drawGameOver image True = translate (-w) h (scale 1.0 1.0 image)
  where
    w = 0
    h = screenTop / 2
    
drawBonusItem :: Images -> BonusItem -> BonusType -> Picture
drawBonusItem i bi _ 
  | hidden bi = draw (imageRandomStar i) bi
drawBonusItem i bi Inv = draw (imageInvincibleStar i) bi
drawBonusItem i bi SizeChange = draw (imageEnlargeStar i) bi
drawBonusItem i bi BirdSpeed = draw (imageFasterStar i) bi
drawBonusItem i bi Gun = draw (imageDonutStar i) bi


drawDonuts :: Images -> Bonus -> Picture
drawDonuts images (DonutGunBonus dgb) = 
  pictures (map (draw (imageDonut images)) (allDonuts dgb))
drawDonuts _ _ = blank

-- | Нарисовать препятствие
draw :: Obstacle o => Picture -> o -> Picture
draw image o = translate x y (scale r r image)
  where
    (x, y) = getPosition o
    r = getSize o
    
-- | Оставить только те картинки фона, которые входят в экран
cropBackgroundInsideScreen :: [BackgroundPicture] -> [BackgroundPicture]
cropBackgroundInsideScreen background = dropWhile (\o -> pos o < screenLeft) $
  takeWhile (\o -> pos o < screenRight) background
  where
    pos = fst . backgroundPicturePosition
