import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as B
import Data.Foldable
import System.Process
import Text.Printf

type Second = Float
type Sample = Float
type Hz = Float
type Pulse = Float
type Semitone = Float
type Beat = Float

outputFilePath :: FilePath
outputFilePath = "output.bin"

volume :: Float
volume = 0.5

bpm :: Beat
bpm = 105.0 * 2

beatDuration :: Second
beatDuration = 60.0 / bpm

sampleRate :: Sample
sampleRate = 48000.0

pitchStandard :: Hz
pitchStandard = 440.0 -- A4

linearSpace :: Int -> Float -> Float -> [Float]
linearSpace n start stop = [start, start + step .. stop]
  where
    step = (stop - start) / (fromIntegral n)

semitoneToHz :: Semitone -> Hz
semitoneToHz n = pitchStandard * (a ** n)
  where
    a = 2.0 ** (1.0/12.0)

envelope :: [Pulse] -> [Pulse]
envelope wave = zipWith (*) wave modifier
  where 
    modifier :: [Float]
    modifier  = (linearSpace nAttack 0.0 1.0) ++ 
      (linearSpace nDecay 1.0 sustainLevel) ++ 
      (replicate nSustain sustainLevel) ++ 
      (linearSpace nRelease sustainLevel 0.0)
    nAttack   = round $ (* fromIntegral n) $ (0.05 / 1.405)
    nDecay    = round $ (* fromIntegral n) $ (0.1 / 1.405)
    nRelease  = round $ (* fromIntegral n) $ (1 / 1.405)
    nSustain  = n - nAttack - nDecay - nRelease
    sustainLevel = 0.8
    n = length wave

instrumentWaveTransform :: Hz -> Float -> Float
-- instrumentWaveTransform frequency x = sin(x * step)
instrumentWaveTransform frequency x = y
  where
    u = (frequency / sampleRate) * 2 * pi * x
    y = sum [sin((2*k - 1)*u)/(2*k - 1) | k <- [1 .. 10]]

freq :: Hz -> Second -> [Pulse]
freq frequency duration =  map (* volume) $ envelope output
  where 
    output = map (instrumentWaveTransform frequency) [0.0 .. sampleRate * duration]

note :: Semitone -> Beat -> [Pulse]
note n beat = freq (semitoneToHz n) (beat * beatDuration)

pause :: Beat -> [Pulse]
pause beat = map (* 0) $ freq 0 (beat * beatDuration)

caveStoryRightHand :: [Pulse]
caveStoryRightHand = concat [
  note 3 1,
  note 5 1,
  note 3 1,
  note 5 1,

  note 3 1,
  note 5 1,
  note 3 1,
  note 5 1,

  note 2 1,
  note 5 1,
  note 2 1,
  note 5 1,
  
  note 2 1,
  note 5 1,
  note 2 1,
  note 2 0.5,
  note 3 0.5,

  note 3 1,
  note 5 1,
  note 3 1,
  note 5 1,

  note 3 1,
  note 5 1,
  note 3 1,
  note 5 1,

  note 2 1,
  note 5 1,
  note 2 1,
  note 5 1,
  
  note 8 1,
  note 7 1,
  note 6 1,
  pause 1,
  
  note 3 1,
  note 4 0.5,
  note 4 0.5,
  note 5 0.5,
  note 6 0.5,
  note 5 0.5,
  note 4 0.5,
  
  note 3 1,
  pause 1,
  note 5 0.5,
  note 6 0.5,
  note 5 0.5,
  note 4 0.5,
  
  note 3 1,
  note 1 1, 
  note 5 0.5, 
  note 5 0.5, 
  note 4 0.5, 
  note 3 0.5, 
  
  note 4 1,
  note 2 1,
  note 6 0.5,
  note 6 0.5,
  note 4 1,
  
  note 3 1,
  note 4 0.5,
  note 4 0.5,
  note 5 0.5,
  note 6 0.5,
  note 5 0.5,
  note 4 0.5,
  
  note 3 1,
  pause 1,
  note 5 0.5,
  note 6 0.5,
  note 5 0.5,
  note 4 0.5,
  
  note 3 1,
  note 1 1,
  note 5 0.5,
  note 5 0.5,
  note 4 0.5,
  note 3 0.5,
  
  note 4 1,
  note 2 1,
  note 6 0.5,
  note 2 0.5,
  note 3 1,
  
  pause 0
  ]
  
caveStoryLeftHand :: [Pulse]
caveStoryLeftHand = concat [

  pause 0
  ] ++ (concat $ replicate 1000 (pause 1))

  
wave :: [Pulse]
wave = concat [
  note (-48) 1,
  note (-47) 1,
  note (-46) 1,
  note (-45) 1,
  note (-44) 1,
  note (-43) 1,
  note (-42) 1,
  note (-41) 1,
  note (-40) 1,
  note (-39) 1,
  note (-38) 1,
  note (-37) 1,
  note (-36) 1,
  note (-35) 1,
  note (-34) 1,
  note (-33) 1,
  note (-32) 1,
  note (-31) 1,
  note (-30) 1,
  note (-29) 1,
  note (-28) 1,
  note (-27) 1,
  note (-26) 1,
  note (-25) 1,
  note (-24) 1,
  note (-23) 1,
  note (-22) 1,
  note (-21) 1,
  note (-20) 1,
  note (-19) 1,
  note (-18) 1,
  note (-17) 1,
  note (-16) 1,
  note (-15) 1,
  note (-14) 1,
  note (-13) 1,
  note (-12) 1,
  note (-11) 1,
  note (-10) 1,
  note (-9) 1,
  note (-8) 1,
  note (-7) 1,
  note (-6) 1,
  note (-5) 1,
  note (-4) 1,
  note (-3) 1,
  note (-2) 1,
  note (-1) 1,
  note (0) 1,
  note (1) 1,
  note (2) 1,
  note (3) 1,
  note (4) 1,
  note (5) 1,
  note (6) 1,
  note (7) 1,
  note (8) 1,
  note (9) 1,
  note (10) 1,
  note (11) 1,
  note (12) 1,
  note (13) 1,
  note (14) 1,
  note (15) 1,
  note (16) 1,
  note (17) 1,
  note (18) 1,
  note (19) 1,
  note (20) 1,
  note (21) 1,
  note (22) 1,
  note (23) 1,
  note (24) 1,
  note (25) 1,
  note (26) 1,
  note (27) 1,
  note (28) 1,
  note (29) 1,
  note (30) 1,
  note (31) 1,
  note (32) 1,
  note (33) 1,
  note (34) 1,
  note (35) 1,
  note (36) 1,
  note (37) 1,
  note (38) 1,
  note (39) 1,
  note 0 0
  ]
  
save :: FilePath -> [Pulse] -> IO ()
save filePath wave = B.writeFile filePath $ B.toLazyByteString $ fold $ map B.floatLE wave
  
play :: IO ()
play = do
  save outputFilePath wave
  _ <- runCommand $ printf "ffplay -showmode 1 -f f32le -ar %f %s" sampleRate outputFilePath
  return ()
  
playCaveStory :: IO()
playCaveStory = do
  save outputFilePath $ zipWith (+) caveStoryRightHand caveStoryLeftHand
  _ <- runCommand $ printf "ffplay -showmode 2 -f f32le -ar %f %s" sampleRate outputFilePath
  return ()