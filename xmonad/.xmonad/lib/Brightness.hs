module Brightness
       (
         incBrightness
       , decBrightness
       ) where

import System.IO.Strict as IOS (readFile)

import XMonad.Util.Run (safeSpawn)

actuallBrightness :: IO Double
actuallBrightness = read `fmap` IOS.readFile "/sys/class/backlight/intel_backlight/actual_brightness"

minBrightness :: IO Double
minBrightness = read `fmap` IOS.readFile "/sys/class/backlight/intel_backlight/bl_power"

maxBrightness :: IO Double
maxBrightness = read `fmap` IOS.readFile "/sys/class/backlight/intel_backlight/max_brightness"

actuallBrightnessFrac :: IO Double
actuallBrightnessFrac = do
  maxb <- maxBrightness
  minb <- minBrightness
  actb <- actuallBrightness
  return $ (actb - minb) / (maxb - minb)

setBrightnessIO :: Double -> IO ()
setBrightnessIO b = do
  maxb <- maxBrightness
  minb <- minBrightness
  let range = maxb - minb
      new' = (b * range) + minb
      new = (new' `min` maxb) `max` minb
  safeSpawn "sudo" ["/home/matus/bin/set-brightness", show $ floor $ new]

decBrightness :: X ()
decBrightness = liftIO $ actuallBrightnessFrac >>= \x -> setBrightnessIO $ 0.1 `max` (x - 0.1)

incBrightness :: X ()
incBrightness = liftIO $ actuallBrightnessFrac >>= \x -> setBrightnessIO (x + 0.1)

