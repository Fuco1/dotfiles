{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | Current means current player.  Toggle changes current player.
module Mpris
       ( toggle
       , toggleCurrent
       , stopCurrent
       , nextCurrent
       , previousCurrent
       ) where

import DBus
import DBus.Client

import System.Locale (defaultTimeLocale)

import Data.List as L
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Format (formatTime)
import qualified Data.Text as T

import XMonad hiding ((=?))
import XMonad.Prompt
import qualified XMonad.Util.ExtensibleState as XS

import Mpris.Properties
import Mpris.Utils (unpack)

import Utils
import Constants

data CurrentPlayer = CurrentPlayer String deriving (Typeable, Read, Show)
instance ExtensionClass CurrentPlayer where
  initialValue = CurrentPlayer ""
  extensionType = PersistentExtension

data MPRISPrompt = MPRISPrompt String
instance XPrompt MPRISPrompt where
    showXPrompt (MPRISPrompt s) = s ++ ": "
    commandToComplete _ = id

listNamesCall :: MethodCall
listNamesCall = (methodCall "/org/freedesktop/DBus" "org.freedesktop.DBus" "ListNames")
  { methodCallDestination = Just "org.freedesktop.DBus" }

mprisPlayerCall :: String -> String -> MethodCall
mprisPlayerCall name target = (methodCall "/org/mpris/MediaPlayer2" "org.mpris.MediaPlayer2.Player" (memberName_ name))
  { methodCallDestination = Just . busName_ $ "org.mpris.MediaPlayer2." ++ target }

-- | Member name, target
callMpris :: String -> String -> IO ()
callMpris member target = do
  client <- connectSession
  callNoReply client $ mprisPlayerCall member target

callPlayPause :: String -> IO ()
callPlayPause = callMpris "PlayPause"

callStop :: String -> IO ()
callStop = callMpris "Stop"

callPause :: String -> IO ()
callPause = callMpris "Pause"

callNext :: String -> IO ()
callNext = callMpris "Next"

callPrevious :: String -> IO ()
callPrevious = callMpris "Previous"

withCurrent :: (String -> IO ()) -> X ()
withCurrent action = do
  CurrentPlayer target <- XS.get
  liftIO $ action target

stopCurrent :: X ()
stopCurrent = withCurrent callStop

nextCurrent :: X ()
nextCurrent = withCurrent callNext

previousCurrent :: X ()
previousCurrent = withCurrent callPrevious

toggleCurrent :: X ()
toggleCurrent = withCurrent callPlayPause

toggle :: X ()
toggle = do
  Just player <- mprisPlayersPrompt
  let target = takeWhile (/= ' ') player
  CurrentPlayer current <- XS.get
  liftIO $ callPause current
  XS.put (CurrentPlayer target)
  liftIO $ callPlayPause target

mprisPlayersPrompt :: X (Maybe String)
mprisPlayersPrompt = do
  players <- liftIO mpris
  mkXPromptWithReturn (MPRISPrompt "Recursive? ") Constants.prompt (playerCompl players) return

playerCompl :: [String] -> String -> IO [String]
playerCompl players pick = return $ L.filter (matchAllWords pick . strToLower) players

-- | Return all available MPRIS clients together with information on
-- what they are playing.
mpris :: IO [String]
mpris = do
  client <- connectSession
  rep <- call_ client listNamesCall
  let plist = unpack $ head (methodReturnBody rep)
      players = L.filter (/= "org.mpris.MediaPlayer2.vlc") . L.filter (isPrefixOf "org.mpris.MediaPlayer2.") $ plist
  playing <- mapM (\x -> do
    m <- getMetadata client x
    pos <- getPosition client x
    return (m,pos)) players
  let candidates = L.map (\(player, (m, pos)) -> drop 23 player ++ " " ++ formatPlayerInfo m pos) (zip players playing)
  return candidates

-- | Metadata, seek position
formatPlayerInfo :: Metadata -> Integer -> String
formatPlayerInfo m seek = s ++ " " ++ desc
  where author = getAuthor m
        desc = if author == ""
               then formatURL . getUrl $ m
               else author ++ ": " ++ getTitle m
        s = "[" ++ formatDuration seek ++ "/" ++ formatDuration (getLength m) ++ "]"


formatDuration :: Integer -> String
formatDuration dur = formatTime defaultTimeLocale "%M:%S" durInSec
  where durInSec = posixSecondsToUTCTime . fromIntegral $ (dur `div` 1000000)

-- | Return file portion of URL if file:///, otherwise do nothing
formatURL :: String -> String
formatURL url = if "file:///" `isPrefixOf` url
                 then T.unpack . T.replace "%20" " " . T.pack . reverse . takeWhile (/= '/') . reverse $ url
                 else url
