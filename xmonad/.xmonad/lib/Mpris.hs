{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | Current means current player.  Toggle changes current player.
module Mpris
       ( toggle
       , toggleCurrent
       , stopCurrent
       , nextCurrent
       , previousCurrent
       , getMprisPlayer
       , formatPlayer
       , formatPlayerXmobar
       , withMprisPlayers
       , Player(..)
       , module Mpris.Properties
       , module Mpris.Utils
       ) where

import Control.Monad (when)

import DBus
import DBus.Client

import System.Locale (defaultTimeLocale)

import Data.Maybe (isJust, fromJust, fromMaybe)
import Data.List as L
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Format (formatTime)
import qualified Data.Text as T

import XMonad hiding ((=?), title)
import XMonad.Prompt
import qualified XMonad.Util.ExtensibleState as XS

import Mpris.Properties
import Mpris.Utils (unpack)

import Utils
import Constants

data CurrentPlayer = CurrentPlayer (Maybe String) deriving (Typeable, Read, Show)
instance ExtensionClass CurrentPlayer where
  initialValue = CurrentPlayer Nothing
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
  CurrentPlayer (Just target) <- XS.get
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
  when (isJust current) (liftIO $ callPause (fromJust current))
  XS.put (CurrentPlayer (Just target))
  liftIO $ callPlayPause target

mprisPlayersPrompt :: X (Maybe String)
mprisPlayersPrompt = do
  players <- liftIO mpris
  mkXPromptWithReturn (MPRISPrompt "Player ") Constants.prompt (playerCompl players) return

playerCompl :: [String] -> String -> IO [String]
playerCompl players pick = return $ L.filter (matchAllWords pick . strToLower) players

data Player = Player { player   :: BusName
                     , status   :: PlayerStatus
                     , author   :: Maybe String
                     , title    :: Maybe String
                     , file     :: Maybe String
                     , duration :: Integer
                     , seek     :: Integer
                     } deriving (Eq, Show)

getMprisPlayer :: Client -- | dbus client
                  -> BusName -- | dbus busname
                  -> IO Player
getMprisPlayer client name = do
  m <- getMetadata client name
  pos <- getPosition client name
  status <- getStatus client name
  return Player { player = name
                , status = status
                , author = getAuthor m
                , title  = getTitle m
                , file   = getUrl m
                , duration = getLength m
                , seek = pos
                }

-- TODO: move to a library
getMprisPlayers :: IO [Player]
getMprisPlayers = do
  client <- connectSession
  rep <- call_ client listNamesCall
  let plist = unpack $ head (methodReturnBody rep)
      players = L.filter (/= "org.mpris.MediaPlayer2.vlc") . L.filter (isPrefixOf "org.mpris.MediaPlayer2.") $ plist
  mapM (getMprisPlayer client . busName_) players

withMprisPlayers :: ([Player] -> IO a) -> IO a
withMprisPlayers action = do
  players <- getMprisPlayers
  action players

-- | Return all available MPRIS clients together with information on
-- what they are playing.
mpris :: IO [String]
mpris = withMprisPlayers $ return . map formatPlayer

formatPlayer :: Player -> String
formatPlayer Player { player = player
                    , author = author
                    , title = title
                    , file = file
                    , duration = duration
                    , seek = seek } =
  (drop 23 $ formatBusName player) ++ " " ++ time ++ " " ++ meta
  where meta = case title of
          Just t -> t ++ ": " ++ fromMaybe "" author
          Nothing ->
            case file of
              Just f -> formatURL f
              Nothing -> ""
        time = "[" ++ formatDuration seek ++ "/" ++ formatDuration duration ++ "]"

formatPlayerXmobar :: Player -> String
formatPlayerXmobar Player { status = status
                          , author = author
                          , title = title
                          , file = file
                          , duration = duration
                          , seek = seek } =
  meta ++ state' ++ formatDuration duration
  where meta = case title of
          Just t -> "<fc=#888a85>" ++ t ++
                    "</fc><fc=#729fcf>" ++ fromMaybe "" author ++ "</fc>"
          Nothing ->
            case file of
              Just f -> "<fc=#888a85>" ++ formatURL f ++ "</fc>"
              Nothing -> "Unknown"
        seek' =  formatDuration seek
        state' = case status of
          Playing -> "<fc=#8ae234>" ++ seek' ++ "</fc>"
          Paused -> "<fc=#edd400>" ++ seek' ++ "</fc>"
          Stopped -> "<fc=#ef2929>" ++ seek' ++ "</fc>"

formatDuration :: Integer -> String
formatDuration dur = formatTime defaultTimeLocale "%M:%S" durInSec
  where durInSec = posixSecondsToUTCTime . fromIntegral $ (dur `div` 1000000)

-- | Return file portion of URL if file:///, otherwise do nothing
formatURL :: String -> String
formatURL url = if "file:///" `isPrefixOf` url
                 then T.unpack . T.replace "%20" " " . T.pack . reverse . takeWhile (/= '/') . reverse $ url
                 else url
