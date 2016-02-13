{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | Current means current player.  Switch changes current player.
module Mpris
       ( switch
       , switchTo
       , toggleCurrent
       , stopCurrent
       , nextCurrent
       , previousCurrent
       , getMprisPlayer
       , formatPlayer
       , formatPlayerXmobar
       , withMprisPlayer
       , withMprisPlayers
       , Player(..)
       , module Mpris.Properties
       , module Mpris.Utils
       ) where

import Control.Monad (when)

import DBus
import DBus.Client
import qualified DBus.Mpris as MP

import System.Locale (defaultTimeLocale)

import Data.Default
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

data LastPlayer = LastPlayer (Maybe String) deriving (Typeable, Read, Show)
instance ExtensionClass LastPlayer where
  initialValue = LastPlayer Nothing
  extensionType = PersistentExtension

data MPRISPrompt = MPRISPrompt String
instance XPrompt MPRISPrompt where
    showXPrompt (MPRISPrompt s) = s ++ ": "
    commandToComplete _ = id

listNamesCall :: MethodCall
listNamesCall = (methodCall "/org/freedesktop/DBus" "org.freedesktop.DBus" "ListNames")
  { methodCallDestination = Just "org.freedesktop.DBus" }

listNames :: Client -> IO [String]
listNames client = DBus.Client.call_ client listNamesCall >>= return . unpack . head . methodReturnBody

callMpris :: (BusName -> MP.Mpris ()) -> String -> IO ()
callMpris action target =
  liftIO $ MP.mpris def (action (busName_ $ "org.mpris.MediaPlayer2." ++ target))

withCurrent :: (BusName -> MP.Mpris ()) -> X ()
withCurrent action = do
  CurrentPlayer (Just target) <- XS.get
  liftIO $ callMpris action target

stopCurrent :: X ()
stopCurrent = withCurrent MP.stop

nextCurrent :: X ()
nextCurrent = withCurrent MP.next

previousCurrent :: X ()
previousCurrent = withCurrent MP.previous

toggleCurrent :: X ()
toggleCurrent = withCurrent MP.playPause

setCurrent :: String -> X ()
setCurrent = XS.put . CurrentPlayer . Just

-- | Switch to new target.  Pause the current player and call action
-- on the new one.
switchTo :: String -> (BusName -> MP.Mpris ()) -> X ()
switchTo target action = do
  CurrentPlayer current <- XS.get
  when (isJust current) (liftIO $ callMpris MP.pause (fromJust current))
  XS.put . LastPlayer $ current
  setCurrent target
  liftIO $ callMpris action target

switch :: X ()
switch = do
  target <- mprisPlayersPrompt
  case target of
   Just "" -> do
     LastPlayer (Just l) <- XS.get
     switchTo l MP.playPause
   Just t -> switchTo t MP.playPause
   Nothing -> return ()

-- | Ask the user for a target
mprisPlayersPrompt :: X (Maybe String)
mprisPlayersPrompt = do
  players <- liftIO mprisPlayers
  mkXPromptWithReturn (MPRISPrompt "Player ") Constants.prompt (playerCompl players) (return . takeWhile (/= ' '))

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

withMprisPlayer :: Client -> BusName -> (Maybe Player -> IO ()) -> IO ()
withMprisPlayer client name action = do
   busNames <- map busName_ `fmap` listNames client
   if name `elem` busNames
     then getMprisPlayer client name >>= action . Just
     else action Nothing

getMprisPlayerNames :: Client -> IO [BusName]
getMprisPlayerNames client = do
  plist <- listNames client
  return $ L.map busName_ . L.filter (/= "org.mpris.MediaPlayer2.vlc") . L.filter (isPrefixOf "org.mpris.MediaPlayer2.") $ plist

-- TODO: move to a library
getMprisPlayers :: IO [Player]
getMprisPlayers = do
  client <- connectSession
  getMprisPlayerNames client >>= mapM (getMprisPlayer client)

withMprisPlayers :: ([Player] -> IO a) -> IO a
withMprisPlayers action = do
  players <- getMprisPlayers
  action players

-- | Return all available MPRIS clients together with information on
-- what they are playing.
mprisPlayers :: IO [String]
mprisPlayers = withMprisPlayers $ return . map formatPlayer

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
  meta ++ state' ++ if duration < 1000 -- this is in microseconds
                    then ""
                    else formatDuration duration
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
