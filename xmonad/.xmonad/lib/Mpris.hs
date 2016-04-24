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
       , formatPlayer
       ) where

import Control.Applicative ((<$>))
import Control.Monad (when)

import DBus
import qualified DBus.Mpris as MP

import System.Locale (defaultTimeLocale)

import Data.Default
import Data.Maybe (isJust, fromJust, fromMaybe)
import Data.List as L
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Format (formatTime)

import Network.HTTP (urlDecode)

import XMonad hiding ((=?), title)
import XMonad.Prompt
import qualified XMonad.Util.ExtensibleState as XS

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

callMpris :: (BusName -> MP.Mpris ()) -> String -> IO ()
callMpris action target = liftIO $ MP.mpris def (action (busName_ target))

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

mprisPlayersPrompt :: X (Maybe String)
mprisPlayersPrompt = do
  players <- map formatPlayer <$> liftIO (MP.mpris def MP.getPlayers)
  mkXPromptWithReturn (MPRISPrompt "Player ") Constants.prompt (playerCompl players) (return . takeWhile (/= ' '))

playerCompl :: [String] -> String -> IO [String]
playerCompl players pick = return $ L.filter (matchAllWords pick . strToLower) players


formatPlayer :: MP.Player -> String
formatPlayer MP.Player { MP.playerBusname = bus
                       , MP.playerName = name
                       , MP.playerPosition = position
                       , MP.playerMetadata =
                         Just (MP.Metadata { MP.artist = artist
                                           , MP.title = title
                                           , MP.len = len
                                           , MP.url = url
                                           }) } = formatBusName bus ++ " " ++ fromMaybe "" name ++ " " ++ time ++ " " ++ meta
  where time = case (position, len) of
                (Just p, Just l) -> "[" ++ formatDuration p ++ "/" ++ formatDuration l ++ "]"
                _ -> ""
        meta = case title of
                Just t -> t ++ ": " ++ fromMaybe "" artist
                Nothing -> case url of
                  Just u -> formatURL u
                  Nothing -> ""
formatPlayer MP.Player { MP.playerBusname = bus
                       , MP.playerName = name
                       , MP.playerMetadata = Nothing }
  = formatBusName bus ++ " " ++ fromMaybe "" name

formatDuration :: Integer -> String
formatDuration dur = formatTime defaultTimeLocale "%M:%S" durInSec
  where durInSec = posixSecondsToUTCTime . fromIntegral $ (dur `div` 1000000)

-- | Return file portion of URL if file:///, otherwise do nothing
formatURL :: String -> String
formatURL url = if "file:///" `isPrefixOf` url
                 then urlDecode . reverse . takeWhile (/= '/') . reverse $ url
                 else url
