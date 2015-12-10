{-# LANGUAGE OverloadedStrings #-}

import Mpris

import Control.Concurrent
import DBus
import DBus.Client
import Data.IORef
import System.IO

mprisEventMatcher :: MatchRule
mprisEventMatcher = matchAny
  { matchSender = Nothing
  , matchDestination = Nothing
  , matchPath = Just "/org/mpris/MediaPlayer2"
  , matchInterface = Just "org.freedesktop.DBus.Properties"
  , matchMember = Just "PropertiesChanged"
  }

mprisCallBack :: IORef (Maybe Player) -> Client -> Signal -> IO ()
mprisCallBack currentPlayer client s = do
  let Just sender = signalSender s
  let items = dictionaryItems $ unpack (signalBody s !! 1)
  case lookup (toVariant ("PlaybackStatus" :: String)) items of
    Just a ->
      case (unpack . unpack) a :: String of
        "Playing" -> getMprisPlayer client sender >>= writeIORef currentPlayer . Just
        _ -> return ()
    Nothing -> return ()

findInitialActivePlayer :: IO (Maybe Player)
findInitialActivePlayer =
  withMprisPlayers $ \players ->
    return $ case filter isPlaying players of
      x:_ -> Just x
      _   -> Nothing
  where isPlaying (Player { status = status }) = status == Playing

main :: IO ()
main = do
  currentPlayer <- newIORef Nothing
  client <- connectSession
  findInitialActivePlayer >>= writeIORef currentPlayer
  addMatch client mprisEventMatcher (mprisCallBack currentPlayer client)
  hSetBuffering stdout LineBuffering
  loop currentPlayer client

loop :: IORef (Maybe Player) -> Client -> IO ()
loop currentPlayer client = do
  threadDelay 800000
  currentPlayer' <- readIORef currentPlayer
  case currentPlayer' of
    Just p -> withMprisPlayer client (Mpris.player p) printPlayer
    Nothing -> return ()
  loop currentPlayer client
  where printPlayer (Just p) = putStrLn $ formatPlayerXmobar p
        printPlayer Nothing = writeIORef currentPlayer Nothing >> putStrLn "Player died"
