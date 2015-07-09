{-# LANGUAGE OverloadedStrings #-}

module MPD
       ( playPlaylist
       , playArtist
       , playDirectory
       , jumpToTrack
       , Action(..)
       , deleteCurrent
       , MPD.clear
       , MPD.next
       , MPD.previous
       , MPD.stop
       , MPD.toggle
       ) where

import Control.Applicative ((<$>))
import Control.Arrow ((&&&))
import Control.Monad (when, void, mapM_, (<=<), (>=>), ap, liftM)
import Control.Monad.Trans (lift)
import Data.Function (on)
import Data.List (elemIndex, isInfixOf, groupBy, nubBy)
import Data.Maybe (fromJust, isJust)
import Data.Map (lookup)
import Data.String (fromString)
import Network.MPD as M
import Network.MPD.Commands.Extensions (addMany, listArtists, getPlaylist)
import XMonad hiding ((=?))
import XMonad.Prompt
import XMonad.Util.Run (runProcessWithInput)
import Prelude hiding (lookup)

import qualified Data.ByteString.Char8 as C

import qualified Constants
import Utils

data MPDPrompt = MPDPrompt String
instance XPrompt MPDPrompt where
    showXPrompt (MPDPrompt s) = s ++ ": "
    commandToComplete _ = id

data Action = Clear | Add deriving (Show, Eq)

playPlaylist :: Action -> X ()
playPlaylist = playGeneric
               getPlaylists
               "playlist"
               (load . PlaylistName . C.pack)

playArtist :: Action -> X ()
playArtist = playGeneric
             listArtists
             "artist"
             (\x -> findAdd (Artist =? fromString x))

playGeneric :: ToString a => MPD [a] -> String -> (String -> MPD ()) -> Action -> X ()
playGeneric getter prompt mpd action = do
  Right choices <- liftMPD getter
  Just pick <- askPrompt
               ((if action == Clear then "Play " else "Add ") ++ prompt)
               . map toString $ choices
  liftMPD_ $ do
    when (action == Clear) M.clear
    mpd pick
    when (action == Clear) $ play Nothing

playDirectory :: Action -> X ()
playDirectory action = do
  Just dir <- listDirectorySongs ""
  Just recursive <- mkXPromptWithReturn (MPDPrompt "Recursive? ") Constants.prompt (mkComplFunFromList' ["recursive", "non-recursive"]) return
  let r = recursive == "recursive" || recursive == ""
  liftMPD_ $ do
    when (action == Clear) M.clear
    songs <- getSongs r dir
    addMany "" songs
    when (action == Clear) $ play Nothing

jumpToTrack :: X ()
jumpToTrack = do
  Right choices <- liftMPD $ playlistInfo Nothing
  mkXPrompt
    (MPDPrompt "Jump to track")
    Constants.prompt
    (trackCompl choices)
    (action . (read :: String -> Int) . takeWhile (/=':'))
  where action = liftMPD_ . play . Just

trackCompl :: [Song] -> String -> IO [String]
trackCompl choices pick = return mapped
  where multiArtist = case nubBy ((==) `on` getArtist) choices of
          a:b:_ -> True
          _     -> False
        picked = filter (matchAllWords pick . strToLower . formatSong) choices
        grouped = groupBy ((==) `on` strToLower . getName) picked
        mapped = grouped >>= (\x -> case x of
                                 [s] -> [formatSong s]
                                 xs  -> map (\s -> formatSong s <%> "[" ++ getAlbum s ++ "]") xs)
        indexify = uncurry (++) . (flip (++) ": " . getIndex &&& getName)
        formatSong s = indexify s ++
                       (if (multiArtist && getArtist s /= "")
                        then " <" ++ getArtist s ++ ">"
                        else "")

---------------------------------------- song tags getters
getName (Song { sgFilePath = path
              , sgTags = tags })
  = case lookup Title tags of
  Just [x] -> toString x
  Nothing  -> toString path

getAlbum (Song { sgTags = tags })
  = case lookup Album tags of
  Just [x] -> toString x
  Nothing  -> ""

getIndex (Song { sgIndex = Just index }) = show index

getArtist (Song { sgTags = tags })
  = case lookup Artist tags of
  Just [x] -> toString x
  Nothing  -> ""

---------------------------------------- getting data
askPrompt :: String -> [String] -> X (Maybe String)
askPrompt name choices =
  mkXPromptWithReturn
    (MPDPrompt name)
    Constants.prompt
    (return . flip filter choices . (isInfixOf `on` strToLower))
    return

listDirectorySongs :: String -> X (Maybe String)
listDirectorySongs path = do
  d <- liftMPD $ liftM (map (getFileName . toString)) . getDirectories $ path
  case d of
    Left _     -> return . Just $ path
    Right []   -> return . Just $ path
    Right dirs -> do
      picked <- askPrompt "Find directory" dirs
      case picked of
        Nothing   -> return Nothing
        Just ""   -> return . Just $ path
        Just pick -> do
          let new = if path == "" then pick else path ++ "/" ++ pick
          listDirectorySongs new

getDirectories :: String -> MPD [Path]
getDirectories = liftM getDirsFromResults . lsInfo . fromString

getPlaylists :: MPD [PlaylistName]
getPlaylists = liftM getPlaylistsFromResults . lsInfo . fromString $ ""

getSongs :: Bool -> String -> MPD [Path]
getSongs recursive path = do
  let listFn = if recursive then listAllInfo else lsInfo
  songs <- liftM getSongsFromResults . listFn . fromString $ path
  return [x | Song {sgFilePath = x} <- songs]

---------------------------------------- projections from results
getSongsFromResults :: [LsResult] -> [Song]
getSongsFromResults r = [x | (LsSong x) <- r]

getDirsFromResults :: [LsResult] -> [Path]
getDirsFromResults r = [x | (LsDirectory x) <- r]

getPlaylistsFromResults :: [LsResult] -> [PlaylistName]
getPlaylistsFromResults r = [x | (LsPlaylist x) <- r]

---------------------------------------- other utils
liftMPD :: MonadIO m => MPD a -> m (Response a)
liftMPD = liftIO . withMPD

liftMPD_ :: MonadIO m => MPD a -> m ()
liftMPD_ = liftM (const ()) . liftMPD

---------------------------------------- playlist control

deleteCurrent :: X ()
deleteCurrent = liftMPD_ $ do
  Just (Song { sgIndex = Just index }) <- currentSong
  delete index

clear :: X ()
clear = liftMPD_ M.clear

next :: X ()
next = liftMPD_ M.next

previous :: X ()
previous = liftMPD_ M.previous

stop :: X ()
stop = liftMPD_ M.stop

toggle :: X ()
toggle = liftMPD_ $ do
  Status { stState = s } <- status
  case s of
    Playing -> pause True
    Paused  -> play Nothing
    Stopped -> play Nothing
