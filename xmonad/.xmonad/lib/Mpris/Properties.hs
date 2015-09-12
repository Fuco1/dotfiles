{-# LANGUAGE OverloadedStrings #-}
module Mpris.Properties
       ( getPosition
       , getMetadata
       , getAuthor
       , getUrl
       , getTitle
       , getLength
       , Metadata
       ) where

import DBus
import DBus.Client

import Data.Int (Int64)
import Data.Map as M

import Mpris.Utils

getProperty :: Client -> String -> String -> IO MethodReturn
getProperty client name property =
  call_ client (methodCall "/org/mpris/MediaPlayer2" "org.freedesktop.DBus.Properties" "Get")
    { methodCallDestination = Just (busName_ name)
    , methodCallBody = [ toVariant ("org.mpris.MediaPlayer2.Player" :: String),
                         toVariant property ]
    }

getPosition :: Client -> String -> IO Integer
getPosition client destination = do
  reply <- getProperty client destination "Position"
  return . fromIntegral $ ((unpack . unpack . head . methodReturnBody $ reply) :: Int64)

type Metadata = Map String Variant

getMetadata :: Client -> String -> IO Metadata
getMetadata client destination = do
  reply <- getProperty client destination "Metadata"
  return ((unpack . unpack $ head (methodReturnBody reply)) :: Map String Variant)

-- | Get author
getAuthor :: Metadata -> String
getAuthor m = case M.lookup "xesam:artist" m of
  Just x  -> head (unpack x)
  Nothing -> ""

getTitle :: Metadata -> String
getTitle m = case M.lookup "xesam:title" m of
  Just x  -> unpack x
  Nothing -> ""

getUrl :: Metadata -> String
getUrl m = case M.lookup "xesam:url" m of
  Just x  -> unpack x
  Nothing -> ""

getLength :: Metadata -> Integer
getLength m = case M.lookup "mpris:length" m of
  -- UGLY!!! For some reason we can't deserialize the variant returned
  -- from spotify... so we parse the string representation instead
  Just x  -> let printed = show x
                 number = read . drop 8 $ printed
             in number :: Integer
  Nothing -> 1
