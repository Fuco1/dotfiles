{-# LANGUAGE DeriveDataTypeable, FlexibleContexts #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Actions.Prefix
-- Copyright   :  (c) Matus Goljer <matus.goljer@gmail.com>
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Matus Goljer <matus.goljer@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A module that allows the user to use a prefix argument (raw or numeric).
--
-----------------------------------------------------------------------------

module Prefix
       (
      -- * Usage
      -- $usage

      -- * Installation
      -- $installation

         PrefixArgument(..)
       , usePrefixArgument
       , useDefaultPrefixArgument
       , withPrefixArgument
       ) where

import Data.Bits
import qualified Data.Map as M
import Data.Maybe
import Control.Monad.Fix (fix)
import Control.Monad (liftM2)

import XMonad
import XMonad.Util.ExtensibleState as XS
import XMonad.Util.Paste (sendKey)

{- $usage

This module implements Emacs-style prefix argument.  The argument
comes in two flavours, "Raw" and "Numeric".

To initiate the "prefix mode" you hit the prefix keybinding (default
C-u).  This sets the Raw argument value to 1.  Repeatedly hitting this
key increments the raw value by 1.  The Raw argument is usually used
as a toggle, changing the behaviour of the function called in some way.

An example might be calling "mpc add" to add new song to the playlist,
but with C-u we also clean up the playlist beforehand.

When in the "Raw mode", you can hit numeric keys 0..9 (with no
modifier) to enter a "Numeric argument".  Numeric argument represents
a natural number.  Hitting numeric keys in sequence produces the
decimal number that would result from typing them.  That is, the
sequence C-u 4 2 sets the Numeric argument value to the number 42.

If you have a function which understands the prefix argument, for example:

>    addMaybeClean :: PrefixArgument -> X ()
>    addMaybeClean (Raw _) = spawn "mpc clear" >> spawn "mpc add <file>"
>    addMaybeClean _ = spawn "mpc add <file>"

you can turn it into an X action with the function 'withPrefixArgument'.

Binding it in your config

>    ((modm, xK_a), withPrefixArgument addMaybeClean)

Hitting MOD-a will add the <file> to the playlist while C-u MOD-a will
clear the playlist and then add the file.

You can of course use an anonymous action, like so:

>    ((modm, xK_a), withPrefixArgument $ \prefix -> do
>        case prefix of ...
>    )

If the prefix key is followed by a binding which is unknown to XMonad,
the prefix along with that binding is sent to the active window.

-}

{- $installation

The simplest way to enable this is to use 'useDefaultPrefixArgument'

>    xmonad $ useDefaultPrefixArgument $ defaultConfig { .. }

The default prefix argument is C-u.  If you want to customize the
prefix argument, use the following:

>    xmonad $ usePrefixArgument prefixKey $ defaultConfig { .. }

where 'prefixKey' is a function which takes 'XConfig' as an argument
in case you wish to extract the 'modMask'.  An example
implementation is the following:

>    prefixKey :: XConfig t -> (KeyMask, KeySym)
>    prefixKey XConfig{modMask = modm} = (modm, xK_u)

-}

data PrefixArgument = Raw Int | Numeric Int | None
                      deriving (Typeable, Read, Show)
instance ExtensionClass PrefixArgument where
  initialValue = None
  extensionType = PersistentExtension

-- TODO: contribute to Submap
-- | Like 'submap', but executes a default action if the key did not match.
submapDefaultWithKeys :: ((KeyMask, KeySym) -> X ())
                      -> M.Map (KeyMask, KeySym) (X ())
                      -> X ()
submapDefaultWithKeys defAction keys = do
    XConf { theRoot = root, display = d } <- ask

    io $ grabKeyboard d root False grabModeAsync grabModeAsync currentTime

    (m, s) <- io $ allocaXEvent $ \p -> fix $ \nextkey -> do
        maskEvent d keyPressMask p
        KeyEvent { ev_keycode = code, ev_state = m } <- getEvent p
        keysym <- keycodeToKeysym d code 0
        if isModifierKey keysym
            then nextkey
            else return (m, keysym)
    -- Remove num lock mask and Xkb group state bits
    m' <- cleanMask $ m .&. ((1 `shiftL` 12) - 1)

    io $ ungrabKeyboard d currentTime

    fromMaybe (defAction (m', s)) (M.lookup (m', s) keys)

-- TODO: contribute to xmonad
-- | Run 'job' in the 'X' monad and then execute 'cleanup'.  In case
-- of exception, 'cleanup' is executed anyway.
--
-- Return the return value of 'job'.
--finallyX :: X a -> X a -> X a
--finallyX job cleanup = catchX (job >>= \r -> cleanup >> return r) cleanup

usePrefixArgument :: LayoutClass l Window
                  => (XConfig Layout -> (KeyMask, KeySym))
                  -> XConfig l
                  -> XConfig l
usePrefixArgument prefix conf = conf {
    keys = liftM2 M.union keys' (keys conf)
  }
  where keys' conf' =
          let binding = prefix conf'
          in M.singleton binding (handlePrefixArg [binding])

useDefaultPrefixArgument :: LayoutClass l Window
                         => XConfig l
                         -> XConfig l
useDefaultPrefixArgument = usePrefixArgument (\_ -> (controlMask, xK_u))

handlePrefixArg :: [(KeyMask, KeySym)] -> X ()
handlePrefixArg events = do
  ks <- asks keyActions
  logger <- asks (logHook . config)
  flip finallyX (XS.put None >> logger) $ do
    prefix <- XS.get
    case prefix of
      Raw a -> XS.put $ Raw (a + 1)
      None -> XS.put $ Raw 1
      _ -> return ()
    logger
    submapDefaultWithKeys defaultKey ks
  where defaultKey key@(m, k) =
          if k `elem` (xK_0 : [xK_1 .. xK_9]) && m == noModMask
          then do
            prefix <- XS.get
            let x = fromJust (Prelude.lookup k keyToNum)
            case prefix of
              Raw _ -> XS.put $ Numeric x
              Numeric a -> XS.put $ Numeric $ a * 10 + x
              None -> return () -- should never happen
            handlePrefixArg (key:events)
          else do
            prefix <- XS.get
            mapM_ (uncurry sendKey) $ case prefix of
              Raw a -> replicate a (head events) ++ [key]
              _ -> reverse (key:events)
        keyToNum = (xK_0, 0) : zip [xK_1 .. xK_9] [1..9]

withPrefixArgument :: (PrefixArgument -> X ()) -> X ()
withPrefixArgument = (>>=) XS.get
