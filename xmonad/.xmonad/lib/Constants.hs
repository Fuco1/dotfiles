{-# LANGUAGE NoMonomorphismRestriction #-}
module Constants
       ( module Constants
       , Workspaces.ScreenOrder -- tento reexport je prasarna
       ) where

import XMonad
import XMonad.Actions.CopyWindow (copyToAll)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks (avoidStruts, manageDocks)
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.Drawer
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.ResizeScreen
import XMonad.Prompt
import XMonad.Util.Loggers

import qualified XMonad.StackSet as W

import Utils
import Workspaces

prompt :: XPConfig
prompt = defaultXPConfig { font = "xft:Monospace-12:narrow"
                         , bgColor = "black"
                         , fgColor = "#888a85"
                         , fgHLight = "#eeeeec"
                         , bgHLight = "#3465a4"
                         , borderColor = "#008800"
                         , promptKeymap = emacsLikeXPKeymap
                         , height = 25
                         , searchPredicate = subseq
                         , alwaysHighlight = True
                         }

workspaces :: [WorkspaceId]
workspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "0",
              "q", "w", "e", "r"]

workspaceKeys = [xK_1 .. xK_9] ++ [xK_0, xK_q, xK_w, xK_e, xK_r]

-- | Just like 'withWorkspaces' but supply default workspace and key lists.
withWorkspacesD :: ButtonMask -> (WorkspaceId -> WindowSet -> WindowSet) -> [((ButtonMask, KeySym), X ())]
withWorkspacesD = withWorkspaces Constants.workspaces workspaceKeys

layout = mixerdrawer `onBottom` as (Full ||| tiled ||| Mirror tiled)
  where
    mixerdrawer = drawer 0 0.5
                  (foldl1 Or . map Resource $ ["alsamixer", "pacmixer"])
                  (Tall 0 0 0)
    tiled = Tall 1 (3/100) (1/2)
    as = avoidStruts . smartBorders

printer = defaultPP { ppCurrent         = xmobarColor "#fcaf3e" ""
                    , ppVisible         = xmobarColor "#d3d7cf" ""
                    , ppHidden          = id
                    , ppHiddenNoWindows = const ""
                    , ppUrgent          = xmobarColor "#cc0000" ""
                    , ppSep             = "│"
                    , ppWsSep           = ""
                    , ppTitle           = xmobarColor "#8cc4ff" ""
                    , ppLayout          = (:[]) . head
                    , ppOrder           = id
                    , ppExtras          = []
                    , ppSort            = getSortByMyCompare
                    }

doKillWindow :: ManageHook
doKillWindow = ask >>= \w -> liftX (killWindow w) >> doF (W.delete w)

manageHook = (composeOne . concat $ --  <&&> resource =? "TeamViewer.exe"
    [ [ (stringProperty "WM_NAME" =? "Computers & Contacts" <&&> resource =? "TeamViewer.exe") -?> doKillWindow
      , isDialog -?> doFloat
      , transience
      , isFullscreen -?> doFullFloat
      , resource =? "TeamViewer.exe" -?> doCenterFloat
      , resource =? "arandr" -?> doCenterFloat
      ]
    , [ className =? c -?> doFloat | c <- myCFloats ]
    , [ title     =? t -?> doFloat | t <- myTFloats ]
    , [ resource  =? r -?> doFloat | r <- myRFloats ]
    , [ className =? c -?> doIgnore | c <- myCIgnores ]
    , [ title     =? t -?> doIgnore | t <- myTIgnores ]
    , [ resource  =? r -?> doIgnore | r <- myRIgnores ]
    , [ resource  =? r -?> doShift "0" | r <- ["alsamixer", "pacmixer", "ncmpcpp"]]
    , [ className =? "Xfce4-notifyd" -?> doIgnore ]
    ])
    <+> manageDocks
    <+> XMonad.manageHook defaultConfig
    where
        myCFloats = ["sun-awt-X11-XFramePeer", "net-sourceforge-jnlp-runtime-Boot"]
        myTFloats = ["GLFW-b-demo"]
        myRFloats = []
        myCIgnores = []
        myTIgnores = []
        myRIgnores = []
