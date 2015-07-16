import Data.Monoid (All(..))
import System.Exit (exitSuccess)
import System.IO (hPutStrLn)
import System.IO.Strict as IOS (readFile)
import System.Process (readProcess)
import XMonad
import XMonad.Actions.CycleWS (toggleWS)
import XMonad.Actions.CycleWindows (cycleRecentWindows)
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, ppOutput)
import XMonad.Hooks.EwmhDesktops (ewmh, ewmhDesktopsStartup, fullscreenEventHook)
import XMonad.Hooks.ManageDocks (docksEventHook, ToggleStruts (..))
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Hooks.UrgencyHook (withUrgencyHook, NoUrgencyHook (..))
import XMonad.Prompt.RunOrRaise (runOrRaisePrompt)
import XMonad.Prompt.Window (windowPromptGoto)
import XMonad.Util.EZConfig (additionalKeys, additionalKeysP)
import XMonad.Util.ExtensibleState as XS
import XMonad.Util.Run (spawnPipe, runInTerm, safeSpawn)
import qualified XMonad.StackSet as W

import qualified Constants as C
import MPD
import Utils
import StackSetExtra as WX
import Workspaces
import PulseAudio
import Interactive

-- TODO: we might want to set urgency for the "to be focused" window in the future.
-- TODO: test that the window sending the event is in fact the firefox
-- window.  So far, this hasn't caused any trouble
-- | This fixes the annoying issue where opening a link from an
-- outside application focuses firefox.
withoutNetActiveWindow :: XConfig a -> XConfig a
withoutNetActiveWindow c = c { handleEventHook = \e -> do
                                  p <- case e of
                                        ClientMessageEvent { ev_message_type = mt } -> do
                                          a_aw <- getAtom "_NET_ACTIVE_WINDOW"
                                          return (mt /= a_aw)
                                        otherwise -> return True
                                  if p then handleEventHook c e else return (All True) }

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

setBrightness :: Double -> IO ()
setBrightness b = do
  maxb <- maxBrightness
  minb <- minBrightness
  let range = maxb - minb
      new' = (b * range) + minb
      new = (new' `min` maxb) `max` minb
  safeSpawn "sudo" ["/home/matus/bin/set-brightness", show $ floor $ new]

main = do
       w <- IOS.readFile "/home/matus/.whereami"
       let (left, middle, right) = case w of
                   "brno" -> (2,0,1)
                   "home" -> (2,1,0)
                   "logio" -> (2,0,1)
                   _ -> (2,0,1)
       let (S xmobarScreen) = case w of
                         "brno" -> right
                         "home" -> left
                         "logio" -> middle
                         _ -> middle
       xmproc <- spawnPipe $ "/home/matus/.cabal/bin/xmobar -x " ++ show xmobarScreen ++ " /home/matus/.xmonad/xmobarrc"
       xmonad $
         (\c -> c { startupHook = do
                       XS.put (Workspaces.ScreenOrder [left, middle, right])
                       startupHook c
                       setWMName "LG3D" }) $
         withoutNetActiveWindow $
         ewmh $
         withUrgencyHook NoUrgencyHook defaultConfig
                {
                  manageHook         = C.manageHook
                , layoutHook         = C.layout
                , logHook            = dynamicLogWithPP C.printer { ppOutput = hPutStrLn xmproc }
                , handleEventHook    = fullscreenEventHook <+> docksEventHook
                , modMask            = mod4Mask
                , borderWidth        = 1
                , terminal           = "urxvtc"
                , normalBorderColor  = "#000000"
                , focusedBorderColor = "#008800"
                , workspaces         = C.workspaces
                } `additionalKeysP`
                [ ("<XF86AudioPlay>", MPD.toggle)
                , ("<XF86AudioStop>", MPD.stop)
                , ("<XF86AudioPrev>", MPD.previous)
                , ("<XF86AudioNext>", MPD.next)
                , (leader <%> "t",        MPD.toggle)
                , (leader <%> "s",        MPD.stop)
                , (leader <%> "p",        MPD.previous)
                , (leader <%> "n",        MPD.next)
                , (leader <%> "<Print>",  MPD.toggle)
                , (leader <%> "-",        MPD.stop)
                , (leader <%> "<Delete>", MPD.next)
                , (leader <%> "d",        MPD.deleteCurrent)
                , (leader <%> "c",        MPD.clear)
                , (leader <%> "<F9>",     MPD.playPlaylist Clear)
                , (leader <%> "<F10>",    MPD.playArtist Clear)
                , (leader <%> "<F11>",    MPD.playDirectory Clear)
                , (leader <%> "u" <%> "<F9>",  MPD.playPlaylist Add)
                , (leader <%> "u" <%> "<F10>", MPD.playArtist Add)
                , (leader <%> "u" <%> "<F11>", MPD.playDirectory Add)
                , (leader <%> "<F12>",   MPD.jumpToTrack)
                , ("<XF86AudioRaiseVolume>", spawn "amixer -q sset Master 3%+")
                , ("<XF86AudioLowerVolume>", spawn "amixer -q sset Master 3%-")
                , ("<XF86AudioMute>",        spawn "amixer -q -D pulse sset Master toggle")
                , ("<XF86MonBrightnessDown>", liftIO $ actuallBrightnessFrac >>= \x -> setBrightness (x - 0.1))
                , ("<XF86MonBrightnessUp>", liftIO $ actuallBrightnessFrac >>= \x -> setBrightness (x + 0.1))
                , (leader <%> "m", muteSinkInput)
                , (leader <%> "v", setVolumeSinkInput)
                , (leader <%> "<Insert>",    spawn "amixer -q -D pulse sset Master toggle")
                , (leader <%> "<F7>",        spawn "/home/matus/bin/toggle-touchpad")
                , ("M4-S-<Return>", runInTerm "" "fish")
                , ("<XF86Sleep>", spawn "sudo pm-suspend")
                , ("<Print>" <%> "<Print>", spawn "/home/matus/bin/take-screenshot")
                , ("<Print>" <%> "u" <%> "<Print>", spawn "/home/matus/bin/take-screenshot noupload")
                , (leader <%> "<F1>" <%> "<F1>", spawn "xfce4-settings-manager")
                , (leader <%> "<F1>" <%> "<F2>", spawn "xfce4-appfinder")
                  -- create a submap for these
                , (leader <%> "=" <%> "a", runInTermOrRaise "alsamixer" "0")
                , (leader <%> "=" <%> "p", runInTermOrRaise "pacmixer" "0")
                , (leader <%> "=" <%> "n", runInTermOrRaise "ncmpcpp" "0")
                , (leader <%> "=" <%> "c", runInTermOrRaise "pavucontrol" "0")
                , ("M2-<Backspace>", toggleWS)
                , ("M2-S-<Pause>", io exitSuccess)
                , ("M2-<Pause>", recompileXMonad)
                , ("M2-p", runOrRaisePrompt C.prompt)
                , (leader <%> leader, windowPromptGoto C.prompt)
                , ("M2-c", kill)
                , ("M2-m", withScreen left W.view)
                , ("M2-,", withScreen middle W.view)
                , ("M2-.", withScreen right W.view)
                , ("M2-C-m", withScreen left WX.shiftAndView)
                , ("M2-C-,", withScreen middle WX.shiftAndView)
                , ("M2-C-.", withScreen right WX.shiftAndView)
                , ("M2-M4-m", withScreen left WX.shiftAndGreedyView)
                , ("M2-M4-,", withScreen middle WX.shiftAndGreedyView)
                , ("M2-M4-.", withScreen right WX.shiftAndGreedyView)
                , ("M2-S-m", withScreen left W.shift)
                , ("M2-S-,", withScreen middle W.shift)
                , ("M2-S-.", withScreen right W.shift)
                , ("M2-/", windows WX.shiftToOtherScreen)
                , ("M4-p", windows W.focusDown)
                , ("M4-n", windows W.focusUp)
                , ("M4-P", windows W.swapDown)
                , ("M4-N", windows W.swapUp)
                , ("M2-[", windows W.focusDown)
                , ("M2-]", windows W.focusUp)
                , ("M2-=", cycleRecentWindows [xK_Alt_R] xK_equal xK_minus)
                , ("M4-b", sendMessage ToggleStruts)
                , ("M4-S-b", broadcastMessage ToggleStruts >> refresh)
                , (leader <%> "=" <%> "o", inotify)
                , (leader <%> "=" <%> "i", inotify2)
                , (leader <%> "=" <%> "u", urxvtc)
                , (leader <%> "=" <%> "m", iMWindow)
                -- TODO: pridat "hide all" a "show all"
                ] `additionalKeys`
                (
                  [ (mod2Mask,                   W.greedyView)
                  , (mod4Mask,                   W.greedyView)
                  , ((shiftMask .|. mod2Mask),   W.shift)
                  , ((shiftMask .|. mod4Mask),   W.shift)
                  , ((controlMask .|. mod2Mask), WX.shiftAndGreedyView)
                  , ((mod4Mask .|. mod2Mask),    WX.shiftAndView)
                  ]
                  >>= (uncurry C.withWorkspacesD)
                )
         where
           leader = "<Pause>"

-- brno letisko LKTB
-- sliac letisko LZSL
-- http://www.airports-worldwide.info/index.php?page=airport
