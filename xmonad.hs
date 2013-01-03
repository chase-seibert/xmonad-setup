{-
  This is my xmonad configuration file.
  There are many like it, but this one is mine.

  If you want to customize this file, the easiest workflow goes
  something like this:
    1. Make a small change.
    2. Hit "super-q", which recompiles and restarts xmonad
    3. If there is an error, undo your change and hit "super-q" again to
       get to a stable place again.
    4. Repeat

  Author:     David Brewer
  Repository: https://github.com/davidbrewer/xmonad-ubuntu-conf
-}

import Data.Ratio ((%))

import XMonad
import XMonad.Actions.GridSelect
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.Grid
import XMonad.Layout.ResizableTile
import XMonad.Layout.IM
import XMonad.Layout.ThreeColumns
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Fullscreen
import XMonad.Util.EZConfig
import XMonad.Util.Paste
import XMonad.Util.Run

import qualified XMonad.StackSet as W

{-
  Xmonad configuration variables. These settings control some of the
  simpler parts of xmonad's behavior and are straightforward to tweak.
-}

myModMask            = mod1Mask       -- changes the mod key to "super"
myFocusedBorderColor = "#ff0000"      -- color of focused border
myNormalBorderColor  = "#cccccc"      -- color of inactive border
myBorderWidth        = 1              -- width of border around windows
myTerminal           = "gnome-terminal"   -- which terminal software to use
myIMRosterTitle      = "Contact List" -- title of roster on IM workspace
myLayouts            = defaultLayouts


{-
  Xmobar configuration variables. These settings control the appearance
  of text which xmonad is sending to xmobar via the DynamicLog hook.
-}

myTitleColor     = "#eeeeee"  -- color of window title
myTitleLength    = 80         -- truncate window title to this length
myCurrentWSColor = "#e6744c"  -- color of active workspace
myVisibleWSColor = "#c185a7"  -- color of inactive workspace
myUrgentWSColor  = "#cc0000"  -- color of workspace with 'urgent' window
myCurrentWSLeft  = "["        -- wrap active workspace with these
myCurrentWSRight = "]"
myVisibleWSLeft  = "("        -- wrap inactive workspace with these
myVisibleWSRight = ")"
myUrgentWSLeft  = "{"         -- wrap urgent workspace with these
myUrgentWSRight = "}"

myWorkspaces = ["1:Web",  "2:Dev", "3:Prod", "4",  "5", "6", "7",  "8", "9", "0"]
startupWorkspace = "2:Dev"  -- which workspace do you want to be on after launch?

-- Define group of default layouts used on most screens, in the
-- order they will appear.
-- "smartBorders" modifier makes it so the borders on windows only
-- appear if there is more than one visible window.
-- "avoidStruts" modifier makes it so that the layout provides
-- space for the status bar at the top of the screen.
defaultLayouts = smartBorders(avoidStruts(
  ResizableTall 1 (3/100) (1/2) []
  ||| Mirror (ResizableTall 1 (3/100) (1/2) [])
  ||| noBorders Full
  ||| Grid
  ||| ThreeColMid 1 (3/100) (3/4)))

myKeyBindings =
  [
    ("M-b", sendMessage ToggleStruts)
    , ("M-l", spawn "slock")
    , ("M-u", focusUrgent)
    , ("M-<F3>", goToSelected defaultGSConfig)
    , ("M-<F8>", spawn "volti-remote -m")
    , ("M-<F9>", spawn "volti-remote -d")
    , ("M-<F10>", spawn "volti-remote -i")
    , ("M-<F11>", spawn "scrot")
    , ("C-<Space>", spawn "exe=`dmenu_path | dmenu -fn '-misc-fixed-bold-r-normal-*-18-*-*-*-*-*-*-*'` && exec $exe")
  ]
myKeys = myKeyBindings

myManagementHooks :: [ManageHook]
myManagementHooks = [
  resource =? "stalonetray" --> doIgnore
  , (className =? "chromium-browser") --> doF (W.shift "1:Web")
  ]

main = do
  xmproc <- spawnPipe "xmobar ~/.xmonad/xmobarrc"
  xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig {
    focusedBorderColor = myFocusedBorderColor
  , normalBorderColor = myNormalBorderColor
  , terminal = myTerminal
  , borderWidth = myBorderWidth
  , layoutHook = myLayouts
  , workspaces = myWorkspaces
  , modMask = myModMask
  , handleEventHook = fullscreenEventHook
  , startupHook = do
      windows $ W.greedyView startupWorkspace
      spawn "~/.xmonad/startup-hook"
  , manageHook = manageHook defaultConfig
      <+> composeAll myManagementHooks
      <+> manageDocks
  , logHook = dynamicLogWithPP $ xmobarPP {
      ppOutput = hPutStrLn xmproc
      , ppTitle = xmobarColor myTitleColor "" . shorten myTitleLength
      , ppCurrent = xmobarColor myCurrentWSColor ""
        . wrap myCurrentWSLeft myCurrentWSRight
      , ppVisible = xmobarColor myVisibleWSColor ""
        . wrap myVisibleWSLeft myVisibleWSRight
      , ppUrgent = xmobarColor myUrgentWSColor ""
        . wrap myUrgentWSLeft myUrgentWSRight
    }
  }
    `additionalKeysP` myKeys
