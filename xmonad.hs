import XMonad
import Data.Monoid ()
import System.Exit ()
import XMonad.Util.SpawnOnce ( spawnOnce )
import Graphics.X11.ExtraTypes.XF86 (xF86XK_AudioLowerVolume, xF86XK_AudioRaiseVolume, xF86XK_AudioMute, xF86XK_MonBrightnessDown, xF86XK_MonBrightnessUp, xF86XK_AudioPlay, xF86XK_AudioPrev, xF86XK_AudioNext)
-- hooks
import XMonad.Hooks.EwmhDesktops ( ewmh )
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers ( doFullFloat, isFullscreen )
import XMonad.Hooks.DynamicLog
-- layout
import XMonad.Layout.NoBorders
import XMonad.Layout.Fullscreen
    ( fullscreenEventHook, fullscreenManageHook, fullscreenSupport, fullscreenFull )
import XMonad.Layout.Spacing ( spacingRaw, Border(Border) )
import XMonad.Layout.Gaps
    ( Direction2D(D, L, R, U),
      gaps,
      setGaps,
      GapMessage(DecGap, ToggleGaps, IncGap) )
-- actions
import XMonad.Actions.CycleWS
import XMonad.Actions.GroupNavigation (historyHook, nextMatch, Direction(..), isOnAnyVisibleWS)
-- prompts
import XMonad.Prompt
import XMonad.Prompt.Window
-- scratchpads
import XMonad.Util.NamedScratchpad
-- easy config
import XMonad.Util.EZConfig (additionalKeysP)
-- misc
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import Data.Maybe (maybeToList)
import Control.Monad ( join, when )
-- run promgram
import XMonad.Util.Run (spawnPipe)

-- key programs
myTerminal      = "kitty"
myBrowser       = "firefox"
-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True
-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = False
-- Width of the window border in pixels.
myBorderWidth   = 0
-- Use meta key
myModMask       = mod4Mask
altKey = mod2Mask
myWorkspaces    = ["1:term", "2:code", "3:web", "4:notion", "5:misc"]
myNormalBorderColor  = "#3b4252"
myFocusedBorderColor = "#bc96da"
-- Scratchpads, always open at the touch of a keybinding
-- scratchpads :: [NamedScratchpad]
-- scratchpads = [
--     NS "neofetch" "alacritty -name neofetch -e 'neofetch -w'" (resource =? "neofetch")
--         (customFloating $ W.RationalRect (2/6) (2/6) (2/6) (2/6)),
--     NS "notion" "notion-app" (className =? "Notion")
--         (customFloating $ W.RationalRect 1 1 1 1),
--     NS "pavucontrol" "pavucontrol" (className =? "Pavucontrol")
--         (customFloating $ W.RationalRect (1/4) (1/4) (2/4) (2/4))
--     ]
------------------------------------------------------------------------
-- Key bindings
myKeys =
    [ ("M-f", sendMessage $ ToggleGaps) -- fullscreen
    , ("M-p", spawn "~/bin/launcher.sh") -- application launcher
    , ("M-q", kill) -- close focused window
    , ("M-<Return>", spawn $ myTerminal ++ " -e zsh") -- open terminal
    , ("M-w", spawn myBrowser) -- open browser
    , ("M-S-p", spawn "~/bin/maimcopy") -- screenshot -> copy
    , ("M-<Print>", spawn "~/bin/maimsave") -- screenshot -> save
    , ("M-<Space>", sendMessage NextLayout) -- switch tiling layout
    , ("M1-<Tab>", nextMatch Forward isOnAnyVisibleWS) -- switch to app on visible WS
    , ("M1-S-<Tab>", nextMatch Backward isOnAnyVisibleWS) -- switch to previous app on visible WS
    , ("M-<Tab>", moveTo Next NonEmptyWS)
    , ("M-S-<Tab>", moveTo Prev NonEmptyWS)
    , ("M-S-<Left>", shiftToPrev >> prevWS) -- push app to left WS
    , ("M-S-<Right>", shiftToNext >> nextWS) -- push app to right WS
    , ("M-e", spawn "~/bin/emoji.sh") -- emoji picker
    , ("M-c", spawn "~/bin/clipboard.sh") -- clipboard
    , ("M-S-q", spawn "~/bin/powermenu.sh") -- quit xmonad
    , ("<XF86AudioPlay>", spawn "playerctl play-pause")
    , ("<XF86AudioPrev>", spawn "playerctl --prev")
    , ("<XF86AudioNext>", spawn "playerctl --next")
    , ("<XF86MonBrightnessUp>", spawn "brightnessctl s +10%")
    , ("<XF86MonBrightnessDown>", spawn "brightnessctl s 10-%")
    , ("<XF86AudioMute>",   spawn "pactl set-sink-mute 0 toggle")
    , ("<XF86AudioLowerVolume>", spawn "pactl set-sink-volume 0 -5%")
    , ("<XF86AudioRaiseVolume>", spawn "pactl set-sink-volume 0 +5%")
    ]
------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))
    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))
    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))
    ]
------------------------------------------------------------------------
-- Layouts:
myLayout = avoidStruts(tiled ||| Mirror tiled ||| Full)
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio
     -- The default number of windows in the master pane
     nmaster = 1
     -- Default proportion of screen occupied by master pane
     ratio   = 1/2
     -- Percent of screen to increment by when resizing panes
     delta   = 3/100
------------------------------------------------------------------------
-- Window rules:
-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook = fullscreenManageHook <+> manageDocks <+> composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "Gimp"           --> doFloat
    , className =? "Alacritty"      --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore
    , isFullscreen --> doFullFloat
    ]
------------------------------------------------------------------------
-- Gaps
myLayoutHook = gaps [(L,20), (R,20), (U,20), (D,20)] $ spacingRaw True (Border 10 10 10 10) True (Border 10 10 10 10) True $ smartBorders $ myLayout
------------------------------------------------------------------------
-- Event handling
myEventHook = mempty
------------------------------------------------------------------------
-- Status bars and logging
myLogHook = return ()
------------------------------------------------------------------------
-- Startup hook
myStartupHook = do
  spawn "xsetroot -cursor_name left_ptr"
  spawnOnce "feh --bg-scale --randomize --recursive ~/wallpapers/*"
  spawnOnce "picom -f"
  spawnOnce "greenclip daemon"
  spawnOnce "dunst"
------------------------------------------------------------------------

myBar = "xmobar /home/conor/.config/xmonad/xmobarrc"
myPP = xmobarPP { ppCurrent = xmobarColor "#429942" "" . wrap "<" ">" }
-- toggle xmobar spacing with Meta + b
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

main = xmonad =<< statusBar myBar myPP toggleStrutsKey defaults

defaults = def {
    terminal           = myTerminal,
    focusFollowsMouse  = myFocusFollowsMouse,
    clickJustFocuses   = myClickJustFocuses,
    borderWidth        = myBorderWidth,
    modMask            = myModMask,
    workspaces         = myWorkspaces,
    normalBorderColor  = myNormalBorderColor,
    focusedBorderColor = myFocusedBorderColor,
    mouseBindings      = myMouseBindings,
    manageHook         = myManageHook, 
    layoutHook         = myLayoutHook,
    handleEventHook    = myEventHook,
    logHook            = myLogHook,
    startupHook        = myStartupHook -- >> addEWMHFullscreen
} `additionalKeysP` myKeys
