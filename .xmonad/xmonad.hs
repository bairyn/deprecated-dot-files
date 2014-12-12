{-# LANGUAGE DeriveDataTypeable, GADTs, TemplateHaskell #-}

-- | XMonad configuration.
--
-- https://github.com/bairyn/dot-files

-- TODO: this is rather messy; could use some cleaning up.
module Main where

import Control.Applicative
import Control.Monad
import Data.Data
import Data.List
import qualified Data.Map as M
import Data.Monoid
import qualified Data.Set as S
import System.Exit
import System.IO
import Text.Printf

import XMonad
import XMonad.Hooks.DynamicLog
--import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.Circle
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.WindowNavigation
import XMonad.StackSet
import XMonad.Util.Loggers
import XMonad.Util.Run

--- MAIN CONFIGURATION ---

main :: IO ()
main = xmonad =<< (dzenBar . withUrgencyHookC dzenUrgencyHook urgency $ xmonadConfig)
    where xmonadConfig = defaultConfig
              { normalBorderColor  = colorUnfocused
              , focusedBorderColor = colorFocused
              --, terminal           = "urxvtc"  -- terminal needs to support -title option or modify the m-s-return key binding to behave properly; I suggest installing, configuring, and using urxvt
              --, terminal           = "sakura"
              -- , terminal           = "terminal"
              -- , terminal           = "gnome-terminal"
              --, terminal           = "urxvt"
              --, terminal           = "gnome-terminal"
              , terminal           = case termConf of 
                                       GnomeTerminal -> "gnome-terminal"
                                       XFCE4Terminal -> "xfce4-terminal"
                                       Rxvtc         -> "urxvtc"
                                       Rxvt          -> "urxvt"
              , layoutHook         = layouts
              , manageHook         = customManageHook
              , handleEventHook    = event
              , XMonad.workspaces  =
                  ---- '`'-9 (qwerty)
                  --[ "Background"
                  -- 1-9 (qwerty)
                  [ "Programming 1"
                  , "Status"
                  , "Empty"
                  , "IRC"
                  , "Media"  -- music, web browsing, etc.
                  , "Programming 2"
                  , "Graphics 1"  -- primarily gimp
                  , "Private 1"
                  , "Administration"
                  , "co'e pe'a"
                  ]
              --, numlockMask        = mod2Mask
              , modMask            = mod4Mask
              , keys               = keyBindings
              , mouseBindings      = mouse
              , borderWidth        = 1
              , logHook            = customLogHook
              , startupHook        = startup
              , focusFollowsMouse  = True
              }
          dzenBar xmonadConfig = do
              spawn =<< customStatusBar
              return xmonadConfig
          {-
          dzenBar xmonadConfig = do
              --h <- spawnPipe $ "dzen2 -fn '-*-terminal-*-*-*-*-12-*-*-*-*-*-*' -e 'onstart=lower' -w 400 -h 14 -ta l -fg 'white' -bg '" ++ colorFocused ++ "'"
              h <- spawnPipe $ "dzen2 -e 'onstart=lower' -h 12 -ta l -fg 'white' -bg '" ++ colorFocused ++ "'"
              return $ xmonadConfig
                  { logHook = do
                        logHook xmonadConfig
                        dynamicLogWithPP pp{ppOutput = hPutStrLn h}
                  }
          pp = dzenPP
              { ppCurrent = const $ ""
              , ppVisible = const $ ""
              , ppHidden  = const $ ""
              , ppHiddenNoWindows  = const $ ""
              , ppUrgent  = dzenColor "red" "yellow" . dzenStrip
              , ppWsSep   = ""
              , ppSep     = " "
              , ppTitle   = const $ ""
              {-
              , ppLayout  = dzenColor "black" "#cccccc" .
                            (\x -> case x of
                                       "TilePrime Horizontal" -> " TTT "
                                       "TilePrime Vertical"   -> " []= "
                                       "Hinted Full"          -> " [ ] "
                                       _                      -> pad x
                            )
              -}
              , ppLayout  = const $ ""
              , ppExtras  =
                    [ aumixVolume
                    , battery
                    , date "%Y-%m-%d:%H:%M:%S"
                    , loadAvg
                    , logCmd "cat /proc/acpi/thermal_zone/THRM/temperature"
                    ]
              }
          -}

data TermConf =
    GnomeTerminal  -- ^ Gnome-terminal.
                   --
                   --   Complaints:
                   --     * Uses single process for all terminals with no
                   --       option to disable.
  | XFCE4Terminal  -- ^ xfce4-terminal.  (On Archlinux, from xfce4-terminal package; command is now "xfce4-terminal" =) )
                   --
                   --   Complaints:
                   --     * Character under cursor, when the color is the same,
                   --       is invisible!
                   --     * Uses single process for all terminals with no
                   --       option to disable.
  | Rxvt           -- ^ urxvtc, rxvt-unicode, etc.
  | Rxvtc          -- ^ use daemon and client, urxvtc.
  deriving (Eq, Ord, Show, Read, Enum, Typeable, Data)

-- | Your xmonad configuration uses this setting to determine which terminal to
-- use.
termConf :: TermConf
termConf = GnomeTerminal

-- | Configure your terminal emulator by the main key binding to automatically
-- start in a new tmux session.
--
-- Warning: if enabled, may prevent shell from starting.  Can disable from the
-- TTY or by using an alternate key-binding to start a terminal emulator that
-- ignores this option, e.g. meta-enter for an IRC terminal.
--
-- Requires "require-tmux" in PATH.  My version, which I've put in both
-- ~/bin/require-tmux and a shell function named require-tmux, looks something like this:
--
-- @
-- #!/bin/bash
-- # Ensure you're in a tmux session.  If TMUX isn't set and isn't
-- # blank, and if TERM isn't/doesn't contain "screen", run "exec tmux $*" (by
-- # default, with no args, "tmux" is "tmux new").
-- if [[ ! $TERM =~ screen ]]; then
-- # TODO: lookup bash AND conditional.
-- if [[ "$TMUX" = "" ]]; then
--     exec tmux $*
-- fi
-- fi
-- @
--
-- and
--
-- @
-- # require-tmux ensures you're in a tmux session.  If TMUX isn't set and isn't
-- # blank, and if TERM isn't/doesn't contain "screen", run "exec tmux $*" (by
-- # default, with no args, "tmux" is "tmux new").
-- function require-tmux()
-- {
--   source ~/bin/require-tmux $*
-- }
-- @
--
-- Solution from http://stackoverflow.com/questions/11068965/how-can-i-make-tmux-be-active-whenever-i-start-a-new-shell-session .
autoRequireTmux :: Bool
autoRequireTmux = True

-- | The command to execute to automatically require tmux when a terminal is
-- started, when configured by 'autoRequireTmux'.
--
-- Currently set to pass -2 to force 256 colors (otherwise vim colors can look
-- different), and -u to tell tmux utf-8 is supported by the terminal.
--
-- Unless the terminal binding that uses 'autoRequireTmuxCommand' with printf is updated, this must be a 'String'.
autoRequireTmuxCommand :: String
autoRequireTmuxCommand = "require-tmux -2u"

-- | The third way to start a Terminal, intended to be used as a fallback, e.g.
-- when 'autoRequireTmux' breaks and prevents new terminals from spawning,
-- allowing one to fix these issues or at least disable 'autoRequireTmux',
-- whether for a tmux protocol mismatch, package removal, or other reason.
fallbackTerminalCommand :: String
fallbackTerminalCommand = "xterm /bin/bash"

compositingManagerStart :: X ()
compositingManagerStart = do
    spawn $ "xcompmgr -c"  -- This will fail if the process is already running, ultimately not doing anything

compositingManagerStop :: X ()
compositingManagerStop = do
    spawn $ "pkill xcompmgr"

colorFocused, colorUnfocused, colorUrgent :: String
colorFocused   = "#121262"
colorUnfocused = "#12122F"
colorUrgent    = "#4212CC"

--- STATUS BAR ---
customStatusBar :: IO String
customStatusBar = do
    return "dzen.sh"
{-
    let input =
            "zsh -c '\
\export INTERVAL=1s\n\
\\n\
\while true; do\n\
\    export DATE=`date \"+%Y-%m-%d_%H:%M:%S\"`\n\
\\n\
\    sleep $INTERVAL\n\
\done\n\
\\n'"
        dzenCmd =
            " | dzen2 -e 'onstart=lower,collapse;onstart=hide;sigusr1=togglehide;sigusr2=unhide' -ta l -fg '#A1A1EF' -bg '#11112F'"
    return $ input ++ dzenCmd
-}

-- Note: This doesn't hide the status bar; it only hides the gaps, because communication with dzen is limited.
hideStatusBar :: X ()
hideStatusBar = do
    broadcastMessage $ SetStruts [] [minBound .. maxBound]
    refresh

showStatusBar :: X ()
showStatusBar = do
    broadcastMessage $ SetStruts [minBound .. maxBound] []
    spawn $ "pkill -USR2 dzen2"
    refresh

toggleStatusBar :: X ()
toggleStatusBar = do
    spawn            $ "pkill -USR1 dzen2"
    broadcastMessage $ ToggleStruts
    refresh

-- | Used by 'startup' to start xflux
zipCode :: String
--zipCode = "83263"
zipCode = "94704"

--- STARTUP ---
-- Some of .xinitrc can be moved here, but be aware that this action is executed each time xmonad is started or restarted.  Don't use 'spawn' if you care about how it terminates or want to block until it terminates.
startup :: X ()
startup = do
    -- Hide or show status bar
    hideStatusBar

    -- Set keyboard rate
    -- 200ms after first keypress, and 64 keys per second
    spawn $ "xset r rate 150 64"

    -- Disable mouse acceleration
    spawn $ "xset m 0 0"

    -- Use programmer dvorak key layout, and map ctrl to left control, since caps lock is so useless
    ----spawn $ "setxkbmap -layout us -variant dvp -option compose:menu -option keypad:atm -option numpad:shift3 -option kpdl:semi -option ctrl:nocaps"
    ----spawn $ "setxkbmap -layout us -variant dvp -option compose:menu -option ctrl:nocaps"
    ----spawn $ "setxkbmap -layout us -variant dvp -option compose:menu -option ctrl:nocaps -option lv3:ralt_switch"

    -- Dvorak
    ----spawn $ "setxkbmap -layout us -variant dvp -option compose:menu -option ctrl:nocaps -option lv3:ralt_switch"
    ----spawn $ "setxkbmap -layout us -variant dvorak -option compose:menu -option ctrl:nocaps -option lv3:ralt_switch"
    --spawn $ "setxkbmap -layout us -variant dvorak -option compose:menu -option ctrl:nocaps -option lv3:ralt_switch -option keypad:pointerkeys"

    -- Now add toggle between us and russian.
    --spawn $ "setxkbmap -layout 'dvorak,ru,ara' -option 'grp:shifts_toggle' -option compose:menu -option ctrl:nocaps -option lv3:ralt_switch -option keypad:pointerkeys"
    --spawn $ "setxkbmap -layout 'dvorak,ru' -option 'grp:shifts_toggle' -option compose:menu -option ctrl:nocaps -option lv3:ralt_switch -option keypad:pointerkeys"
    -- Run "setxkbmap -option" to reset previous options.
    spawn $ "setxkbmap -option -layout 'dvorak,ru' -option 'grp:shifts_toggle' -option compose:menu -option ctrl:nocaps -option lv3:ralt_switch -option keypad:pointerkeys"

    -- Qwerty
    --spawn $ "setxkbmap -layout us -variant qwerty -option compose:menu -option ctrl:nocaps -option lv3:ralt_switch"

    -- Start screensaver
    spawn $ "xscreensaver -no-splash"  -- xscreensaver won't start again if a process is already running

    -- Set resolution and gamma (restarting just xmonad can take care of issues)
    --spawn $ "xrandr -s 0"
    --spawn $ "xrandr --output VGA-0 --mode 1024x768"
    spawn $ "xrandr --auto"
    spawn $ "xrandr --output LVDS1 --auto"
    spawn $ "xrandr -o normal"
    spawn $ "xgamma -gamma 1.0"
    -- 32 dpcm
    --spawn $ "xrandr --dpi 81.28"
    --spawn $ "xrandr --dpi 96"
    spawn $ "xrandr --dpi 81.28"
    spawn $ "xrandr --auto"
    spawn $ "xrandr -s 0"

    -- Start pulseaudio
    spawn $ "start-pulseaudio-x11"

    -- Start urxvtd daemon if using Rxvt terminal with daemon / client setup.
    case termConf of
      Rxvt -> spawn $ "urxvtd -q -f -o"
      _    -> return ()

    -- Start a terminal
    --spawn =<< asks (terminal . config)

    -- Kill xflux and start it
    spawn $ "pkill xflux; xflux -z " ++ zipCode

--- SHUTDOWN ---
-- Note: this is only called when the user uses the m-2-q key bindings
shutdown :: X ()
shutdown = do
    spawn $ "pkill dzen2"
    return ()

--- URGENCY ---
urgency :: UrgencyConfig
urgency = urgencyConfig
    { suppressWhen = Focused--Never
    , remindWhen   = Every 120  -- seconds
    }

--- WINDOW MANAGING ---
customManageHook :: ManageHook
customManageHook = composeAll $
    [ manageDocks

    -- Windows to automatically float
    --, className =? "MPlayer"          --> doFloat
    , className =? "Gimp"             --> doFloat
    , className =? "Thunar"           --> doFloat
    , className =? "VLC media player" --> doFloat
    , className =? "Xcfe4-panel"      --> doFloat
    , className =? "Xfce-mcs-manager" --> doFloat
    , className =? "Xcfe-mixer"       --> doFloat
    , className =? "Gui.py"           --> doFloat

    -- Windows to ignore
    , resource  =? "gnome_panel"      --> doIgnore
    , resource  =? "desktop_window"   --> doIgnore

    -- Windows to move to a workspace
    , title     =? "Navigator"        --> doShift "Media"
    , title     =? "Namoroka"         --> doShift "Media"
    , title     =? "Vimperator"       --> doShift "Media"
    , title     =? "Iceweasel"        --> doShift "Media"
    , title     =? "IRC"              --> doShift "IRC"
    , className =? "Gimp"             --> doShift "Graphics 1"
    ]

--- LOG HOOK ---
customLogHook :: X ()
customLogHook = do--ewmhDesktopsLogHook
    --dynamicLogDzen
    dynamicLogWithPP dzenPP

--- EVENT HOOK ---
event :: Event -> X All
event = mempty

--- MOUSE ---
mouse :: XConfig t -> M.Map (KeyMask, Button) (Window -> X ())
mouse (XConfig {XMonad.modMask = modm}) = M.fromList $
    -- mod-button1; set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> XMonad.focus w >> mouseMoveWindow w
                                       >> windows shiftMaster))
 
    -- mod-button2; raise the window to the top of the stack
    , ((modm, button2), (\w -> XMonad.focus w >> windows shiftMaster))
 
    -- mod-button3; set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> XMonad.focus w >> mouseResizeWindow w
                                       >> windows shiftMaster))
 
    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

--- LAYOUTS ---

-- Feel free to add your own.  Check out XMonad.Layout.* at http://hackage.haskell.org/package/xmonad-contrib-0.9.1 (the newest version, of course) and see http://hackage.haskell.org/packages/archive/xmonad-contrib/latest/doc/html/XMonad-Doc-Extending.html#Editing_the_layout_hook.
layouts = windowNavigation . avoidStruts $ smartBorders (noBorders (tabbed shrinkText customTabbedConfig) ||| Full ||| tall ||| Mirror tall ||| Circle)
    where tall = ResizableTall 1 (3.0 / 100.0) (1.0 / 2.0) []
          customTabbedConfig = defaultTheme
              { inactiveBorderColor = colorUnfocused
              , activeBorderColor   = "white"
              , urgentBorderColor   = "white"
              , inactiveTextColor   = "grey"
              , activeTextColor     = "white"
              , urgentTextColor     = "white"
              , inactiveColor       = colorUnfocused
              , activeColor         = colorFocused
              , urgentColor         = colorUrgent
              , decoHeight          = 8
              , fontName            = "-*-terminus-*-*-*-*-8-*-*-*-*-*-iso10646-1"
              }

--- KEYS ---

screenshotProgram :: String
screenshotProgram =
    "ruby -e \"require 'gtk2';tmp_filepath = '/tmp/tmp-scrot.png';system \\\"scrot #{tmp_filepath}\\\";clipboard=Gtk::Clipboard.get Gdk::Display.default, Gdk::Selection::CLIPBOARD;clipboard.image = Gdk::Pixbuf.new(tmp_filepath);clipboard.store\""

keyBindings :: XConfig Layout -> M.Map (ButtonMask, KeySym) (X ())
keyBindings xmonadConfig@(XConfig { modMask = modm
                            }) = M.fromList . (\ ~(x:y:xs) -> y:x:xs) $
-- Notice that some bindings don't use the layout conversion functions.  This happens when the key is a mnemonic for its function (instead of its positioning being important) and its position will never be inconvenient or conflict with another position.
    -- XMonad --
    -- quit
    [ ((modm, xK_q), shutdown >> broadcastMessage ReleaseResources >> restart "xmonad" True)
    , ((modm .|. shiftMask, xK_q), shutdown >> (io $ exitWith ExitSuccess))


    -- Window Navigation --

    -- arrow keys
    , ((modm, xK_Right), sendMessage $ Go R)
    , ((modm, xK_Left ), sendMessage $ Go L)
    , ((modm, xK_Up   ), sendMessage $ Go U)
    , ((modm, xK_Down ), sendMessage $ Go D)

    , ((modm .|. shiftMask, xK_Right), sendMessage $ Swap R)
    , ((modm .|. shiftMask, xK_Left ), sendMessage $ Swap L)
    , ((modm .|. shiftMask, xK_Up   ), sendMessage $ Swap U)
    , ((modm .|. shiftMask, xK_Down ), sendMessage $ Swap D)                                      

    , ((modm .|. controlMask .|. shiftMask, xK_Right), sendMessage $ Move R)
    , ((modm .|. controlMask .|. shiftMask, xK_Left),  sendMessage $ Move L)
    , ((modm .|. controlMask .|. shiftMask, xK_Up),    sendMessage $ Move U)
    , ((modm .|. controlMask .|. shiftMask, xK_Down),  sendMessage $ Move D)

    -- home row
    , ((modm, toKey $ xK_k), windows $ focusUp)
    , ((modm, toKey $ xK_j), windows $ focusDown)

    , ((modm .|. shiftMask, toKey $ xK_k), windows $ swapUp)
    , ((modm .|. shiftMask, toKey $ xK_j), windows $ swapDown)

    -- tab
    , ((modm, xK_Tab),               windows focusDown)
    , ((modm .|. shiftMask, xK_Tab), windows focusUp)


    -- Layouts, master area, and arrangement of windows --

    -- shrink and expand focused window; shift does so by a greater magnitude; keys conveniently placed near backspace (on most keyboards)
    , ((modm, toKey $ xK_minus), sendMessage MirrorShrink)
    , ((modm, toKey $ xK_equal), sendMessage MirrorExpand)
    , ((modm .|. shiftMask, toKey $ xK_minus), replicateM_ 4 $ sendMessage MirrorShrink)
    , ((modm .|. shiftMask, toKey $ xK_equal), replicateM_ 4 $ sendMessage MirrorExpand)

    -- shrink and expand master area
    , ((modm, toKey $ xK_h), sendMessage Shrink)
    , ((modm, toKey $ xK_l), sendMessage Expand)

    -- focus the master area by focusing a window in the master area
    , ((modm, xK_m), windows focusMaster)

    -- tile the focused window; this is particularity useful for floating windows
    , ((modm, toKey $ xK_t), withFocused $ windows . sink)  -- Conveniently placed immediately after master size keys

    -- decrement or increment master area size (which is the number of windows the master area will try to contain)
    -- '<' or '>'
    , ((modm , xK_comma),  sendMessage (IncMasterN (-1)))
    , ((modm , xK_period), sendMessage (IncMasterN ( 1)))

    -- layout updates
    , ((modm, xK_space), sendMessage NextLayout)                              -- Next layout
    , ((modm .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook xmonadConfig)  -- Set layout to default one

    -- kill a window
    , ((modm, xK_k), kill)
    , ((modm .|. shiftMask, xK_k), spawn "xkill")


    -- Urgency --
    , ((modm, xK_BackSpace), focusUrgent)
    , ((modm .|. shiftMask, xK_BackSpace), clearUrgents)

    -- Background --
    , ((modm, toKey $ xK_z), spawn "bg-blank" >> compositingManagerStop)
    , ((modm .|. shiftMask, toKey $ xK_z), spawn "bg-show-alt" >> compositingManagerStart)
    , ((modm .|. controlMask, toKey $ xK_z), spawn "bg-rot")
    , ((modm .|. shiftMask .|. controlMask, toKey $ xK_z), spawn "bg-rot-back")

    -- Screenshots --
    , ((modm, toKey $ xK_semicolon), spawn "scrot '%Y-%m-%d--%H:%M%S_$wx$h.png' -e 'mv $f ~/screenshots/'")
    , ((modm .|. shiftMask, toKey $ xK_semicolon), spawn "scrot -s '%Y-%m-%d--%H:%M%S_$wx$h.png' -e 'mv $f ~/screenshots/'")
    , ((modm .|. controlMask, toKey $ xK_semicolon), spawn screenshotProgram)


    -- Misc --
    --, ((modm, xK_o), broadcastMessage (SetStruts [minBound .. maxBound] []) >> spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"")
    --, ((modm, xK_o), showStatusBar >> spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"")  -- Mnemonic: open program; esoteric note: in case the struts setting and the hide setting are in conflict, this can be used since it sets the struts setting instead of toggling it
    , ((modm, xK_o), showStatusBar >> spawn "dmenu_run")  -- Mnemonic: open program; esoteric note: in case the struts setting and the hide setting are in conflict, this can be used since it sets the struts setting instead of toggling it
    --, ((modm, xK_b), sendMessage ToggleStruts)  -- toggle status bar
    --, ((modm, xK_b), spawn $ "pkill -USR1 dzen2")  -- toggle status bar
    , ((modm, xK_b), toggleStatusBar)  -- toggle status bar
    , ((modm, xK_r), refresh)  -- Mnemonic: refresh
    --, ((modm .|. shiftMask, xK_Return), spawn $ terminal xmonadConfig ++ if "urxvt" `isInfixOf` (terminal xmonadConfig) then " -title IRC" else " --title IRC")  -- These terminals are moved to the IRC workspace
    --, ((modm .|. shiftMask, xK_Return), spawn $ terminal xmonadConfig ++ if "rxvt" `isInfixOf` (terminal xmonadConfig) then " -title IRC" else " --title IRC")  -- These terminals are moved to the IRC workspace
    , ((modm .|. shiftMask, xK_Return), spawn $ terminal xmonadConfig)  -- Start a fallback terminal, ignoring extra commands and configuration such as automatically starting tmux.
    , ((modm .|. shiftMask .|. controlMask, xK_Return), spawn $ fallbackTerminalCommand)  -- Start the last fallback terminal command.
    --, ((modm, xK_a), spawn $ terminal xmonadConfig)
    , ((modm, xK_a), if autoRequireTmux then spawn $ printf "%s -e \"%s\"" (terminal xmonadConfig) (autoRequireTmuxCommand) else spawn $ terminal xmonadConfig)
    , ((modm, toKey $ xK_p), spawn "xset dpms force off; xscreensaver-command -lock")
    -- increase and decrease opacity
    , ((modm, toKey $ xK_d), spawn "transset-df -a --dec .1")
    , ((modm, toKey $ xK_f), spawn "transset-df -a --inc .1")


    -- Program bindings.  This key configuration is focused on using dmenu to start programs.  dmenu is often more convenient than running "program &disown %1&&exit" in a terminal.  If you wish to still use a key binding to start a program, you can add your own bindings as shown below.
    -- example:
    -- , ((modm, xK_f), spawn "firefox")
    -- Or, if the position of the key is more important than its mnemonic or character, toKey can be used with the character on a qwerty keyboard, and the position won't change when your key layout is changed.  For example:
    -- , ((modm, toKey $ xK_a), spawn "firefox")
    ]

    ++

    -- Workspaces --

    [((m .|. modm, k), windows $ f i) | (k, i) <- zip workspaceKeys $ XMonad.workspaces xmonadConfig, (f, m) <- [(greedyView, 0), (shift, shiftMask)]]

    ++

    -- Screens --

    [((m .|. modm, k), screenWorkspace s >>= flip whenJust (windows . f)) | (k, s) <- zip screenKeys $ [0..], (f, m) <- [(view, 0), (shift, shiftMask)]]

    where workspaceKeys, screenKeys :: [KeySym]
          --workspaceKeys = toKey <$> [xK_grave] ++ [xK_1 .. xK_9]
          workspaceKeys = toKey <$> [xK_1 .. xK_9] ++ [xK_period]
          screenKeys    = toKey <$> [xK_y, xK_u, xK_i]

--- KEY LAYOUTS ---

keyLayout :: KeyLayout
keyLayout = dvorak

toQwerty :: KeySym -> KeySym
toQwerty = toQwertyLayout keyLayout

toKey :: KeySym -> KeySym
toKey = toKeyLayout keyLayout

toQwertyLayout :: KeyLayout -> KeySym -> KeySym
toQwertyLayout kl k = let s = S.filter ((== k) . klk_toKey) $ kl_layout kl
                      in  case () of
                            _ | S.null s  -> k
                              | otherwise -> klk_qwerty . head . S.elems $ s
toKeyLayout    :: KeyLayout -> KeySym -> KeySym
toKeyLayout    kl k = let s = S.filter ((== k) . klk_qwerty) $ kl_layout kl
                      in  case () of
                            _ | S.null s  -> k
                              | otherwise -> klk_toKey . head . S.elems $ s

-- unionKeyLayout original append; returns append with the layout of original appended to the layout of append
unionKeyLayout :: KeyLayout -> KeyLayout -> KeyLayout
unionKeyLayout original append = append{kl_layout = kl_layout original `S.union` kl_layout append}

newtype KeyLayout = KeyLayout { kl_layout :: S.Set KeyLayoutTrans
                              } deriving (Eq, Ord, Show, Read)

data KeyLayoutTrans = KeyLayoutTrans { klk_qwerty :: KeySym
                                     , klk_toKey  :: KeySym
                                     } deriving (Eq, Ord, Show, Read)

qwerty :: KeyLayout
qwerty = KeyLayout . S.fromList $ []

dvorak :: KeyLayout
dvorak = KeyLayout . S.fromList . map (\(q, t) -> KeyLayoutTrans {klk_qwerty = q, klk_toKey = t}) $
    [ (xK_a, xK_a)
    , (xK_b, xK_x)
    , (xK_c, xK_j)
    , (xK_d, xK_e)
    , (xK_e, xK_period)
    , (xK_f, xK_u)
    , (xK_g, xK_i)
    , (xK_h, xK_d)
    , (xK_i, xK_c)
    , (xK_j, xK_h)
    , (xK_k, xK_t)
    , (xK_l, xK_n)
    , (xK_m, xK_m)
    , (xK_n, xK_b)
    , (xK_o, xK_r)
    , (xK_p, xK_l)
    , (xK_q, xK_apostrophe)
    , (xK_r, xK_p)
    , (xK_s, xK_o)
    , (xK_t, xK_y)
    , (xK_u, xK_g)
    , (xK_v, xK_k)
    , (xK_w, xK_comma)
    , (xK_x, xK_q)
    , (xK_y, xK_f)
    , (xK_z, xK_semicolon)
    , (xK_bracketleft, xK_slash)
    , (xK_bracketright, xK_equal)
    , (xK_backslash, xK_backslash)
    , (xK_slash, xK_z)
    , (xK_equal, xK_bracketright)
    , (xK_minus, xK_bracketleft)
    , (xK_apostrophe, xK_minus)
    , (xK_comma, xK_w)
    , (xK_period, xK_v)
    , (xK_semicolon, xK_s)
    ]

programmerDvorak :: KeyLayout
programmerDvorak = (dvorak `unionKeyLayout`) . KeyLayout . S.fromList . map (\(q, t) -> KeyLayoutTrans {klk_qwerty = q, klk_toKey = t}) $
    [ (xK_q,            xK_semicolon)
    , (xK_z,            xK_apostrophe)
    , (xK_grave,        xK_dollar)
    , (xK_1,            xK_ampersand)
    , (xK_2,            xK_bracketleft)
    , (xK_3,            xK_braceleft)
    , (xK_4,            xK_braceright)
    , (xK_5,            xK_parenleft)
    , (xK_6,            xK_equal)
    , (xK_7,            xK_asterisk)
    , (xK_8,            xK_parenright)
    , (xK_9,            xK_plus)
    , (xK_0,            xK_bracketright)
    , (xK_minus,        xK_exclam)
    , (xK_equal,        xK_numbersign)
    , (xK_bracketleft,  xK_bracketleft)
    , (xK_bracketright, xK_at)
    , (xK_backslash,    xK_backslash)
    , (xK_asciitilde,   xK_asciitilde)
    , (xK_exclam,       xK_percent)
    , (xK_at,           xK_7)
    , (xK_numbersign,   xK_5)
    , (xK_dollar,       xK_3)
    , (xK_percent,      xK_1)
    , (xK_asciicircum,  xK_9)
    , (xK_ampersand,    xK_0)
    , (xK_asterisk,     xK_2)
    , (xK_parenleft,    xK_4)
    , (xK_parenright,   xK_6)
    , (xK_underscore,   xK_8)
    , (xK_plus,         xK_grave)
    , (xK_braceleft,    xK_question)
    , (xK_braceright,   xK_asciicircum)
    , (xK_bar,          xK_bar)
    ]
