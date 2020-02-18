import XMonad
import XMonad.ManageHook
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Layout.Spacing
import XMonad.Util.EZConfig
import XMonad.Actions.SpawnOn

main :: IO ()
main = do 
    xmonad =<< xmobar myConfig

xmobarShowHideKeymap (XConfig {modMask = modKey}) = (modKey, xK_b)

myWorkspaces = [ "1","2","3","4","5","6","7","8","9:spotify" ]

myManageHook = composeAll
   [ className =? "" --> doShift "9:spotify" ]

myStartupHook :: X ()
myStartupHook = do
    spawn "compton --backend glx --xrender-sync --xrender-sync-fence -fcCz -l -17 -t -17 -D 5"
    spawnOn "9:spotify" "spotify"

myConfig = def
    { modMask            = mod4Mask
    , borderWidth        = 2
    , layoutHook = smartSpacingWithEdge 6 $ Tall 1 (3/100) (1/2)
    , normalBorderColor  = "#bbbbbb"
    , focusedBorderColor = "#58E894" 
    , focusFollowsMouse  = False 
    , startupHook        = myStartupHook
    , manageHook         = manageDocks <+> myManageHook
    , workspaces         = myWorkspaces
    }
    `additionalKeysP` myKeysP
    `additionalKeys` myKeys

myKeysP :: [(String, X ())]
myKeysP =
    [ ("<XF86AudioRaiseVolume>", spawn "amixer -D pulse sset Master 5%+")
    , ("<XF86AudioLowerVolume>", spawn "amixer -D pulse sset Master 5%-")
    , ("<XF86AudioMute>", spawn "amixer -D pulse set Master 1+ toggle")
    , ("<XF86AudioPlay>", spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.PlayPause")
    , ("<XF86AudioStop>", spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Stop")
    , ("<XF86AudioNext>", spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Next")
    , ("<XF86AudioPrev>", spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Previous")
    ]

myKeys =
    [ ((0, xK_Print), spawn "scrot ~/Pictures/Screenshots/%Y-%m-%d-%H%M%S.png")
    , ((mod4Mask, xK_Print), spawn "~/.xmonad/selectScreenshot.sh")
    , ((mod4Mask .|. shiftMask, xK_Print), spawn "~/.xmonad/selectScreenshotFirefox.sh")