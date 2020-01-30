import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Layout.Spacing
import XMonad.Util.EZConfig

main :: IO ()
main = do 
    xmonad =<< xmobar myConfig

xmobarShowHideKeymap (XConfig {modMask = modKey}) = (modKey, xK_b)

myStartupHook :: X ()
myStartupHook = 
    spawn
        "compton --backend glx --xrender-sync --xrender-sync-fence -fcCz -l -17 -t -17 -D 5"

myConfig = def
    { modMask            = mod4Mask
    , borderWidth        = 2
    , layoutHook = smartSpacingWithEdge 6 $ Tall 1 (3/100) (1/2)
    , normalBorderColor  = "#bbbbbb"
    , focusedBorderColor = "#58E894" 
    , focusFollowsMouse  = False 
    , startupHook        = myStartupHook
    }
    `additionalKeysP` myKeys

myKeys :: [(String, X ())]
myKeys =
    [ ("<XF86AudioRaiseVolume>", spawn "amixer -D pulse sset Master 5%+")
    , ("<XF86AudioLowerVolume>", spawn "amixer -D pulse sset Master 5%-")
    , ("<XF86AudioMute>", spawn "amixer -D pulse set Master 1+ toggle")
    ]