#!/bin/sh
xinput set-prop "    Touchpad" "libinput Natural Scrolling Enabled" 1
xinput set-prop "    Touchpad" "libinput Tapping Enabled" 1
compton &
#uncomment for blurred background, performance issues?
#compton --blur-background &