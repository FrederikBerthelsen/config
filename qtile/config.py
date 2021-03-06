# Copyright (c) 2010 Aldo Cortesi
# Copyright (c) 2010, 2014 dequis
# Copyright (c) 2012 Randall Ma
# Copyright (c) 2012-2014 Tycho Andersen
# Copyright (c) 2012 Craig Barnes
# Copyright (c) 2013 horsik
# Copyright (c) 2013 Tao Sauvage
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

from libqtile.config import Key, Screen, Group, Drag, Click
from libqtile.lazy import lazy
from libqtile import layout, bar, widget, hook
from libqtile.command_client import InteractiveCommandClient
import time
import os
import subprocess

import fontawesome as fa

from typing import List  # noqa: F401

# Variables

# Helper functions
# def get_shell_out(arguments):
#     out = subprocess.Popen(arguments, stdout = subprocess.PIPE)
#     stdout = out.communicate()
#     stdout_string = stdout[0].decode('utf-8')
#     return stdout_string[1:-2]

# Hooks
@hook.subscribe.startup_once
def autostart():
    home = os.path.expanduser('~/.config/qtile/autostart.sh')
    subprocess.call([home])

mod = "mod4"

keys = [
    # Switch between windows in current stack pane
    Key([mod], "k", lazy.layout.down()),
    Key([mod], "j", lazy.layout.up()),

    # Move windows up or down in current stack
    Key([mod, "control"], "k", lazy.layout.shuffle_down()),
    Key([mod, "control"], "j", lazy.layout.shuffle_up()),

    # Switch window focus to other pane(s) of stack
    Key([mod], "Tab", lazy.layout.next()),

    # Swap panes of split stack
    Key([mod, "shift"], "space", lazy.layout.rotate()),

    # Toggle between split and unsplit sides of stack.
    # Split = all windows displayed
    # Unsplit = 1 window displayed, like Max layout, but still with
    # multiple stack panes
    Key([mod], "Return", lazy.layout.toggle_split()),
    Key([mod, "shift"], "Return", lazy.spawn("terminator")),

    # Toggle between different layouts as defined below (Between MonadTall and Max)
    Key([mod], "space", lazy.next_layout()),

    # Kill Window
    Key([mod, "shift"], "c", lazy.window.kill()),

    # Restart and Shutdown
    Key([mod, "shift"], "r", lazy.restart()),
    Key([mod, "shift"], "q", lazy.shutdown()),

    # Spawn commandline in bar
    Key([mod], "p", lazy.spawncmd()),

    # Volume and media keys
    Key([], "XF86AudioRaiseVolume", lazy.spawn("amixer -D pulse sset Master 5%+")),
    Key([], "XF86AudioLowerVolume", lazy.spawn("amixer -D pulse sset Master 5%-")),
    Key([], "XF86AudioMute", lazy.spawn("amixer -D pulse set Master 1+ toggle")),
    Key([], "XF86AudioPlay", lazy.spawn("dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.PlayPause")),
    Key([], "XF86AudioNext", lazy.spawn("dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Next")),
    Key([], "XF86AudioPrev", lazy.spawn("dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Previous")),

    # Screenshot keys
    Key([], "Print", lazy.spawn("flameshot gui")),

    # Firefox Spawn
    Key([mod], "i", lazy.spawn("firefox")),

    # Hide Bar
    Key([mod], "b", lazy.hide_show_bar("top"))
]

groups = [Group(i) for i in "12345678"]

groups.append(Group('9', spawn='spotify'))

for i in groups:
    keys.extend([
        # mod1 + letter of group = switch to group
        Key([mod], i.name, lazy.group[i.name].toscreen()),

        # mod1 + shift + letter of group = switch to & move focused window to group
        # Key([mod, "shift"], i.name, lazy.window.togroup(i.name, switch_group=True)),
        # Or, use below if you prefer not to switch to that group.
        # # mod1 + shift + letter of group = move focused window to group
        Key([mod, "shift"], i.name, lazy.window.togroup(i.name)),
    ])

groups[0].label = "1  "
groups[1].label = "2 " + fa.icons['code'] + " "
groups[2].label = "3 " + fa.icons['firefox']
groups[8].label = "9 " + fa.icons['spotify']

layouts = [
    layout.MonadTall(margin=10, single_margin=0, single_border_width=0, border_focus='#32D9AD'),
    layout.Max(),
    # layout.Stack(num_stacks=2),
    # Try more layouts by unleashing below layouts.
    # layout.Bsp(),
    # layout.Columns(),
    # layout.Matrix(),
    # layout.MonadWide(),
    # layout.RatioTile(),
    # layout.Tile(),
    # layout.TreeTab(),
    # layout.VerticalTile(),
    # layout.Zoomy(),
]

widget_defaults = dict(
    font='Hack Nerd Font',
    fontsize=12,
    padding=3,
)
extension_defaults = widget_defaults.copy()

screens = [
    Screen(
        wallpaper='~/Pictures/wallpapers/wallpaper.png',
        wallpaper_mode='fill',
        top=bar.Bar(
            [
                widget.TextBox(
                        text ='  ', #  🍎
                        fontsize=20,
                        foreground=['32D9AD'],
                        ),
                widget.TextBox(
                    font="Arial",
                    foreground=['32D9AD', '282828'],
                    text="◢",
                    fontsize=50,
                    padding=-1
                ),
                widget.GroupBox(
                    font='Hack Nerd Font',
                    inactive='000000',
                    background=['32D9AD', '282828'],
                    highlight_color=['32D9AD', '282828'],
                    hide_unused=True,
                    highlight_method='line',
                    spacing=5,
                ),
                widget.TextBox(
                    font="Arial", 
                    foreground=['32D9AD', '282828'],
                    text="◤",
                    fontsize=45,
                    padding=-2
                ),
                widget.Prompt(),
                #widget.WindowName(),
                #widget.Systray(),
                widget.Spacer(),
                widget.TextBox(
                    text='',
                    foreground = ['32D9AD', '282828'],
                    padding=-6.5,
                    fontsize=47
                ),
                widget.TextBox(
                    text='ﱘ ',
                    background = ['32D9AD', '282828']
                ),
                widget.Spacer(
                    length=5,
                    background = ['32D9AD', '282828']
                ),
                widget.Mpris2(
                    fmt='{} ',
                    name='spotify',
                    scroll_chars=None,
                    display_metadata=['xesam:title', 'xesam:artist'],
                    objname='org.mpris.MediaPlayer2.spotify',
                    stop_pause_text=' ',
                    background = ['32D9AD', '282828']
                ),
                widget.TextBox(
                    text='',
                    background = ['32D9AD', '282828'],
                    foreground = ['282828', '32D9AD'],
                    padding=-6.5,
                    fontsize=47
                ),
                widget.TextBox(
                    text='  ',
                    background = ['282828', '32D9AD']
                ),
                widget.Volume(
                    device='pulse',
                    fmt = '{} ',
                    background = ['282828', '32D9AD']
                ),
                widget.TextBox(
                    text='',
                    foreground = ['32D9AD', '282828'],
                    background = ['282828', '32D9AD'],
                    padding=-6.5,
                    fontsize=47
                ),
                widget.TextBox(
                    font="Hack Nerd Font",
                    text=' ',
                    background = ['32D9AD', '282828']
                ),
                widget.Battery(
                    format = "{char} {percent:-1.0%} ",
                	update_interval = 1.0,
                    padding=4,
                	low_percentage = -1.05,
                	full_char = " Full",
                	charge_char = "",
                	discharge_char = "",
                    font="Hack Nerd Font",
                    background = ['32D9AD', '282828']
                ),
                widget.TextBox(
                    text='',
                    background = ['32D9AD', '282828'],
                    foreground = ['282828', '32D9AD'],
                    padding=-6.5,
                    fontsize=47
                    ),
                widget.Clock(
                    format='%A, %B %d ',
                    background = ['282828', '32D9AD']
                    ),
                widget.TextBox(
                        text='',
                        foreground = ['32D9AD', '282828'],
                        background = ['282828', '32D9AD'],
                        padding=-6.5,
                        fontsize=47
                        ),
                widget.Clock(
                    format='%H:%M ',
                    background = ['32D9AD', '282828']
                ),
            ],
            24,
            #background=["#000000", "#949494"],
        ),
    ),
]

# Drag floating layouts.
mouse = [
    Drag([mod], "Button1", lazy.window.set_position_floating(),
         start=lazy.window.get_position()),
    Drag([mod], "Button3", lazy.window.set_size_floating(),
         start=lazy.window.get_size()),
    Click([mod], "Button2", lazy.window.bring_to_front())
]

dgroups_key_binder = None
dgroups_app_rules = []  # type: List
main = None
follow_mouse_focus = False
bring_front_click = False
cursor_warp = False
floating_layout = layout.Floating(float_rules=[
    # Run the utility of `xprop` to see the wm class and name of an X client.
    {'wmclass': 'confirm'},
    {'wmclass': 'dialog'},
    {'wmclass': 'download'},
    {'wmclass': 'error'},
    {'wmclass': 'file_progress'},
    {'wmclass': 'notification'},
    {'wmclass': 'splash'},
    {'wmclass': 'toolbar'},
    {'wmclass': 'confirmreset'},  # gitk
    {'wmclass': 'makebranch'},  # gitk
    {'wmclass': 'maketag'},  # gitk
    {'wname': 'branchdialog'},  # gitk
    {'wname': 'pinentry'},  # GPG key password entry
    {'wmclass': 'ssh-askpass'},  # ssh-askpass
])
auto_fullscreen = True
focus_on_window_activation = "smart"

# XXX: Gasp! We're lying here. In fact, nobody really uses or cares about this
# string besides java UI toolkits; you can see several discussions on the
# mailing lists, GitHub issues, and other WM documentation that suggest setting
# this string if your java app doesn't work correctly. We may as well just lie
# and say that we're a working one by default.
#
# We choose LG3D to maximize irony: it is a 3D non-reparenting WM written in
# java that happens to be on java's whitelist.
wmname = "LG3D"
