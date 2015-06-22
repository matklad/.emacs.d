#!/bin/bash

function find_window {
    windows=$(wmctrl -lx | awk -v name=".Emacs" '$3 ~ name' | grep -v "Hangouts")
}


find_window
if [ $? != 0 ];
then
    emacsclient -c -a ""
else
    set -- $windows
    if [ $1 ]; then
        wmctrl -ia $1
    fi
fi
