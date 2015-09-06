#!/bin/bash

file=$1
line=$2
col=$3
/usr/bin/emacsclient -c -e \
    "(progn

       ;; Load the file
       (find-file \"$file\")

       ;; Jump to the same point as in IntelliJ
       ;; Unfortunately, IntelliJ doesn't always supply the values
       ;; depending on where the open is invoked from; e.g. keyboard
       ;; works, tab context doesn't
       (when (not (string= \"\" \"$line\"))
         (goto-char (point-min))
         (forward-line (1- $2))
         (forward-char (1- $3)))

       ;; Raise/focus our window; depends on the windowing system
       (if (string-equal system-type \"darwin\")
         (ns-do-applescript \"tell application \\\"Emacs\\\" to activate\")
         (raise-frame)))"
