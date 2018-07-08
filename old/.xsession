#!/bin/sh

#
# ~/.xsession
#
# Executed by xdm/gdm/kdm at login
#

if which zsh &>>/dev/null && zsh -lc : ; then
  exec /bin/zsh -il ~/.xinitrc
else
  exec /bin/bash --login -i ~/.xinitrc
fi

