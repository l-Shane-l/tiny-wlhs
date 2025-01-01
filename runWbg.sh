#!/bin/bash

# Kill any existing instances
pkill wbg

# Start wbg in the background and properly detach it
nohup wbg ~/.wallpapers/haskell.png >/dev/null 2>&1 &
disown -h %1
