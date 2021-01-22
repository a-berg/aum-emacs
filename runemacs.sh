#!/usr/bin/env bash

# This script is for development purposes only. It starts up Emacs with no other
# configuration than the one in init.el.

emacs -Q --load init.el ../gray_scott.py &
