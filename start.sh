#!/bin/bash
PORT=${1:-4555}
google-chrome "http://localhost:$PORT" &
sbcl --load package.lisp
#--eval "( pretty-lisp:up :port $PORT)"
exit 0
