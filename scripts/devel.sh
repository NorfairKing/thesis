#!/bin/bash

set -e
set -x

sel=$*

cmd="thesis build"
if [[ "$sel" != "" ]]
then
  cmd="$cmd --selection $sel"
fi

stack install :thesis --file-watch --exec="$cmd" --fast
