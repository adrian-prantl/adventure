#!/bin/sh

basepath=/
if [ x`hostname` = xF4Z ]; then
    basepath=/adrian/adventure/
fi
swipl -O -G64m -L64m -T64m -f advserver.pl "$basepath"

echo TERMINATED
