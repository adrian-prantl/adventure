#!/bin/sh

basepath=/
if [ x`hostname` = xf4z ]; then
    basepath=/adrian/adventure/
fi
swipl -O -G64m -L64m -T64m -f advserver.pl "$basepath"

echo TERMINATED
