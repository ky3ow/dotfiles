#!/usr/bin/env bash
delta=$(synclient | grep VertScrollDelta | cut -d= -f 2 | tr -d ' ')

synclient VertScrollDelta=-$delta
synclient HorizScrollDelta=-$delta

setxkbmap -option 'caps:swapescape'
