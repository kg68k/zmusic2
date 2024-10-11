echo off
del musicz.fnc > nul
as /w musicz.has /s dbg=%1 > er
lk musicz -o MUSICZ.FNC > nul
copy musicz.fnc a:\basic2\musicz.fnc > nul
