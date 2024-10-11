echo off
zmsc -r > nul
as /w4 /m3000 zmsc_.has /s type=%1 /s mpu=%2 /o zmsc.o /s debug=%3 > er
lk zmsc > nul
