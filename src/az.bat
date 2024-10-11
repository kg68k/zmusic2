echo off
rem 	AZ 1  ??	デバッグルーチンインクルード
rem 	AZ 3  ??	ＲＳ－ＭＩＤＩ対応版生成
rem 	AZ 4  ??	ＰＯＬＹＰＨＯＮ対応版生成
rem 	AZ ?? 24	改造Ｘ６８ｋ対応版生成
rem 	AZ ?? 30	Ｘ６８０３０対応版生成
zmusic -r > nul
as -w4 -stype=%1 -smpu=%2 -ozmusic.o -sdebug=%3 zmsc.has > er
lk zmusic
