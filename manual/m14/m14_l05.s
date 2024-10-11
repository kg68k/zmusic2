Z_MUSIC	macro	func		*ドライバへのファンクションコール
	moveq.l	func,d1
	trap	#3
	endm

play_music:
	* < d0.l=曲番号(0～16383)
	movem.l	d0-d2/a0-a1,-(sp)
	add.l	d0,d0
	add.l	d0,d0
	movea.l	music_data_tbl(pc,d0.l),a1
	addq.w	#7,a1
	moveq.l	#0,d2				*高速応答モード
	Z_MUSIC	#$11
	movem.l	(sp)+,d0-d2/a0-a1
	rts

music_data_tbl:
	dc.l	music0
	dc.l	music1
	dc.l	music2
	dc.l	music3
	dc.l	music4
	dc.l	music5
	dc.l	music6
	dc.l	music7
		:
		:
		:
		:
