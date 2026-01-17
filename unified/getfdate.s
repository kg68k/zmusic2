
get_filedate:
	clr.l	-(sp)
	move.w	d5,-(sp)
rwff3:
	DOS	_V2_FILEDATE
	addq.w	#6,sp
	rts
