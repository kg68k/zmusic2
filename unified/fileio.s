
so_read:
	move.w	sr,-(sp)
	andi.w	#$f8ff,sr
	move.l	d3,-(sp)	*push size
	move.l	d1,-(sp)	*push addr
	move.w	d5,-(sp)	*file handle
	DOS	_READ		*サンプリングデータの読み込み
	lea	10(sp),sp
	move.w	(sp)+,sr
	rts

so_ope:
	move.w	#$7400,case_child-work(a6)

	move.w	#%0_000_01,-(sp)	*自分自身へ出力しちゃう
	move.l	out_name(pc),-(sp)
	DOS	_OPEN
*	addq.w	#6,sp
	move.l	d0,d5		*d5.w=file handle

	move.l	d3,-(sp)	*size
	move.l	d1,-(sp)	*data address
	move.w	d5,-(sp)
	DOS	_WRITE
*	lea	10(sp),sp
	lea	16(sp),sp	*!!!

	bsr	do_fclose

	move.w	#NOP,case_child-work(a6)
	rts
