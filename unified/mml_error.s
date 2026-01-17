
prt_err_code:			*ｺﾝﾊﾟｲﾙﾓｰﾄﾞ時に置けるｴﾗｰｺｰﾄﾞの表示
	* < d0.b=error code
	* - all
	movem.l	d0-d1/a1,-(sp)
	moveq.l	#0,d1
	move.b	d0,d1
	cmpi.b	#28,d1
	bne	@f
	bset.b	#0,outmem_flg-work(a6)
	bne	exit_prt_er		*２度目以降はメッセージ無し
@@:
	addq.l	#1,num_of_err-work(a6)

	pea	sr_filename(pc)
	DOS	_PRINT
*	addq.w	#4,sp

	move.l	line_number(pc),d0
	bsr	num_to_str
	pea	suji-1(pc)	*print from tab
	DOS	_PRINT		*line number
*	addq.w	#4,sp

	move.w	#TAB,-(sp)
	DOS	_PUTCHAR
*	addq.w	#2,sp

	move.l	d1,d0
	add.w	d0,d0
	move.w	err_mes_tbl(pc,d0.w),d0
	lea	err_mes_tbl(pc,d0.w),a1
	bsr	print_error_mes

	pea	error_mes(pc)
	DOS	_PRINT
*	addq.w	#4,sp

	move.l	d1,d0
	bsr	num_to_str
	pea	suji(pc)
	DOS	_PRINT		*error code
*	addq.w	#4,sp

	pea	kakko_toji(pc)
	DOS	_PRINT
*	addq.w	#4,sp
	lea	22(sp),sp	*!!!
exit_prt_er:
	movem.l	(sp)+,d0-d1/a1
	rts

err_mes_tbl:
	dc.w	0			*0 (RESERVED)
	dc.w	0			*1 (RESERVED)
	dc.w	err_mes2-err_mes_tbl	*2
	dc.w	0			*3 (RESERVED)
	dc.w	0			*4 (RESERVED)
	dc.w	err_mes5-err_mes_tbl	*5
	dc.w	err_mes6-err_mes_tbl	*6
	dc.w	0			*7 (RESERVED)
	dc.w	0			*8 (RESERVED)
	dc.w	err_mes9-err_mes_tbl	*9
	dc.w	err_mes10-err_mes_tbl	*10
	dc.w	err_mes11-err_mes_tbl	*11
	dc.w	err_mes12-err_mes_tbl	*12
	dc.w	err_mes13-err_mes_tbl	*13
	dc.w	err_mes14-err_mes_tbl	*14
	dc.w	err_mes15-err_mes_tbl	*15
	dc.w	err_mes16-err_mes_tbl	*16
	dc.w	err_mes17-err_mes_tbl	*17
	dc.w	err_mes18-err_mes_tbl	*18
	dc.w	err_mes19-err_mes_tbl	*19
	dcb.w	2,err_mes20-err_mes_tbl	*20-21
	dcb.w	3,err_mes22-err_mes_tbl	*22-24
	dcb.w	2,err_mes25-err_mes_tbl	*25-26
	dc.w	err_mes27-err_mes_tbl	*27
	dc.w	err_mes28-err_mes_tbl	*28
	dc.w	err_mes29-err_mes_tbl	*29
	dc.w	err_mes30-err_mes_tbl	*30
	dc.w	err_mes31-err_mes_tbl	*31
	dc.w	err_mes32-err_mes_tbl	*32
	dc.w	err_mes33-err_mes_tbl	*33
	dc.w	err_mes27-err_mes_tbl	*34
	dcb.w	2,err_mes35-err_mes_tbl	*35-36
	dc.w	err_mes37-err_mes_tbl	*37
	dc.w	err_mes38-err_mes_tbl	*38
	dc.w	err_mes39-err_mes_tbl	*39
	dcb.w	3,err_mes40-err_mes_tbl	*40-42
	dc.w	err_mes43-err_mes_tbl	*43
	dc.w	err_mes40-err_mes_tbl	*44
	dc.w	err_mes45-err_mes_tbl	*45
	dc.w	err_mes46-err_mes_tbl	*46
	dc.w	err_mes47-err_mes_tbl	*47
	dc.w	err_mes48-err_mes_tbl	*48
	dcb.w	2,err_mes49-err_mes_tbl	*49-50
	dc.w	err_mes51-err_mes_tbl	*51
	dc.w	err_mes52-err_mes_tbl	*52
	dc.w	err_mes53-err_mes_tbl	*53
	dc.w	err_mes54-err_mes_tbl	*54
	dc.w	err_mes55-err_mes_tbl	*55
	dc.w	err_mes56-err_mes_tbl	*56
	dc.w	err_mes57-err_mes_tbl	*57
	dc.w	err_mes58-err_mes_tbl	*58
	dc.w	err_mes59-err_mes_tbl	*59
	dc.w	0			*60 (RESERVED)
	dc.w	0			*61 (RESERVED)
	dc.w	err_mes62-err_mes_tbl	*62
	dc.w	err_mes63-err_mes_tbl	*63
	dc.w	err_mes64-err_mes_tbl	*64
	dc.w	0			*65 (RESERVED)
	dc.w	0			*66 (RESERVED)
	dc.w	err_mes38-err_mes_tbl	*67
	dc.w	0			*68 (RESERVED)
	dc.w	err_mes69-err_mes_tbl	*69
	dc.w	err_mes70-err_mes_tbl	*70
	dc.w	0			*71
	dc.w	err_mes72-err_mes_tbl	*72
	dc.w	0			*73
	dc.w	err_mes74-err_mes_tbl	*74
	dc.w	err_mes75-err_mes_tbl	*75
	dc.w	err_mes76-err_mes_tbl	*76
	dc.w	err_mes77-err_mes_tbl	*77
	dc.w	err_mes78-err_mes_tbl	*78
	dc.w	err_mes79-err_mes_tbl	*79
	dc.w	err_mes80-err_mes_tbl	*80
	dc.w	err_mes81-err_mes_tbl	*81
	dc.w	err_mes82-err_mes_tbl	*82
	dc.w	err_mes83-err_mes_tbl	*83
	dc.w	err_mes84-err_mes_tbl	*84
	dc.w	err_mes85-err_mes_tbl	*85
	dc.w	err_mes86-err_mes_tbl	*86
	dc.w	err_mes87-err_mes_tbl	*87
	dc.w	err_mes88-err_mes_tbl	*88
	dc.w	err_mes89-err_mes_tbl	*89
	dc.w	err_mes90-err_mes_tbl	*90
	dc.w	err_mes91-err_mes_tbl	*91
	dc.w	err_mes92-err_mes_tbl	*92
	dc.w	err_mes93-err_mes_tbl	*93

print_error_mes:
	* < a1=message line
	* X d0,a1
	pea	(a0)
	moveq.l	#0,d0
pem_lp:
	suba.l	a0,a0
	move.b	(a1)+,d0
	beq	pem_end
	cmpi.b	#'|',d0
	bne	@f
	lea	ILLEGAL(pc),a0
@@:
	cmpi.b	#'ｴ',d0
	bne	@f
	lea	ERROR(pc),a0
@@:
	cmpi.b	#'c',d0
	bne	@f
	lea	COMMAND(pc),a0
@@:
	cmpi.b	#'#',d0
	bne	@f
	lea	NUMBER(pc),a0
@@:
	cmpi.b	#'%',d0
	bne	@f
	lea	PARAMETER(pc),a0
@@:
	cmpi.b	#'=',d0
	bne	@f
	lea	EFFECTS(pc),a0
@@:
	move.l	a0,-(sp)	*わざとmove.l
	bne	@f
	move.w	d0,-(sp)
	DOS	_PUTCHAR
	addq.w	#6,sp
	bra	pem_lp
@@:
	DOS	_PRINT
	addq.w	#4,sp
	bra	pem_lp
pem_end:
	move.l	(sp)+,a0
	rts
