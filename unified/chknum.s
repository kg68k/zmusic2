
chk_num:			*数字かどうかチェック
	* > eq=number
	* > mi=not num
	move.l	d0,-(sp)
	bsr	skip_spc
	tst.l	d4
	beq	not_num
	move.b	(a4),d0
	cmpi.b	#'%',d0
	beq	yes_num
	cmpi.b	#'$',d0
	beq	yes_num
	cmpi.b	#'-',d0
	beq	yes_num
	cmpi.b	#'+',d0
	beq	yes_num
	cmpi.b	#'0',d0
	bcs	not_num
	cmpi.b	#'9',d0
	bhi	not_num
yes_num:
	moveq.l	#0,d0		*eq
	bra	@f
not_num:
	moveq.l	#-1,d0		*mi
@@:
	movem.l	(sp)+,d0	*わざとmovem
	rts
