
skip_spc:			*スペースをスキップする
	subq.l	#1,d4
	bmi	@f
	cmpi.b	#' ',(a4)+
	beq	skip_spc
	cmpi.b	#TAB,-1(a4)
	beq	skip_spc
	subq.w	#1,a4
@@:
	addq.l	#1,d4
	rts

skip_plus:			*PLUSをスキップする
	subq.l	#1,d4
	bmi	@f
	cmpi.b	#'+',(a4)+
	beq	skip_plus
	subq.w	#1,a4
@@:
	addq.l	#1,d4
	rts

skip_sep:			*セパレータをスキップする(スペースやタブも)
	IF_FULL	<move.w d0,-(sp)>
	IF_TINY	<move.l d0,-(sp)>
skip_sep_lp:
	subq.l	#1,d4
	bmi	@f
	move.b	(a4)+,d0
	cmpi.b	#' ',d0
	beq	skip_sep_lp
	cmpi.b	#09,d0
	beq	skip_sep_lp
	cmpi.b	#',',d0
	beq	skip_sep_lp
	cmpi.b	#':',d0
	beq	skip_sep_lp
	cmpi.b	#'=',d0		*OPMD CNF FILEのケース
	beq	skip_sep_lp
	subq.w	#1,a4
@@:
	addq.l	#1,d4
	IF_FULL	<move.w (sp)+,d0>
	IF_TINY	<move.l (sp)+,d0>
	rts
