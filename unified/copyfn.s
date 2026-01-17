
copy_fn:			*ファイルネームをバッファへ転送
	* < a0.l=destination address
	* < a4=address
	* < d4=counter
	* > d1.b=minus 拡張子がある
	* > d1.b=plus 拡張子はなかった
	* - all except d1,d4,a4
	movem.l	d0/d2/a0,-(sp)
	moveq.l	#0,d1
	moveq.l	#0,d2
copy_fnlp01:
	subq.l	#1,d4
	bmi	exit_cfn
	move.b	(a4)+,d0
	cmpi.b	#'.',d0
	bne	@f
	cmpi.b	#' ',(a4)
	bls	@f
	st.b	d1
@@:
	cmpi.b	#',',d0		*separater
	beq	exit_copyfn
	cmpi.b	#' ',d0		*ctrl code
	bls	exit_copyfn
	tst.b	d2
	beq	@f
	tst.b	-1(a0)
	bmi	cfst_lt
@@:
cf_patch:			*-C時にパッチが当たる
	IF_FULL <bsr.s mk_capital>
	IF_TINY	<bsr mk_capital>
cfst_lt:
	move.b	d0,(a0)+
	st.b	d2
	bra	copy_fnlp01
exit_copyfn:
	subq.w	#1,a4
exit_cfn:
	addq.l	#1,d4
	cmpi.b	#'.',-1(a0)
	bne	@f
	subq.w	#1,a0		*最後の'.'を潰す
@@:
	clr.b	(a0)
	movem.l	(sp)+,d0/d2/a0
	rts
