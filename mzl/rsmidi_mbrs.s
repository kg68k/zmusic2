* Mouse Bit Rate Set Program ver0.52 by ふなぽん氏
* から抜粋、サブルーチン化。

**********************************************************
**			時定数調査			**
**********************************************************
IN_ClOCK_SORT:
	movem.l	d1-d4,-(sp)
	moveq.l	#0,d3
	move.w	#$4c07,d1
	IOCS	_SET232C

	IOCS	_TIMEGET
	move.l	d0,d2
2:
	IOCS	_TIMEGET
	cmp.l	d2,d0
	beq	2B
3:
	IOCS	_TIMEGET
	move.l	d0,d2
5:
	IOCS	_TIMEGET
	cmp.l	d2,d0
	bne	6F
7:
	IOCS	_OSNS232C
	tst.l	d0
	beq	5B				*送信不可能

	move.b	#'@',d1
	IOCS	_OUT232C
	addq.l	#1,d3
8:
	bra	5B
6:
						*((d3-1)/30)-2
	subq.l	#1,d3
	move.l	d3,d0
	movem.l	(sp)+,d1-d4
	rts

