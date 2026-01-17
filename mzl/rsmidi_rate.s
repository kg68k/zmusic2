set_rsmidi_rate:
	movem.l	d0/d2-d7/a0-a1,-(sp)
	bsr	IN_ClOCK_SORT
	move.l	d0,d3
	asl.l	#2,d0
	add.l	d3,d0
	move.l	d0,d3
	moveq	#10,d2
	asl.l	d2,d3
	move.l	d3,d0
	move.l	#31250,d7
	divu	d7,d0
	moveq	#0,d4
	move	d0,d4
	lsr.l	#4,d4
	addq.l	#1,d4
	lsr.l	#1,d4
	subq.l	#2,d4
	move.l	d4,d0
	addq.l	#2,d0
	asl.l	#5,d0
	move.l	d3,d5
	divu	d0,d5
	moveq	#0,d1
	move	d5,d1
	move	d7,d0
	cmp	d1,d0
	bcc	@f
	exg	d0,d1
@@:
	sub	d1,d0
	moveq	#0,d1
	move	d0,d1
	move.l	d1,d0
	add.l	d0,d0
	add.l	d1,d0
	asl.l	#3,d0
	add.l	d1,d0
	asl.l	#2,d0
	divu	d7,d0
	movea	d0,a0
	move.l	#'.)  ',rsmidi_mes4-work(a6)
	move	#'  ',rsmidi_mes5-work(a6)
	tst	d0
	beq	1f

	swap	d0
	add	d0,d0
	cmp	d0,d7
	bcc	@f

	move.l	#'+α.',rsmidi_mes4-work(a6)
	move	#') ',rsmidi_mes5-work(a6)
@@:
	swap	d0
1:
	move.l	#'3125',d5
	moveq	#2,d2
	cmp.b	d0,d2
	bcc	9f

	move.l	d3,d0
	move.l	#38400,d7
	divu	d7,d0
	moveq	#0,d6
	move	d0,d6
	lsr.l	#4,d6
	addq.l	#1,d6
	lsr.l	#1,d6
	subq.l	#2,d6
	move.l	d6,d0
	addq.l	#2,d0
	asl.l	#5,d0
	move.l	d3,d5
	divu	d0,d5
	moveq	#0,d1
	move	d5,d1
	move	d7,d0
	cmp	d1,d0
	bcc	@f
	exg	d0,d1
@@:
	sub	d1,d0
	moveq	#0,d1
	move	d0,d1
	move.l	d1,d0
	add.l	d0,d0
	add.l	d1,d0
	asl.l	#3,d0
	add.l	d1,d0
	asl.l	#2,d0
	divu	d7,d0
	movea	d0,a1
	move.l	#'.)  ',rsmidi_mes4-work(a6)
	move	#'  ',rsmidi_mes5-work(a6)
	tst	d0
	beq	1f

	swap	d0
	add	d0,d0
	cmp	d0,d7
	bcc	@f

	move.l	#'+α.',rsmidi_mes4-work(a6)
	move	#') ',rsmidi_mes5-work(a6)
@@:
	swap	d0
1:
	move.l	#'3840',d5
	moveq	#2,d2
	cmp.b	d0,d2
	bcc	@f

	move.l	#'+α.',rsmidi_mes4-work(a6)
	move	#') ',rsmidi_mes5-work(a6)
	move.b	#7,rswarn_mes-work(a6)	*TAB
@@:
	move.l	#'3125',d5
	move	a1,d1
	cmp	a0,d1
	bcc	9f
	move.l	#'3840',d5
	move	d6,d4
9:
	moveq	#0,d1
	move	d4,d1
	add.b	#'0',d4
	cmpi.b	#'9'+1,d4
	bcs	@f
	add.b	#'A'-'0',d4
@@:
	move.b	d4,rsmidi_mes1-work(a6)
	move.l	d5,rsmidi_mes3-work(a6)
	movem.l	(sp)+,d0/d2-d7/a0-a1
	rts

rsmidi_mes:
	dc.b	'SCC Bitrate Generator Value = $0'
rsmidi_mes1:
	dc.b	'3.'
rsmidi_mes2:
	dc.b	' (RS-MIDI Bitrate = '
rsmidi_mes3:
	dc.b	'31250bps'
rsmidi_mes4:
	dc.b	'.)  '
rsmidi_mes5:
	dc.b	'  ',13,10
rswarn_mes:
	dc.b	0
	dc.b	"WARNING: 'α' value was greater than 2%."
	dc.b	13,10,0
	.even

