Z_MUSIC	macro	func		*ドライバへのファンクションコール
	moveq.l	func,d1
	trap	#3
	endm

se_play1:
	* < d0.l=sound number
	* - all
	movem.l	d0-d2/a0-a1,-(sp)
	add.w	d0,d0
	move.w	se_tbl(pc,d0.w),d0
	lea	se_tbl(pc,d0.w),a1
	moveq.l	#7,d2			*割り込むトラック番号
	Z_MUSIC	#$12
	movem.l	(sp)+,d0-d2/a0-a1
	rts

se_play2:
	* < d0.l=sound number
	* < d2.l=ch number
	* - all
	movem.l	d0-d2/a0-a1,-(sp)
	add.w	d0,d0
	move.w	se_tbl(pc,d0.w),d0
	lea	se_tbl(pc,d0.w),a1
	move.b	d2,7(a1)		*絶対チャンネルセット
	addq.b	#1,d2			*割り込むトラック番号
	Z_MUSIC	#$12
	movem.l	(sp)+,d0-d2/a0-a1
	rts

se_play3:
	* < d0.l=sound number
	* - all
	movem.l	d0-d2/a0-a1,-(sp)
	add.w	d0,d0
	move.w	se_tbl(pc,d0.w),d0
	lea	se_tbl(pc,d0.w),a1
	lea	which_ch(pc),a0
	eori.b	#1,(a0)
	move.b	(a0),d2
	addq.b	#6,d2
	move.b	d2,7(a1)		*絶対チャンネルセット
	addq.b	#1,d2			*割り込むトラック番号
	Z_MUSIC	#$12
	movem.l	(sp)+,d0-d2/a0-a1
	rts

which_ch:	dc.w	0

se_tbl:
	dc.w	se0-se_tbl		*$00
	dc.w	se1-se_tbl		*$01
	dc.w	se2-se_tbl		*$02
	dc.w	se3-se_tbl		*$03
	dc.w	se4-se_tbl		*$04
			:
			:
			:

se0:
	.dc.b	$00,$01			演奏総トラック数(.W)
	.dc.b	$00,$00,$00,$02		演奏データまでのオフセット(.L)
	.dc.b	$00,$07			演奏絶対チャンネル(.W)
	.dc.b	$a0,$0c,$b6,$05,$b3,$d1,$00,$00		演奏データ
	.dc.b	$00,$00,$e6,$00,$3c,$e8,$00,$18,$ff,$ff,$d6,$00,$03,$00,$00,$e0
	.dc.b	$39,$00,$0c,$ff,$ff,$00,$00,$00,$40,$00,$01,$45,$48,$ff,$e0,$45
	.dc.b	$00,$0c,$00,$0c,$00,$00,$ff,$c0,$00,$ff,$ff
	.even
se1:
	.dc.b	$00,$02			演奏総トラック数(.w)
	.dc.b	$00,$00,$00,$08		演奏データまでのオフセット(.L)
	.dc.b	$00,$06			演奏絶対チャンネル(.W)
	.dc.b	$00,$00,$00,$29		演奏データまでのオフセット(.L)
	.dc.b	$00,$07			演奏絶対チャンネル(.W)
	.dc.b	$a0,$07,$b6,$02,$b3,$d1,$00,$00		６チャンネル用演奏データ
	.dc.b	$00,$00,$64,$01,$ff,$73,$01,$ff,$54,$01,$ff,$56,$01,$ff,$70,$01
	.dc.b	$ff,$e0,$65,$00,$18,$00,$18,$00,$00,$00,$20,$00,$01,$b0,$ff
	.dc.b	$a0					７チャンネル用演奏データ
	.dc.b	$04,$b6,$0a,$b3,$d1,$00,$00,$00,$00,$e0,$60,$00,$06,$ff,$ff,$00
	.dc.b	$00,$ff,$80,$00,$ff,$e0,$54,$00,$06,$00,$06,$00,$00,$00,$aa,$ab
	.dc.b	$01,$80,$c0,$c0,$ff
	.even
			:
			:
			:
			:
