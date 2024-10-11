
Z_MUSIC	macro	func		*ドライバへのファンクションコール
	moveq.l	func,d1
	trap	#3
	endm

ADPCM_se_play1:					*ADPCM効果音
	*
	*任意のアドレスに格納されたADPCM音を効果音として鳴らす
	*
	* < d0.l=効果音番号(0～255)
	*
	movem.l	d0-d3/a0-a1,-(sp)
	add.w	d0,d0
	add.w	d0,d0
	move.w	d0,d1
	add.w	d0,d0
	add.w	d1,d0		*d0.w=d0.w×12
	movem.l	adr_frq_tbl(pc,d0.w),d2-d3/a1
	Z_MUSIC	#$13
	movem.l	(sp)+,d0-d3/a0-a1
	rts

adr_frq_tbl:
	*	ADPCMデータのサイズ(.L)		+0
	*	優先レベル＆ＰＡＮ＆ＦＲＱ(.L)	+4
	*	ADPCMデータの開始アドレス(.L)	+8
	*
	dc.l	2212,$0000_0203,adpcm_se0	*0
	dc.l	3478,$0001_0403,adpcm_se1	*1
	dc.l	5453,$0000_0301,adpcm_se2	*2
	dc.l	1401,$0001_0402,adpcm_se3	*3
	dc.l	8799,$0000_0403,adpcm_se4	*4
			:
			:
			:
			:


ADPCM_se_play2:				
	*
	*ZMUSICに登録したADPCM音を効果音として鳴らす
	*
	* < d0.l=効果音番号(0～255)
	*
	movem.l	d0-d3/a0-a1,-(sp)
	move.l	d0,d2
	add.w	d0,d0
	add.w	d0,d0
	move.l	frq_pan_tbl(pc,d0.w),d3
	Z_MUSIC	#$14
	movem.l	(sp)+,d0-d3/a0-a1
	rts

frq_pan_tbl:				*効果音番号
	*	優先レベル＆ＰＡＮ＆ＦＲＱ(.L)	
	dc.l	$0000_0203		*0
	dc.l	$0000_0303		*1
	dc.l	$0000_0201		*2
	dc.l	$0001_0402		*3
	dc.l	$0001_0203		*4
	dc.l	$0001_0103		*5
			:
			:
			:
			:
