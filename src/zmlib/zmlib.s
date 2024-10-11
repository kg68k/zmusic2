*		ＺＭＵＳＩＣ．Ｘ用 Ｃ用関数
*
*		   ＺＭＵＳＩＣ．Ｌ v2.00
*
*  参考文献
*	     ZMUSICLIB.L (C)砂塚佳成 ＠ＳＵＮ.
*
	.include	doscall.mac
	.include	iocscall.mac

param1		equ	4
param2		equ	8
param3		equ	12
param4		equ	16
param5		equ	20
param6		equ	24
param7		equ	28
param8		equ	32
param9		equ	36

tr_max:		equ	80	*確保出来るトラック数の最大値

Z_MUSIC	macro	func		*ドライバへのファンクションコール
	moveq.l	func,d1
	trap	#3
	endm

	.global	_m_assign		*1
	.global	_m_alloc		*2
	.global	_m_vget			*3
	.global	_m_vset			*4
	.global	_m_trk			*5
	.global	_m_play			*6
	.global	_m_stop			*7
	.global	_m_cont			*8
	.global	_m_init			*9
	.global	_m_stat			*10
	.global	_m_free			*11
	.global	_m_tempo		*12
	.global	_m_atoi			*13
	.global	_m_assign2		*14
	.global	_m_ch			*15
	.global	_m_pcmset		*16
	.global	_m_pcmplay		*17
	.global	_m_rec			*18
	.global	_m_save			*19
	.global	_m_rstop		*20
	.global	_m_trans		*21
	.global	_m_fmvset		*22
	.global	_m_out			*23
	.global	_m_dirout		*24
	.global	_m_exc			*25
	.global	_m_roland		*26
	.global	_m_total		*27
	.global	_m_fadeout		*28
	.global	_m_pcmcnf		*29
*	.global	_m_switch		*30
	.global	_sc55_v_reserve		*31
	.global	_sc55_reverb		*32
	.global	_sc55_chorus		*33
	.global	_sc55_part_setup	*34
	.global	_sc55_drum_setup	*35
	.global	_sc55_print		*36
	.global	_sc55_display		*37
	.global	_m_adpcm_block		*38
	.global	_mt32_p_reserve		*39
	.global	_mt32_reverb		*40
	.global	_mt32_part_setup	*41
	.global	_mt32_drum_setup	*42
	.global	_mt32_common		*43
	.global	_mt32_patch		*44
	.global	_mt32_partial		*45
	.global	_mt32_print		*46
	.global	_m_print		*47
	.global	_u220_print		*48
	.global	_u220_setup		*49
	.global	_u220_common		*50
	.global	_u220_drum_setup	*51
	.global	_u220_part_setup	*52
	.global	_u220_timbre		*53
	.global	_u220_drum_inst		*54
	.global	_m1_midi_ch		*55
	.global	_m1_part_setup		*56
	.global	_m1_effect_setup	*57
	.global	_m1_print		*58
	.global	_send_to_m1		*59
	.global	_zmd_play		*60
	.global	_m_debug		*61
	.global	_m_count		*62
	.global	_fm_master		*63
	.global	_m_mute			*64
	.global	_m_solo			*65
	.global	_m_wave_form		*66
	.global	_m_wave_form2		*66_
	.global	_sc55_init		*67
	.global	_mt32_init		*68

	.global	_adpcm_to_pcm		*69
	.global	_pcm_to_adpcm		*70

	.global	_exec_zms		*71
	.global	_m_inp			*72

	.global	_zm_ver			*73

	.global	_m_trk2			*74

	.global	_zm_work		*75

_m_alloc:			*トラックバッファの確保
	move.l	param1+2(sp),d2	*get trk number
	move.w	param2+2(sp),d2
	Z_MUSIC	#$01
	rts

_m_assign:			*チャンネルアサイン
	move.l	param1+2(sp),d2	*get ch
	move.w	param2+2(sp),d2	*get tr
	Z_MUSIC	#$02
	rts

_m_vget:
	move.l	param1(sp),d2	*sound number
	movea.l	param2(sp),a1	*配列
	Z_MUSIC	#$03
	rts

_m_vset:
	move.l	param1(sp),d2	*sound number
	movea.l	param2(sp),a1	*配列
	Z_MUSIC	#$04
	rts

_m_tempo:
	move.l	param1(sp),d2
	cmpi.l	#$ffff,d2
	bls	@f
	moveq.l	#-1,d2
@@:
	Z_MUSIC	#$05
	rts

_m_trk:
	move.l	param1(sp),d2	*get trk
	movea.l	param2(sp),a1	*get str address
	Z_MUSIC	#$06
	rts

_m_free:
	move.l	param1(sp),d2	*get trk
	Z_MUSIC	#$07
	rts

_m_play:
	movem.l	d3-d5,reg_buf
	bsr	bit_pat_set
	Z_MUSIC	#$08
	movem.l	reg_buf(pc),d3-d5
	rts

_m_stat:
	moveq.l	#0,d2
	move.l	param1(sp),d0
	beq	@f
	subq.b	#1,d0
	bset.l	d0,d2
@@:
	Z_MUSIC	#$09
	rts

_m_stop:
	movem.l	d3-d5,reg_buf
	bsr	bit_pat_set
	Z_MUSIC	#$0a
	movem.l	reg_buf(pc),d3-d5
	rts

_m_cont:
	movem.l	d3-d5,reg_buf
	bsr	bit_pat_set
	Z_MUSIC	#$0b
	movem.l	reg_buf(pc),d3-d5
	rts

_m_init:
	Z_MUSIC	#$00
	rts

_m_atoi:
	move.l	param1(sp),d2
	Z_MUSIC	#$0c
	rts

_m_assign2:
	movea.l	param1(sp),a0	*文字列アドレス
				*文字でチャンネルアサイン
	* < (a0)=str addr.
	* > (a0)=next
	* > d1=ch value
	bsr	skip_spc2
	move.b	(a0)+,d0
	bsr	mk_capital
	moveq.l	#0,d1
	moveq.l	#0,d2
	cmpi.b	#'F',d0
	beq	gsc_srch_num
	cmpi.b	#'A',d0
	beq	adp??
	cmpi.b	#'P',d0
	bne	@f
adp??:				*PCM8を考慮
	bsr	srch_num
	bmi	adp1
	bsr	get_num
	move.l	d0,d1
	beq	adp1
	subq.l	#1,d1
	beq	adp1_
	cmpi.l	#7,d1
	bhi	error		*illegal ch
	add.b	#24,d1
	bra	gsc
adp1_:
	move.w	#$8008,d1
	bra	gsc
adp1:
	moveq.l	#8,d1
	bra	gsc
@@:
	cmpi.b	#'M',d0
	bne	error		*illegal ch
	moveq.l	#9,d2
gsc_srch_num:
	bsr	srch_num
	bmi	error
	bsr	get_num
	move.l	d0,d1
	cmpi.l	#1,d1
	blt	error
	add.l	d2,d1
	subq.w	#1,d1
gsc:
	move.l	d1,-(sp)	*save ch
	Z_MUSIC	#$3a		*d0=real_ch_tbl
	movea.l	d0,a0
	move.l	(sp)+,d1
	moveq.l	#32-1,d2
	moveq.l	#1,d0
gsc1_lp:
	cmp.b	(a0)+,d1
	beq	exit_gsc1
	addq.w	#1,d0
	dbra	d2,gsc1_lp
	bra	error
exit_gsc1:
	move.w	d0,d2
	swap	d2
	move.w	param2+2(sp),d2
	Z_MUSIC	#$02
	rts
error:
	moveq.l	#-1,d0
	rts

_m_ch:				*ベースチャンネル切り換え
	move.l	param1(sp),a0	*文字列アドレス
	move.b	(a0)+,d0
	bsr	mk_capital
	moveq.l	#0,d2
	cmpi.b	#'M',d0
	seq	d2		*Mならd2=$ff
	Z_MUSIC	#$15
	rts

_m_pcmset:			*ＡＤＰＣＭデータリード
	movem.l	d3-d7,reg_buf
	move.l	param1(sp),d2	*d2=to set note number
	move.l	param2(sp),a1	*a1=file name
	moveq.l	#0,d3
	move.l	#$ffff,d4
	moveq.l	#0,d5
	moveq.l	#0,d6
	moveq.l	#0,d7
	move.l	#$000c_0064,d3	*non pitch and 100%
	cmpi.l	#'NASI',param3(sp)
	beq	@f
	move.l	param3(sp),d0	*get pitch param.
	swap	d3
	move.w	d0,d3
	swap	d3
@@:
	cmpi.l	#'NASI',param4(sp)
	beq	@f
	move.l	param4(sp),d0	*get vol. param.
	move.w	d0,d3
@@:
	cmpi.l	#'NASI',param5(sp)
	beq	@f
	move.l	param5(sp),d0	*get mix param.
	move.w	d0,d4
@@:
	cmpi.l	#'NASI',param6(sp)
	beq	@f
	move.l	param6(sp),d0	*get delay param.
	swap	d4
	move.w	d0,d4
	swap	d4
@@:
	cmpi.l	#'NASI',param7(sp)
	beq	@f
	move.l	param7(sp),d5	*get cut param.
@@:
	cmpi.l	#'NASI',param8(sp)
	beq	@f
	move.l	param8(sp),d6	*get reverse param.
@@:
	cmpi.l	#'NASI',param9(sp)
	beq	@f
	move.l	param9(sp),d7	*get fade param.
@@:
	Z_MUSIC	#$10
	movem.l	reg_buf(pc),d3-d7
	rts

_m_pcmplay:			*adpcm dataの演奏
	move.l	param1(sp),d2	*get note
	move.l	param2(sp),d0	*get pan
	move.l	param3(sp),d1	*get frq
	move.l	d3,-(sp)
	lsl.w	#8,d1
	or.w	d1,d0
	move.l	d0,d3
	Z_MUSIC	#$14
	move.l	(sp)+,d3
	rts

_m_rec:				*MIDIデータ録音
	Z_MUSIC	#$16
	rts

_m_rstop:			*MIDIデータ録音停止
	moveq.l	#0,d2		*ascii data作成モード
	Z_MUSIC	#$17
	move.l	a0,d1
	beq	error
	move.b	#$1a,(a0,d0.l)
	lea	rec_address(pc),a1
	move.l	a0,(a1)+
	addq.l	#1,d0
	move.l	d0,(a1)+
	rts

_m_save:				*MIDIデータセーブ
	movem.l	d4-d5,reg_buf
	move.l	rec_size(pc),d4		*size
	bne	@f
	moveq.l	#0,d2		*ascii data作成モード
	Z_MUSIC	#$17
	move.l	a0,d1
	beq	_m_save_err
	move.b	#$1a,(a0,d0.l)
	lea	rec_address(pc),a1
	move.l	a0,(a1)+
	addq.l	#1,d0
	move.l	d0,(a1)+
	move.l	d0,d4
	beq	_m_save_err		*file size=0
@@:
	move.l	param1(sp),a0
	move.w	#32,-(sp)
	pea	(a0)		*move.l	12(sp),-(sp)	*filename
	DOS	_CREATE
	addq.w	#6,sp
	move.l	d0,d5		*d5.w=file handle
	bmi	_m_save_err

	move.l	d4,-(sp)		*data size
	move.l	rec_address(pc),-(sp)	*data addr
	move.w	d5,-(sp)
	DOS	_WRITE
	lea	10(sp),sp
	tst.l	d0
	bmi	_m_save_err

	move.w	d5,-(sp)
	DOS	_CLOSE
	addq.w	#2,sp
	movem.l	reg_buf(pc),d4-d5
	rts
_m_save_err:
	moveq.l	#-1,d0
	movem.l	reg_buf(pc),d4-d5
	rts

_m_trans:			*MIDIデータファイルの送信
	movem.l	d3-d7/a3-a5,reg_buf
	move.l	param1(sp),a2	*a2=file name
	bsr	fopen
	tst.l	d5
	bmi	_m_trans_err	*read error
	bsr	read		*return=a5:address,d3:size
	bmi	_m_trans_err	*read error
	moveq.l	#0,d2		*ascii mode
	movea.l	a5,a1		*address
	Z_MUSIC	#$18
	pea	(a5)
	DOS	_MFREE
	addq.w	#4,sp
	movem.l	reg_buf(pc),d3-d7/a3-a5
	rts
_m_trans_err:
	moveq.l	#-1,d0
	movem.l	reg_buf(pc),d3-d7/a3-a5
	rts

_m_fmvset:			*ＦＭ音源音色登録その2
	move.l	param1(sp),d2	*sound number
	movea.l	param2(sp),a1	*配列
	Z_MUSIC	#$1b
	rts

_m_out:				*ＭＩＤＩデータ出力
	lea	md_buf(pc),a1
	lea	param1(sp),a0
	moveq.l	#32-1,d1
moutlp:
	move.l	(a0)+,d0
	cmpi.l	#$ff,d0
	bhi	@f
	move.b	d0,(a1)+	*値を一時的にバッファへ書き込み
	dbra	d1,moutlp
@@:
	lea	md_buf(pc),a0
	suba.l	a0,a1
	move.l	a1,d2
	beq	error
	movea.l	a0,a1
	Z_MUSIC	#$18
	rts

_m_dirout:
	movea.l	param1(sp),a1	*配列
	move.l	param2(sp),d2	*get size
	beq	error
	Z_MUSIC	#$18
	rts

_m_exc:				*エクスクルーシブ転送
	movea.l	param1(sp),a1	*配列
	move.l	param2(sp),d2	*get size
	beq	error
	Z_MUSIC	#$1d
	rts

_m_roland:			*ROLANDエクスクルーシブ転送
	move.l	d3,a2		*push d3
	move.l	param1(sp),d3	*dev id
	lsl.w	#8,d3
	move.l	param2(sp),d1	*model id
	move.b	d1,d3
	movea.l	param3(sp),a1	*配列
	move.l	param4(sp),d2	*get size
	beq	error
	Z_MUSIC	#$1c
	move.l	a2,d3		*pop d3
	rts

_m_total:			*ステップタイムのトータル値表示
	moveq.l	#0,d2
	Z_MUSIC	#$19
	rts

_m_fadeout:			*フェードアウト
	moveq.l	#16,d2
	move.l	param1(sp),d1
	bpl	@f
	neg.l	d1
@@:
	cmpi.l	#85,d1
	bhi	@f
	move.l	param1(sp),d2
@@:
	Z_MUSIC	#$1a
	rts

_m_pcmcnf:			*ＡＤＰＣＭコンフィギュレーションファイルセット
	movem.l	d3-d7/a3-a5,reg_buf
	move.l	param1(sp),a2
	bsr	fopen
	tst.l	d5
	bmi	_m_pcmcnf_err	*read error
	bsr	read		*return a5=address,d3=size
	bmi	_m_trans_err	*read error
	lea	OPM(PC),a2
	bsr	self_output
	pea	(a5)
	DOS	_MFREE
	addq.w	#4,sp
	movem.l	reg_buf(pc),d3-d7/a3-a5
	rts
_m_pcmcnf_err:
	moveq.l	#-1,d0
	movem.l	reg_buf(pc),d3-d7/a3-a5
	rts

_sc55_v_reserve:		*sc55 voice resereve
	move.l	d3,a2		*push d3
	move.l	param1(sp),a1
	moveq.l	#-1,d3
	cmpi.l	#$7f,param2(sp)
	bhi	@f
	move.l	param2(sp),d3	*get dev id
@@:
	moveq.l	#16,d2
	Z_MUSIC	#$1e
	move.l	a2,d3		*pop d3
	rts

_sc55_reverb:			*sc55 reverb parameter
	move.l	d3,a2		*push d3
	move.l	param1(sp),a1
	moveq.l	#-1,d3
	cmpi.l	#$7f,param2(sp)
	bhi	@f
	move.l	param2(sp),d3	*get dev id
@@:
	moveq.l	#7,d2
	tst.l	param3(sp)
	beq	@f
	cmp.l	param3(sp),d2	*get size?
	bcs	@f
	move.l	param3(sp),d2
@@:
	Z_MUSIC	#$1f
	move.l	a2,d3		*pop d3
	rts

_sc55_chorus:			*sc55 chorus parameter
	move.l	d3,a2		*push d3
	move.l	param1(sp),a1
	moveq.l	#-1,d3
	cmpi.l	#$7f,param2(sp)
	bhi	@f
	move.l	param2(sp),d3	*get dev id
@@:
	moveq.l	#8,d2
	tst.l	param3(sp)
	beq	@f
	cmp.l	param3(sp),d2	*get size?
	bcs	@f
	move.l	param3(sp),d2
@@:
	Z_MUSIC	#$20
	move.l	a2,d3		*pop d3
	rts

_sc55_part_setup:
	move.l	d3,a2		*push d3
	move.l	param1(sp),d3
	cmpi.l	#16,d3
	bhi	_sc55_part_setup_err
	swap	d3
	movea.l	param2(sp),a1
	moveq.l	#-1,d1
	cmpi.l	#$7f,param3(sp)
	bhi	@f
	move.l	param3(sp),d1	*get dev id
@@:
	move.w	d1,d3
	moveq.l	#119,d2
	tst.l	param4(sp)
	beq	@f
	cmp.l	param4(sp),d2	*get size?
	bcs	@f
	move.l	param4(sp),d2
@@:
	Z_MUSIC	#$21
	move.l	a2,d3		*pop d3
	rts
_sc55_part_setup_err:
	moveq.l	#-1,d0
	move.l	a2,d3		*pop d3
	rts

_sc55_drum_setup:		*sc55 drum parameter
	move.l	d3,a2		*push d3
	move.l	param1(sp),d3	*get map number
	cmpi.l	#1,d3
	bhi	_sc55_drum_setup_err	*1以上はエラー
	move.l	param2(sp),d0	*get note number
	cmpi.l	#127,d0
	bhi	_sc55_drum_setup_err
	lsl.w	#8,d3
	move.b	d0,d3
	swap	d3
	movea.l	param3(sp),a1
	moveq.l	#-1,d1
	cmpi.l	#$7f,param4(sp)
	bhi	@f
	move.l	param4(sp),d1	*get dev id
@@:
	move.w	d1,d3
	moveq.l	#8,d2
	tst.l	param5(sp)
	beq	@f
	cmp.l	param5(sp),d2	*get size?
	bcs	@f
	move.l	param5(sp),d2
@@:
	Z_MUSIC	#$22
	move.l	a2,d3		*pop d3
	rts
_sc55_drum_setup_err:
	moveq.l	#-1,d0
	move.l	a2,d3		*pop d3
	rts

_sc55_print:			*SC55に文字列を表示する
	move.l	d3,a2		*push d3
	move.l	param1(sp),a1
	move.l	a1,a0
	moveq.l	#0,d2
@@:
	addq.w	#1,d2		*count length
	tst.b	(a0)+
	bne	@b
	subq.w	#1,d2
	beq	_sc55_print_err
	cmpi.w	#32,d2
	bhi	_sc55_print_err	*文字データ多すぎ
	moveq.l	#-1,d3
	cmpi.l	#$7f,param2(sp)
	bhi	@f
	move.l	param2(sp),d3	*get dev id
@@:
	Z_MUSIC	#$23
	move.l	a2,d3		*pop d3
	rts
_sc55_print_err:
	moveq.l	#-1,d0
	move.l	a2,d3		*pop d3
	rts

_sc55_display:			*SC55のレベル・ディスプレイに表示する
	move.l	d3,a2		*push d3
	movea.l	param1(sp),a0
	moveq.l	#-1,d3
	cmpi.l	#$7f,param2(sp)
	bhi	@f
	move.l	param2(sp),d3	*get dev id
@@:
	lea	env_work(pc),a1
	moveq.l	#16-1,d2
@@:
	move.l	(a0)+,d0
	move.w	d0,(a1)+
	dbra	d2,@b
	lea	env_work(pc),a1
	Z_MUSIC	#$24
	move.l	a2,d3		*pop d3
	rts

_m_adpcm_block:			*ブロックデータの読み込み
	move.l	param1(sp),a1
	Z_MUSIC	#$39
	rts

_mt32_p_reserve:		*mt32パーシャルリザーブ
	move.l	d3,a2		*push d3
	movea.l	param1(sp),a1
	moveq.l	#-1,d3
	cmpi.l	#$7f,param2(sp)
	bhi	@f
	move.l	param2(sp),d3	*get dev id
@@:
	moveq.l	#9,d2
	Z_MUSIC	#$25
	move.l	a2,d3		*pop d3
	rts

_mt32_reverb:			*mt32リバーブ・パラメータ設定
	move.l	d3,a2		*push d3
	movea.l	param1(sp),a1
	moveq.l	#-1,d3
	cmpi.l	#$7f,param2(sp)
	bhi	@f
	move.l	param2(sp),d3	*get dev id
@@:
	moveq.l	#3,d2
	tst.l	param3(sp)
	beq	@f
	cmp.l	param3(sp),d2	*get size?
	bcs	@f
	move.l	param3(sp),d2
@@:
	Z_MUSIC	#$26
	move.l	a2,d3		*pop d3
	rts

_mt32_part_setup:		*mt32パートＭＩＤＩチャンネル設定
	move.l	d3,a2		*push d3
	movea.l	param1(sp),a1
	moveq.l	#-1,d3
	cmpi.l	#$7f,param2(sp)
	bhi	@f
	move.l	param2(sp),d3	*get dev id
@@:
	moveq.l	#9,d2
	tst.l	param3(sp)
	beq	@f
	cmp.l	param3(sp),d2	*get size?
	bcs	@f
	move.l	param3(sp),d2
@@:
	Z_MUSIC	#$27
	move.l	a2,d3		*pop d3
	rts

_mt32_drum_setup:		*mt32ドラムセットアップ設定
	move.l	d3,a2		*push d3
	move.l	param1(sp),d3
	swap	d3
	movea.l	param2(sp),a1
	moveq.l	#-1,d1
	cmpi.l	#$7f,param3(sp)
	bhi	@f
	move.l	param3(sp),d1	*get dev id
@@:
	move.w	d1,d3
	moveq.l	#4,d2
	tst.l	param4(sp)
	beq	@f
	cmp.l	param4(sp),d2	*get size?
	bcs	@f
	move.l	param4(sp),d2
@@:
	Z_MUSIC	#$28
	move.l	a2,d3		*pop d3
	rts

_mt32_common:			*mt32音色コモンパラメータ設定
	move.l	d3,a2		*push d3
	move.l	param1(sp),d3	*timbre number
	swap	d3
	movea.l	param2(sp),a1	*name address
	tst.b	(a1)
	beq	_mt32_common_err
	moveq.l	#-1,d1
	cmpi.l	#$7f,param4(sp)
	bhi	@f
	move.l	param4(sp),d1	*get dev id
@@:
	move.w	d1,d3		*set id to d3
	lea	env_work(pc),a0
	moveq.l	#0,d0
@@:
	addq.w	#1,d0
	move.b	(a1)+,(a0)+	*set timbre name
	bne	@b
	subq.w	#1,d0
	beq	_mt32_common_err
	cmpi.b	#10,d0
	bhi	_mt32_common_err
	moveq.l	#4,d2
	tst.l	param5(sp)
	beq	@f
	cmp.l	param5(sp),d2	*get size?
	bcs	@f
	move.l	param5(sp),d2
@@:
	movea.l	param3(sp),a1
@@:
	move.b	(a1)+,(a0)+	*set parameters
	dbra	d2,@b
	lea	env_work(pc),a1	*a1=data address
	move.l	a0,d2
	sub.l	a1,d2
	Z_MUSIC	#$29
	move.l	a2,d3		*pop d3
	rts
_mt32_common_err:
	moveq.l	#-1,d0
	move.l	a2,d3		*pop d3
	rts

_mt32_patch:			*mt32パッチパラメータ設定
	move.l	d3,a2		*push d3
	move.l	param1(sp),d3
	swap	d3
	movea.l	param2(sp),a1
	moveq.l	#-1,d1
	cmpi.l	#$7f,param3(sp)
	bhi	@f
	move.l	param3(sp),d1	*get dev id
@@:
	move.w	d1,d3
	moveq.l	#7,d2
	tst.l	param4(sp)
	beq	@f
	cmp.l	param4(sp),d2	*get size?
	bcs	@f
	move.l	param4(sp),d2
@@:
	Z_MUSIC	#$2b
	move.l	a2,d3		*pop d3
	rts

_mt32_partial:			*パーシャルパラメータ設定
	move.l	d3,a2		*push d3
	move.l	param1(sp),d3	*timbre number
	lsl.w	#8,d3
	move.l	param2(sp),d0	*partial number
	move.b	d0,d3
	swap	d3
	movea.l	param3(sp),a1
	moveq.l	#-1,d1
	cmpi.l	#$7f,param4(sp)
	bhi	@f
	move.l	param4(sp),d1	*get dev id
@@:
	move.w	d1,d3		*set id to d3
	moveq.l	#58,d2
	tst.l	param5(sp)
	beq	@f
	cmp.l	param5(sp),d2	*get size?
	bcs	@f
	move.l	param5(sp),d2
@@:
	Z_MUSIC	#$2a
	move.l	a2,d3		*pop d3
	rts

_mt32_print:			*MT32に文字列を表示する
	move.l	d3,a2		*push d3
	move.l	param1(sp),a1
	move.l	a1,a0
	moveq.l	#0,d2
@@:
	addq.w	#1,d2		*count length
	tst.b	(a0)+
	bne	@b
	subq.w	#1,d2
	beq	_mt32_print_err
	cmpi.w	#20,d2
	bhi	_mt32_print_err	*文字データ多すぎ
	moveq.l	#-1,d3
	cmpi.l	#$7f,param2(sp)
	bhi	@f
	move.l	param2(sp),d3	*get dev id
@@:
	Z_MUSIC	#$2c
	move.l	a2,d3		*pop d3
	rts
_mt32_print_err:
	moveq.l	#-1,d0
	move.l	a2,d3		*pop d3
	rts

_m_print:			*文字列を表示する
	move.l	param1(sp),a1
	move.l	a1,a0
	moveq.l	#0,d2
@@:
	addq.w	#1,d2		*count length
	tst.b	(a0)+
	bne	@b
	subq.w	#1,d2
	beq	error
	cmpi.w	#96,d2
	bhi	error		*文字データ多すぎ
	moveq.l	#-1,d1
	IOCS	_B_LOCATE
	move.l	d0,-(sp)
	pea	CRLF(PC)
	DOS	_PRINT
*	addq.w	#4,sp
	pea	(a1)
	DOS	_PRINT
*	addq.w	#4,sp
	addq.w	#8,sp
	move.l	(sp)+,d2	*カーソル位置を戻す
	move.l	d2,d1
	swap	d1
	IOCS	_B_LOCATE
	rts

_u220_print:			*U220に文字列を表示する
	move.l	d3,a2		*push d3
	move.l	param1(sp),a1
	move.l	a1,a0
	moveq.l	#0,d2
@@:
	addq.w	#1,d2		*count length
	tst.b	(a0)+
	bne	@b
	subq.w	#1,d2
	beq	_u220_print_err
	cmpi.w	#12,d2
	bhi	_u220_print_err	*文字データ多すぎ
	moveq.l	#-1,d3
	cmpi.l	#$7f,param2(sp)
	bhi	@f
	move.l	param2(sp),d3	*get dev id
@@:
	Z_MUSIC	#$31
	move.l	a2,d3		*pop d3
	rts
_u220_print_err:
	moveq.l	#-1,d0
	move.l	a2,d3		*pop d3
	rts

_u220_setup:			*u220セットアップパラメータの設定
	move.l	d3,a2		*push d3
	movea.l	param1(sp),a1
	moveq.l	#-1,d3
	cmpi.l	#$7f,param2(sp)
	bhi	@f
	move.l	param2(sp),d3	*get dev id
@@:
	moveq.l	#7,d2
	Z_MUSIC	#$2d
	move.l	a2,d3		*pop d3
	rts

_u220_common:			*u220テンポラリパッチ・コモンパラメータの設定
	move.l	d3,a2		*push d3
	movea.l	param1(sp),a1
	moveq.l	#-1,d3
	cmpi.l	#$7f,param2(sp)
	bhi	@f
	move.l	param2(sp),d3	*get dev id
@@:
	moveq.l	#18,d2
	Z_MUSIC	#$2e
	move.l	a2,d3		*pop d3
	rts

_u220_drum_setup:		*u220テンポラリ・ドラム・セットアップパラメータの設定
	move.l	d3,a2		*push d3
	movea.l	param1(sp),a1
	moveq.l	#-1,d3
	cmpi.l	#$7f,param2(sp)
	bhi	@f
	move.l	param2(sp),d3	*get dev id
@@:
	moveq.l	#7,d2
	Z_MUSIC	#$2f
	move.l	a2,d3		*pop d3
	rts

_u220_part_setup:		*u220テンポラリパート・セットアップパラメータ設定
	move.l	d3,a2		*push d3
	move.l	param1(sp),d3
	swap	d3
	movea.l	param2(sp),a1
	moveq.l	#-1,d1
	cmpi.l	#$7f,param3(sp)
	bhi	@f
	move.l	param3(sp),d1	*get dev id
@@:
	move.w	d1,d3
	moveq.l	#13,d2
	Z_MUSIC	#$30
	move.l	a2,d3		*pop d3
	rts

_u220_timbre:			*U220音色パラメータ設定
	move.l	d3,a2		*push d3
	move.l	param1(sp),d3	*timbre number
	swap	d3
	movea.l	param2(sp),a1	*name address
	moveq.l	#-1,d1
	cmpi.l	#$7f,param4(sp)
	bhi	@f
	move.l	param4(sp),d1	*get dev id
@@:
	move.w	d1,d3		*set id to d3
	lea	env_work(pc),a0
	moveq.l	#0,d0
@@:
	addq.w	#1,d0
	move.b	(a1)+,(a0)+	*set timbre name
	bne	@b
	subq.w	#1,d0
	beq	_u220_timbre_err
	cmpi.w	#12,d0
	bhi	_u220_timbre_err
	movea.l	param3(sp),a1
@@:
	move.b	(a1)+,(a0)+	*set parameters
	dbra	d2,@b
	lea	env_work(pc),a1	*a1=data address
	move.l	a0,d2
	sub.l	a1,d2
	Z_MUSIC	#$32
	move.l	a2,d3		*pop d3
	rts
_u220_timbre_err:
	moveq.l	#-1,d0
	move.l	a2,d3		*pop d3
	rts

_u220_drum_inst:		*u220ドラムインストパラメータ設定
	move.l	d3,a2		*push d3
	move.l	param1(sp),d3
	swap	d3
	movea.l	param2(sp),a1
	moveq.l	#-1,d1
	cmpi.l	#$7f,param3(sp)
	bhi	@f
	move.l	param3(sp),d1	*get dev id
@@:
	move.w	d1,d3
	moveq.l	#20,d2
	tst.l	param4(sp)
	beq	@f
	cmp.l	param4(sp),d2	*get size?
	bcs	@f
	move.l	param4(sp),d2
@@:
	Z_MUSIC	#$33
	move.l	a2,d3		*pop d3
	rts

_m1_midi_ch:			*Ｍ１・ＳＯＮＧ０のＭＩＤＩチャンネル設定
	movea.l	param1(sp),a1
	Z_MUSIC	#$34
	rts

_m1_part_setup:			*Ｍ１・ＳＯＮＧ０のパートパラメータ設定
	movea.l	param1(sp),a1
	Z_MUSIC	#$36
	rts

_m1_effect_setup:		*Ｍ１・ＳＯＮＧ０のエフェクトパラメータ設定
	movea.l	param1(sp),a1
	Z_MUSIC	#$37
	rts

_m1_print:				*Ｍ１・ＳＯＮＧ０のソングネーム設定
	move.l	param1(sp),a1
	move.l	a1,a0
	moveq.l	#0,d2
@@:
	addq.w	#1,d2		*count length
	tst.b	(a0)+
	bne	@b
	subq.w	#1,d2
	beq	error
	cmpi.w	#10,d2
	bhi	error		*文字データ多すぎ
	Z_MUSIC	#$38
	rts

_send_to_m1:			*Ｍ１へ送信
	move.l	d3,a2		*push d3
	moveq.l	#-1,d3
	cmpi.l	#$7f,param1(sp)
	bhi	@f
	move.l	param1(sp),d3	*get dev id
@@:
	Z_MUSIC	#$35
	move.l	a2,d3		*pop d3
	rts

_zmd_play:			*ＺＭＤの読み込み演奏
	movem.l	d3-d7/a3-a5,reg_buf
	move.l	param1(sp),a1	*a1=file name
	lea	filename(pc),a2
	clr.b	d1
zmdplp01:				*ファイルネームのゲット
	move.b	(a1)+,d0
	cmpi.b	#'.',d0
	bne	@f
	st.b	d1		*MARK
@@:
	cmpi.b	#' ',d0
	bls	@f
	move.b	d0,(a2)+
	bra	zmdplp01
@@:
	tst.b	d1
	bne	@f
	move.b	#'.',(a2)+	*拡張子をセット
	move.b	#'Z',(a2)+
	move.b	#'M',(a2)+
	move.b	#'D',(a2)+
@@:
	clr.b	(a2)		*end code
	lea	filename(pc),a2
	bsr	fopen		*< a2=filename
	tst.l	d5
	bmi	_zmd_play_err	*read error
	bsr	read		*return=a5:address,d3:size
	bmi	_m_trans_err	*read error
	moveq.l	#0,d2		*ascii mode
	movea.l	a5,a1		*address
	cmp.l	#$105a6d75,(a1)+
	bne	_zmd_play_err
	cmp.w	#'Si',(a1)+
	bne	_zmd_play_err
	cmp.b	#'C',(a1)+
	bne	_zmd_play_err
	move.l	d3,d2
	Z_MUSIC	#$11
	lea	zmd_play_wk(pc),a0
	move.l	(a0),d0
	beq	@f
	move.l	d0,-(sp)	*前回演奏中の曲バッファを解放
	DOS	_MFREE
	addq.w	#4,sp
@@:
	move.l	a5,(a0)
	movem.l	reg_buf(pc),d3-d7/a3-a5
	rts
_zmd_play_err:
	moveq.l	#-1,d0
	movem.l	reg_buf(pc),d3-d7/a3-a5
	rts

_m_debug:			*[!]コマンドの有効/無効化
	move.l	param1(sp),d2
	Z_MUSIC	#$41
	rts

_m_count:			*全音符の絶対音長の設定
	move.l	param1(sp),d2
	Z_MUSIC	#$42
	rts

_fm_master:			*FM音源の出力バランス設定
	move.l	param1(sp),d2
	Z_MUSIC	#$3e
	rts

_m_mute:
	movem.l	d3-d5,reg_buf
	bsr	bit_pat_set
	Z_MUSIC	#$44
	movem.l	reg_buf(pc),d3-d5
	rts

_m_solo:
	movem.l	d3-d5,reg_buf
	bsr	bit_pat_set
	tst.l	d2
	beq	@f
	not.l	d2
@@:
	Z_MUSIC	#$44
	movem.l	reg_buf(pc),d3-d5
	rts

_m_wave_form:			*波形メモリセット
	move.l	d3,a2		*push d3
	move.l	param1(sp),d3	*wave number
	lsl.w	#8,d3
	move.l	param2(sp),d1	*loop type
	move.b	d1,d3
	swap	d3
	moveq.l	#0,d1
	cmpi.l	#$ffff,param3(sp)
	bhi	@f
	move.l	param3(sp),d1	*loop point
@@:
	move.w	d1,d3
	movea.l	param4(sp),a1	*配列
	move.l	#$ffff,d2
	tst.l	param5(sp)
	beq	@f
	cmp.l	param5(sp),d2	*get size?
	bcs	@f
	move.l	param5(sp),d2
@@:				*配列加工
	move.l	a1,a0
	move.l	d2,d1
	subq.w	#1,d1
@@:
	move.l	(a0)+,d0
	move.w	d0,(a1)+
	dbra	d1,@b
	movea.l	param4(sp),a1	*配列
	Z_MUSIC	#$4a
	move.l	a2,d3		*pop d3
	rts

_m_wave_form2:			*波形メモリセット(short int型)
	move.l	d3,a2		*push d3
	move.l	param1(sp),d3	*wave number
	lsl.w	#8,d3
	move.l	param2(sp),d1	*loop type
	move.b	d1,d3
	swap	d3
	moveq.l	#0,d1
	cmpi.l	#$ffff,param3(sp)
	bhi	@f
	move.l	param3(sp),d1	*loop point
@@:
	move.w	d1,d3
	movea.l	param4(sp),a1	*配列
	move.l	#$ffff,d2
	tst.l	param5(sp)
	beq	@f
	cmp.l	param5(sp),d2	*get size?
	bcs	@f
	move.l	param5(sp),d2
@@:
	Z_MUSIC	#$4a
	move.l	a2,d3		*pop d3
	rts

_sc55_init:			*SC55初期化
	move.l	d3,a2		*push d3
	moveq.l	#-1,d3
	cmpi.l	#$7f,param1(sp)
	bhi	@f
	move.l	param1(sp),d3	*get dev id
@@:
	Z_MUSIC	#$51
	move.l	a2,d3		*pop d3
	rts

_mt32_init:			*MT32初期化
	move.l	d3,a2		*push d3
	moveq.l	#-1,d3
	cmpi.l	#$7f,param1(sp)
	bhi	@f
	move.l	param1(sp),d3	*get dev id
@@:
	Z_MUSIC	#$52
	move.l	a2,d3		*pop d3
	rts

_adpcm_to_pcm:
	move.l	param1(sp),a0
	move.l	param2(sp),d0
	move.l	param3(sp),a1
	movem.l	d3-d7/a3-a6,-(sp)
	bsr	just_adpcm_to_pcm
	movem.l	(sp)+,d3-d7/a3-a6
	rts

_pcm_to_adpcm:
	move.l	param1(sp),a1
	move.l	param2(sp),d0
	move.l	param3(sp),a0
	movem.l	d3-d7/a3-a6,-(sp)
	bsr	pcm_to_adpcm
	movem.l	(sp)+,d3-d7/a3-a6
	rts

_exec_zms:			*ＺＭＳコマンド実行
	move.l	param1(sp),a1
	movem.l	d3-d7/a3-a5,-(sp)
	lea	env_work(pc),a2
	move.l	a2,a5
	moveq.l	#0,d3
@@:
	move.b	(a1)+,(a2)+
	beq	@f
	addq.l	#1,d3
	bra	@b
@@:
	move.b	#$0d,-1(a2)
	move.b	#$0a,(a2)+
	move.b	#$1a,(a2)+
	addq.l	#3,d3
	lea	OPM(PC),a2
	bsr	self_output	*<d3.l=size,a5.l=data address
	movem.l	(sp)+,d3-d7/a3-a5
	rts

_m_inp:
	move.l	param1(sp),d2
	Z_MUSIC	#$55
	move.l	a0,d1
	beq	error
	rts

_zm_ver:
	clr.l	-(sp)
	DOS	_SUPER
	addq.w	#4,sp
	move.l	d0,-(sp)

	moveq.l	#0,d1
	movea.l	$88.w,a0	*PCM8常駐チェック
	move.l	-8(a0),d0
	move.b	#$20,d0
	cmpi.l	#'PCM ',d0
	bne	@f
	moveq.l	#-1,d1
@@:
	clr.w	d1
	move.l	$8c.w,a0
	subq.w	#8,a0
	cmpi.l	#'ZmuS',(a0)+
	bne	@f
	cmpi.w	#'iC',(a0)+
	bne	@f
	move.w	(a0)+,d1
@@:
	DOS	_SUPER		*見付けた
	addq.w	#4,sp
	move.l	d1,d0
	rts

_m_trk2:
	movea.l	param1(sp),a1	*get str address
	lea.l	param2(sp),a2
	move.l	d3,-(sp)
	moveq.l	#8-1,d3
@@:
	move.l	(a2)+,d2	*get trk
	beq	@f
	cmpi.l	#tr_max,d2
	bhi	@f
	Z_MUSIC	#$06
	dbra	d3,@b
@@:
	move.l	(sp)+,d3
	rts

_zm_work:
	move.l	param1(sp),d2
	Z_MUSIC	#$3c
	moveq.l	#0,d0
	move.l	param2(sp),d2
	move.b	(a0,d2.l),d0
	rts

iyan

just_adpcm_to_pcm:		*ピッチチェンジやレベルチェンジを
				*行わない単なるADPCM→PCM変換
	* < a0=adpcm data buffer
	* < a1=pcm data buffer
	* < d0.l=adpcm data count
	lea	scaleval(pc),a5
	lea	levelchg(pc),a6
	moveq.l	#0,d2
	moveq.l	#0,d3
	moveq.l	#0,d7
	moveq.l	#$0f,d4
	add.l	d0,d0
	lea	last_val(pc),a3
	clr.w	(a3)
__atp_lp:
	move.b	(a0),d1
	and.w	d4,d1
	tst.b	d4
	bpl	__neg_d4
	lsr.b	#4,d1		*get 4bit data
	addq.w	#1,a0
__neg_d4:
	not.b	d4
	bsr	calc_pcm_val	*実際の計算
	move.w	d2,(a1)+	*add pcm data to buffer
	subq.l	#1,d0
	bne	__atp_lp
	rts

calc_pcm_val:
	* < d1.b=adpcm value
	* < d7.w=scale level
	* > d2.w=pcm value
	* > d7.w=next scale level
	* > d1.b=adpcm*2
	* < a3.l=last_val
	* X d3 d2
	add.b	d1,d1
calc_pcm_val_:
	add.b	d7,d7
	move.w	(a5,d7.w),d3	*=d
	lsr.b	d7

	move.w	cpv(pc,d1.w),d2
	jmp	cpv(pc,d2.w)
abc:
	add.w	(a3),d2
*	bsr	chk_ovf
	move.w	d2,(a3)		*d2=pcmdata

	add.w	(a6,d1.w),d7		*scalelevl+=levelchg(adpcm value)
	bmi	rst_sclv
	cmpi.w	#48,d7
	bls	allend
	moveq.l	#48,d7
allend:
	rts
rst_sclv:
	moveq.l	#0,d7
	rts

cpv:
	dc.w	cpv0000-cpv
	dc.w	cpv0001-cpv
	dc.w	cpv0010-cpv
	dc.w	cpv0011-cpv
	dc.w	cpv0100-cpv
	dc.w	cpv0101-cpv
	dc.w	cpv0110-cpv
	dc.w	cpv0111-cpv
	dc.w	cpv1000-cpv
	dc.w	cpv1001-cpv
	dc.w	cpv1010-cpv
	dc.w	cpv1011-cpv
	dc.w	cpv1100-cpv
	dc.w	cpv1101-cpv
	dc.w	cpv1110-cpv
	dc.w	cpv1111-cpv

cpv0000:
	lsr.w	#3,d3
	move.w	d3,d2
	bra	abc
cpv0001:
	lsr.w	#2,d3
	move.w	d3,d2
	lsr.w	d3
	add.w	d3,d2
	bra	abc
cpv0010:
	lsr.w	d3
	move.w	d3,d2
	lsr.w	#2,d3
	add.w	d3,d2
	bra	abc
cpv0011:
	lsr.w	d3
	move.w	d3,d2
	lsr.w	d3
	add.w	d3,d2
	lsr.w	d3
	add.w	d3,d2
	bra	abc
cpv0100:
	move.w	d3,d2
	lsr.w	#3,d3
	add.w	d3,d2
	bra	abc
cpv0101:
	move.w	d3,d2
	lsr.w	#2,d3
	add.w	d3,d2
	lsr.w	d3
	add.w	d3,d2
	bra	abc
cpv0110:
	move.w	d3,d2
	lsr.w	d3
	add.w	d3,d2
	lsr.w	#2,d3
	add.w	d3,d2
	bra	abc
cpv0111:
	move.w	d3,d2
	lsr.w	d3
	add.w	d3,d2
	lsr.w	d3
	add.w	d3,d2
	lsr.w	d3
	add.w	d3,d2
	bra	abc
cpv1000:
	lsr.w	#3,d3
	move.w	d3,d2
	neg.w	d2
	bra	abc
cpv1001:
	lsr.w	#2,d3
	move.w	d3,d2
	lsr.w	d3
	add.w	d3,d2
	neg.w	d2
	bra	abc
cpv1010:
	lsr.w	d3
	move.w	d3,d2
	lsr.w	#2,d3
	add.w	d3,d2
	neg.w	d2
	bra	abc
cpv1011:
	lsr.w	d3
	move.w	d3,d2
	lsr.w	d3
	add.w	d3,d2
	lsr.w	d3
	add.w	d3,d2
	neg.w	d2
	bra	abc
cpv1100:
	move.w	d3,d2
	lsr.w	#3,d3
	add.w	d3,d2
	neg.w	d2
	bra	abc
cpv1101:
	move.w	d3,d2
	lsr.w	#2,d3
	add.w	d3,d2
	lsr.w	d3
	add.w	d3,d2
	neg.w	d2
	bra	abc
cpv1110:
	move.w	d3,d2
	lsr.w	d3
	add.w	d3,d2
	lsr.w	#2,d3
	add.w	d3,d2
	neg.w	d2
	bra	abc
cpv1111:
	move.w	d3,d2
	lsr.w	d3
	add.w	d3,d2
	lsr.w	d3
	add.w	d3,d2
	lsr.w	d3
	add.w	d3,d2
	neg.w	d2
	bra	abc

pcm_to_adpcm:			*ＰＣＭデータをＡＤＰＣＭデータへ変換する
	* < a0=adpcm data buffer
	* < a1=pcm data buffer
	* < d0.l=pcm data count
	* X d0-d5/a1,a2,a3,a5,a6

	lea	scaleval(pc),a5
	lea	levelchg(pc),a6

	moveq.l	#0,d6		*scalelevel=0
	moveq.l	#0,d7
	moveq.l	#0,d4
*	add.l	d0,d0
	lea	last_val(pc),a3
	move.w	d4,(a3)		*clr.w	(a3)
pta_lp:
	move.w	(a1)+,d3	*d3=pcm data

calc_adpcm_val:
	* < d3.w=pcm value
	* < d7.w=scale level
	* > d1.b=adpcm value
	* > d7.w=next scale level
	* X
	sub.w	(a3),d3		*d3=diff
	bmi	case_diff_minus
	moveq.l	#0,d1
	bra	calc_diff
case_diff_minus:
	neg.w	d3
	moveq.l	#8,d1		*d1:become data
calc_diff:
	add.b	d7,d7
	move.w	(a5,d7.w),d2	*=d
	lsr.b	d7

	cmp.w	d3,d2
	bge	_or2
	sub.w	d2,d3
	ori.b	#4,d1
_or2:
	lsr.w	d2
	cmp.w	d3,d2
	bge	_or1
	sub.w	d2,d3
	ori.b	#2,d1
_or1:
	lsr.w	d2
	cmp.w	d3,d2
	bge	chg_scalelvl
	ori.b	#1,d1

chg_scalelvl:
	add.b	d1,d1
	add.w	(a6,d1.w),d7		*scalelevl+=levelchg(adpcm value)
	bmi	rst_sclv_
	cmpi.w	#48,d7
	bls	mk_olv
	moveq.l	#48,d7
	bra	mk_olv
rst_sclv_:
	moveq.l	#0,d7
mk_olv:
	exg	d7,d6
	bsr	calc_pcm_val_
	exg	d6,d7
*-----------------------------------------------------------------------------
	not.b	d4
	bne	set_lower
				*case upper 4bits
	lsl.b	#3,d1
	or.b	d1,d5
	move.b	d5,(a0)+
	bra	check_cnt
set_lower:
	lsr.b	d1
	move.b	d1,d5
check_cnt:
	subq.l	#1,d0
	bne	pta_lp
	rts

bit_pat_set:			*ビットパターンを作る
	moveq.l	#4,d1		*offset(bsrの時点でspが-4になっているから)
	moveq.l	#0,d2
	moveq.l	#0,d3
	moveq.l	#0,d4
	moveq.l	#10-1,d5	*basicのパラメータマックスが10個のため
bps_lp:
	move.l	param1(sp,d1.w),d0	*get trk number
	subq.l	#1,d0
	bcs	@f
	cmpi.l	#tr_max-1,d0
	bhi	next_bps
	cmpi.b	#31,d0
	bls	bset_d2
	cmpi.b	#63,d0
	bhi	bset_d4
	bset.l	d0,d3
	bra	next_bps
bset_d4:
	bset.l	d0,d4
	bra	next_bps
bset_d2:
	bset.l	d0,d2
next_bps:
	addq.w	#4,d1		*次へ
	dbra	d5,bps_lp
@@:
	rts

self_output:
	* < a2=file name
	* < d3=size
	* < a5=data address
	move.w	#%0_000_01,-(sp)	*自分自身へ出力しちゃう
	pea	(a2)
	DOS	_OPEN
*	addq.w	#6,sp
	move.l	d0,d5		*d5.w=file handle

	move.l	d3,-(sp)	*size
	pea	(a5)		*data address
	move.w	d5,-(sp)
	DOS	_WRITE
*	lea	10(sp),sp

	move.w	d5,-(sp)
	DOS	_CLOSE
*	addq.w	#2,sp
	lea	18(sp),sp
	rts

fopen:				*ファイルのオープン(環境変数参照モード)
	* < a2=file name
	* > d5=file handle (error:d5<0)
	* - all 
	movem.l	d0-d2/a0-a3,-(sp)

	clr.w	-(sp)
	pea     (a2)
	DOS	_OPEN
	addq.w	#6,sp
	move.l	d0,d5		*d5.w=file handle
	bpl	exit_fopen	*no problem

	movea.l	a2,a0		*a0=file name only(non pathed)

	pea	getname(pc)
	bsr	search_env
	addq.w	#4,sp
	tst.l	d0
	bmi	exit_fopen
	move.l	d0,a1

	moveq.l	#0,d1
fopen_lp01:
	lea	open_fn(pc),a2
	tst.l	d1
	bne	cont_standard_dir
	move.b	(a1)+,d0
	cmpi.b	#'/',d0
	beq	standard_dir
	cmpi.b	#'-',d0
	beq	standard_dir
	subq.w	#1,a1
	moveq.l	#0,d1
fopen_lp02:
	move.b	(a1)+,d0
	beq	do_fopen
	cmpi.b	#';',d0
	beq	do_fopen
	move.b	d0,(a2)+
	bra	fopen_lp02
cont_standard_dir:
	move.l	d1,a1
	bra	subptlp
standard_dir:
	move.b	(a1)+,getname+6-work(a6)
	pea	getname(pc)
	bsr	search_env
	addq.w	#4,sp
	tst.l	d0
	bmi	exit_fopen
	move.l	d0,a3
	clr.b	getname+6-work(a6)
	move.l	a1,d1
subptlp:
	move.b	(a1)+,d0
	beq	@f
	cmpi.b	#';',d0
	beq	@f
	move.b	d0,(a2)+
	bra	subptlp
@@:
	moveq.l	#'\',d0		*余計な'\'記号の削除
subptlp0:
	cmp.b	-1(a2),d0
	bne	subpt0
	move.b	-2(a2),d2
	bpl	@f
	cmpi.b	#$a0,d2		*漢字か
	bcs	subpt0
	cmpi.b	#$df,d2
	bhi	subpt0
@@:
	subq.w	#1,a2
	bra	subptlp0
subpt0:
@@:
	cmp.b	(a3),d0
	bne	@f
	addq.w	#1,a3
	bra	@b
@@:
	move.b	d0,(a2)+
@@:
	move.b	(a3)+,d0
	beq	@f
	cmpi.b	#';',d0
	beq	do_fopen
	move.b	d0,(a2)+	*DIR名の転送
	bra	@b
@@:
	moveq.l	#0,d1
do_fopen:
	pea	(a0)
	moveq.l	#'\',d0		*余計な'\'記号の削除
subptlp1:
	cmp.b	-1(a2),d0
	bne	subpt1
	move.b	-2(a2),d2
	bpl	@f
	cmpi.b	#$a0,d2		*漢字か
	bcs	subpt1
	cmpi.b	#$df,d2
	bhi	subpt1
@@:
	subq.w	#1,a2
	bra	subptlp1
subpt1:
@@:
	cmp.b	(a0),d0
	bne	@f
	addq.w	#1,a0
	bra	@b
@@:
	move.b	d0,(a2)+
do_fopenlp:
	move.b	(a0)+,(a2)+
	bne	do_fopenlp
	clr.b	(a2)
	move.l	(sp)+,a0

	clr.w	-(sp)
	pea     open_fn(pc)
	DOS	_OPEN
	addq.w	#6,sp
	move.l	d0,d5		*d5.w=file handle
	bpl	exit_fopen	*no problem
	tst.l	d1
	bne	fopen_lp01
	tst.b	-1(a1)		*まだ環境変数が残ってるか
	bne	fopen_lp01
exit_fopen:
	movem.l	(sp)+,d0-d2/a0-a3
	rts

search_env:
	movem.l	a0-a1,-(sp)
	DOS	_GETPDB
	move.l	d0,a1		*環境変数文字列群
	move.l	(a1),a1
	addq.w	#4,a1
	tst.b	(a1)
	beq	exit_sen	*環境変数なし
sen_lp00:
	move.l	2*4+4(sp),a0	*サーチ環境変数名
sen_lp01:
	move.b	(a1)+,d0
	cmpi.b	#'=',d0
	bne	@f
	moveq.l	#0,d0
@@:
	cmp.b	(a0)+,d0
	bne	skip_next_en
	tst.b	d0
	bne	sen_lp01
	move.l	a1,d0
	bra	exit_sen
skip_next_en:
	tst.b	(a1)+
	bne	skip_next_en
	tst.b	(a1)
	bne	sen_lp00
	moveq.l	#-1,d0		*error
exit_sen:
	movem.l	(sp)+,a0-a1
	rts

read:
	* < d5.l=file handle
	* > a5=data address
	* > d3.l=size
	* X d0
	move.w	#2,-(sp)	*ファィルの長さを調べる
	clr.l	-(sp)
	move.w	d5,-(sp)
	DOS	_SEEK
 	addq.w	#8,sp		*d0.l=file length
	move.l	d0,d3		*d3=length
	bne	@f
	addq.w	#4,sp
	bra	read_err	*file size=0
@@:
	move.l	d0,-(sp)
	move.w	#2,-(sp)
	DOS	_MALLOC2
	addq.w	#6,sp
	tst.l	d0
	bpl	@f
	addq.w	#4,sp
	bra	read_err	*OUT OF MEMORY
@@:
	move.l	d0,a5

	clr.w	-(sp)		*ファイルポインタを元に戻す
	clr.l	-(sp)
	move.w	d5,-(sp)
	DOS	_SEEK
*	addq.w	#8,sp

	move.l	d3,-(sp)	*push size
	pea	(a5)		*push addr
	move.w	d5,-(sp)	*file handle
	DOS	_READ
*	lea	10(sp),sp
	lea	18(sp),sp
	tst.l	d0
	bpl	@f
	addq.w	#4,sp
	bra	read_err	*読み込み失敗
@@:
	move.w	d5,-(sp)	*close
	DOS	_CLOSE
	addq.l	#2,sp
	rts
read_err:
	moveq.l	#-1,d0
	rts

srch_num:			*数字までスキップ
	* X d0
	move.b	(a0)+,d0
	beq	sn_err
	cmpi.b	#'$',d0
	beq	its_num
	cmpi.b	#'%',d0
	beq	its_num
	cmpi.b	#'0',d0
	bcs	srch_num
	cmpi.b	#'9',d0
	bhi	srch_num
its_num:
	subq.w	#1,a0
	moveq.l	#0,d0
	rts
sn_err:
	subq.w	#1,a0
	moveq.l	#-1,d0
	rts

skip_plus2:
	cmpi.b	#'+',(a0)+
	beq	skip_plus2
	subq.w	#1,a0
	rts

skip_spc2:			*スペースをスキップする
	* > a0=next
	* - all
	move.l	d0,-(sp)
@@:
	move.b	(a0)+,d0
	cmpi.b	#' ',d0
	beq	@b
	cmpi.b	#09,d0		*skip tab
	beq	@b
	subq.w	#1,a0
	move.l	(sp)+,d0
	rts

mk_capital:			*小文字→大文字(英字以外の場合はそのままthrough out)
	* < d0.b=letter chr
	cmpi.b	#'a',d0
	bcs	exit_mkcptl
	cmpi.b	#'z',d0
	bhi	exit_mkcptl
	andi.w	#$df,d0		*わざと.w
exit_mkcptl:
	rts

get_num:			*数字文字列を数値へ
	* < (a0)=number strings
	* > d0.l=value
	* > (a0)=next_data
	* - all
	movem.l	d1-d4,-(sp)
	st.b	d4		*set hajimete
	cmpi.b	#'-',(a0)
	seq	d2   		*'-'ならマーク
	bne	_get_num0
	addq.w	#1,a0
_get_num0:
	bsr	skip_plus2
	bsr	skip_spc2

	cmpi.b	#'$',(a0)		*16進数??
	beq	get_hexnum
	cmpi.b	#'%',(a0)
	beq	get_binnum

	moveq.l	#0,d0
	moveq.l	#0,d1
_num_lp01:
	move.b	(a0)+,d1
	cmpi.b	#'_',d1
	beq	_num_lp01
	sub.b	#$30,d1
	bmi	_num_exit 
	cmp.b	#9,d1
	bhi	_num_exit 

	clr.b	d4		*clr.b	hajimete

	add.l	d0,d0
	move.l	d0,d3
	add.l	d0,d0
	add.l	d0,d0
	add.l	d3,d0		*d0=d0*10
	add.l	d1,d0		*d0=d0+d1
	bra	_num_lp01
_num_exit:
	subq.w	#1,a0		*つじつま合わせ
	tst.b	d2
	beq	_num_exit_
	neg.l	d0
_num_exit_:
	tst.b	d4		*数字はなかったか
	bmi	get_num_error
	moveq.l	#0,d2		*no error
	movem.l	(sp)+,d1-d4
	rts
get_num_error:			*数字がないじゃないか
	moveq.l	#-1,d2		*error
	movem.l	(sp)+,d1-d4
	rts

get_hexnum:			*16進数をゲット
	moveq.l	#0,d0
	moveq.l	#0,d1
	addq.w	#1,a0		*skip '$'
	bsr	skip_spc2
__num_lp01:
	move.b	(a0)+,d1
	cmpi.b	#'_',d1
	beq	__num_lp01
	cmpi.b	#'a',d1
	bcs	exit_mkcptl_hx
	cmpi.b	#'z',d1
	bhi	exit_mkcptl_hx
	andi.b	#$df,d1
exit_mkcptl_hx:
	sub.b	#$30,d1
	bmi	_num_exit 
	cmp.b	#9,d1
	bls	calc_hex
	cmpi.b	#17,d1
	bcs	_num_exit
	cmpi.b	#22,d1
	bhi	_num_exit
	subq.b	#7,d1
calc_hex:
	clr.b	d4
	lsl.l	#4,d0
	or.b	d1,d0
	bra	__num_lp01
get_binnum:			*2進数をゲット
	moveq.l	#0,d0
	moveq.l	#0,d1
	addq.w	#1,a0		*skip '%'
	bsr	skip_spc2
b__num_lp01:
	move.b	(a0)+,d1
	cmpi.b	#'_',d1
	beq	b__num_lp01
	cmpi.b	#'0',d1
	beq	exit_mkcptl_bn
	cmpi.b	#'1',d1
	bne	_num_exit
exit_mkcptl_bn:
	sub.b	#$30,d1
	clr.b	d4		*数字をゲットしたことをマーク
	lsl.l	#1,d0
	or.b	d1,d0
	bra	b__num_lp01

work:
getname:	.dc.b	'zmusic',0,0
OPM:		.dc.b	'OPM',0
CRLF:		.dc.b	13,10,0
	.even
scaleval:
	dc.w	 16,17,19,21,23,25,28
	dc.w	 31,34,37,41,45,50,55
	dc.w	 60,66,73,80,88,97,107
	dc.w	 118,130,143,157,173,190,209
	dc.w	 230,253,279,307,337,371,408
	dc.w	 449,494,544,598,658,724,796
	dc.w	 876,963,1060,1166,1282,1411,1552
levelchg:
	dc.w	-1,-1,-1,-1,2,4,6,8
	dc.w	-1,-1,-1,-1,2,4,6,8

zmd_play_wk:	dc.l	0

	.even
md_buf:		ds.b	32
	.even
env_work:	ds.b	256
open_fn:	ds.b	91
filename:	ds.b	91
	.even
last_val:	ds.w	1
rec_address:	ds.l	1	*!!!
rec_size:	ds.l	1	*!!!
reg_buf:	ds.l	16	*レジスタバッファ
	dc.b	'ZMUSIC.L V2.01 (C)ZENJI SOFT'
