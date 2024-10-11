*		ＺＭＵＳＩＣ．Ｘ用 外部関数
*
*		ＭＵＳＩＣＺ．ＦＮＣ v2.02
*
*	参考文献	MUSIC.FNC	SHARP
*			MUSIC2.FNC	SAN M.S.
*
*
	.include	doscall.mac
	.include	iocscall.mac
	.include	fdef.mac

ch_max:		equ	25	*fm8+adpcm1+midi16=25
pl_max:		equ	32	*一度に演奏可能なトラック数
tr_max:		equ	80	*確保出来るトラック数の最大値
wv_max:		equ	28	*波形メモリの登録最大数

val:		equ	6

Z_MUSIC	macro	func		*ドライバへのファンクションコール
	moveq.l	func,d1
	trap	#3
	endm

information_table:
	dc.l	init
	dc.l	run
	dc.l	end
	dc.l	system
	dc.l	break
	dc.l	ctrl_d
	dc.l	yobi
	dc.l	yobi
	dc.l	token_table
	dc.l	parameter
	dc.l	exec_address
	dcb.b	20,0

token_table:
	dc.b	'm_assign',0		*1
	dc.b	'm_alloc',0		*2
	dc.b	'm_vget',0		*3
	dc.b	'm_vset',0		*4
	dc.b	'm_trk',0		*5
	dc.b	'm_play',0		*6
	dc.b	'm_stop',0		*7
	dc.b	'm_cont',0		*8
	dc.b	'm_init',0		*9
	dc.b	'm_stat',0		*10
	dc.b	'm_free',0		*11
	dc.b	'm_tempo',0		*12
	dc.b	'm_atoi',0		*13
	dc.b	'm_assign2',0		*14
	dc.b	'm_ch',0		*15
	dc.b	'm_pcmset',0		*16
	dc.b	'm_pcmplay',0		*17
	dc.b	'm_rec',0		*18
	dc.b	'm_save',0		*19
	dc.b	'm_rstop',0		*20
	dc.b	'm_trans',0		*21
	dc.b	'm_fmvset',0		*22
	dc.b	'm_out',0		*23
	dc.b	'm_dirout',0		*24
	dc.b	'm_exc',0		*25
	dc.b	'm_roland',0		*26
	dc.b	'm_total',0		*27
	dc.b	'm_fadeout',0		*28
	dc.b	'm_pcmcnf',0		*29
	dc.b	'm_switch',0		*30
	dc.b	'sc55_v_reserve',0	*31
	dc.b	'sc55_reverb',0		*32
	dc.b	'sc55_chorus',0		*33
	dc.b	'sc55_part_setup',0	*34
	dc.b	'sc55_drum_setup',0	*35
	dc.b	'sc55_print',0		*36
	dc.b	'sc55_display',0	*37
	dc.b	'm_adpcm_block',0	*38
	dc.b	'mt32_p_reserve',0	*39
	dc.b	'mt32_reverb',0		*40
	dc.b	'mt32_part_setup',0	*41
	dc.b	'mt32_drum_setup',0	*42
	dc.b	'mt32_common',0		*43
	dc.b	'mt32_patch',0		*44
	dc.b	'mt32_partial',0	*45
	dc.b	'mt32_print',0		*46
	dc.b	'm_print',0		*47
	dc.b	'u220_print',0		*48
	dc.b	'u220_setup',0		*49
	dc.b	'u220_common',0		*50
	dc.b	'u220_drum_setup',0	*51
	dc.b	'u220_part_setup',0	*52
	dc.b	'u220_timbre',0		*53
	dc.b	'u220_drum_inst',0	*54
	dc.b	'm1_midi_ch',0		*55
	dc.b	'm1_part_setup',0	*56
	dc.b	'm1_effect_setup',0	*57
	dc.b	'm1_print',0		*58
	dc.b	'send_to_m1',0		*59
	dc.b	'zmd_play',0		*60
	dc.b	'm_debug',0		*61
	dc.b	'm_count',0		*62
	dc.b	'fm_master',0		*63
	dc.b	'm_mute',0		*64
	dc.b	'm_solo',0		*65
	dc.b	'm_wave_form',0		*66
	dc.b	'sc55_init',0		*67
	dc.b	'mt32_init',0		*68
	dc.b	'adpcm_to_pcm',0	*69
	dc.b	'pcm_to_adpcm',0	*70
	dc.b	'exec_zms',0		*71
	dc.b	'm_inp',0		*72
	dc.b	'zm_ver',0		*73
	dc.b	'm_trk2',0		*74
	dc.b	'zm_work',0		*75
	dc.b	0			*end code
	.even
parameter:
	dc.l	m_assign_p		*1
	dc.l	m_alloc_p		*2
	dc.l	m_vget_p		*3
	dc.l	m_vset_p		*4
	dc.l	m_trk_p			*5
	dc.l	m_play_p		*6
	dc.l	m_stop_p		*7
	dc.l	m_cont_p		*8
	dc.l	m_init_p		*9
	dc.l	m_stat_p		*10
	dc.l	m_free_p		*11
	dc.l	m_tempo_p		*12
	dc.l	m_atoi_p		*13
	dc.l	m_assign2_p		*14
	dc.l	m_ch_p			*15
	dc.l	m_pcmset_p		*16
	dc.l	m_pcmplay_p		*17
	dc.l	m_rec_p			*18
	dc.l	m_save_p		*19
	dc.l	m_rstop_p		*20
	dc.l	m_trans_p		*21
	dc.l	m_fmvset_p		*22
	dc.l	m_out_p			*23
	dc.l	m_dirout_p		*24
	dc.l	m_exc_p			*25
	dc.l	m_roland_p		*26
	dc.l	m_total_p		*27
	dc.l	m_fadeout_p		*28
	dc.l	m_pcmcnf_p		*29
	dc.l	m_switch_p		*30
	dc.l	sc55_vr_p		*31
	dc.l	sc55_rvb_p		*32
	dc.l	sc55_cho_p		*33
	dc.l	sc55_pst_p		*34
	dc.l	sc55_dpr_p		*35
	dc.l	sc55_prt_p		*36
	dc.l	sc55_dsp_p		*37
	dc.l	adpcm_blk_p		*38
	dc.l	mt32_pr_p		*39
	dc.l	mt32_rvb_p		*40
	dc.l	mt32_pst_p		*41
	dc.l	mt32_dst_p		*42
	dc.l	mt32_cmn_p		*43
	dc.l	mt32_ptch_p		*44
	dc.l	mt32_prtl_p		*45
	dc.l	mt32_prt_p		*46
	dc.l	m_prt_p			*47
	dc.l	u220_prt_p		*48
	dc.l	u220_setup_p		*49
	dc.l	u220_cmn_p		*50
	dc.l	u220_dst_p		*51
	dc.l	u220_pst_p		*52
	dc.l	u220_tmb_p		*53
	dc.l	u220_dis_p		*54
	dc.l	m1_mdch_p		*55
	dc.l	m1_ptst_p		*56
	dc.l	m1_efct_p		*57
	dc.l	m1_prt_p		*58
	dc.l	send_p			*59
	dc.l	zmd_play_p		*60
	dc.l	j_f_p			*61
	dc.l	m_c_p			*62
	dc.l	fm_mstr_p		*63
	dc.l	m_mute_p		*64
	dc.l	m_solo_p		*65
	dc.l	m_wf_p			*66
	dc.l	sc55_init_p		*67
	dc.l	mt32_init_p		*68
	dc.l	adpcm_to_pcm_p		*69
	dc.l	pcm_to_adpcm_p		*70
	dc.l	exec_zms_p		*71
	dc.l	m_inp_p			*72
	dc.l	zm_ver_p		*73
	dc.l	m_trk2_p		*74
	dc.l	zm_work_p		*75

m_assign_p:	dc.w	char_val,char_val,int_ret	*1
m_alloc_p:	dc.w	char_val,int_val,int_ret	*2
m_vget_p:						*3
m_vset_p:						*4
		dc.w	char_val,ary2_c,int_ret
m_trk_p:	dc.w	char_val,str_val,int_ret	*5
m_play_p:						*6
m_stop_p:						*7
m_cont_p:						*8
m_mute_p:						*65
m_solo_p:						*66
		dcb.w	10,char_omt
		dc.w	int_ret
m_init_p:	dc.w	int_ret				*9
m_stat_p:	dc.w	char_omt,int_ret		*10
m_free_p:	dc.w	char_val,int_ret		*11
m_tempo_p:	dc.w	int_omt,int_ret			*12
m_atoi_p:	dc.w	char_val,int_ret		*13
m_assign2_p:	dc.w	str_val,char_val,int_ret	*14
m_ch_p:		dc.w	str_val,int_ret			*15
		*	note	filename pitch
m_pcmset_p:	dc.w	int_val,str_val,int_omt		*16
		*	volume	mix     delay
		dc.w	int_omt,int_omt,int_omt
		*	cut offset,size
		dc.w	int_omt
		*	reverse
		dc.w	char_omt
		*	fade point,level
		dc.w	int_omt,int_ret
m_pcmplay_p:	dc.w	int_val,char_val,char_val	*17
		dc.w	int_ret
m_rec_p:	dc.w	int_ret				*18
m_save_p:	dc.w	str_val,int_ret			*19
m_rstop_p:	dc.w	int_ret				*20
m_trans_p:	dc.w	str_val,int_ret			*21
m_fmvset_p:	dc.w	char_val,ary2_c,int_ret		*22
m_out_p:	dc.w	char_val			*23
		dcb.w	9,char_omt
		dc.w	int_ret
m_dirout_p:	dc.w	ary1_c,int_omt,int_ret		*24
m_exc_p:	dc.w	ary1_c,int_omt,int_ret		*25
m_roland_p:	dc.w	char_val,char_val		*26
		dc.w	ary1_c,int_omt,int_ret
m_total_p:	dc.w	int_ret				*27
m_fadeout_p:	dc.w	int_omt,int_ret			*28
m_pcmcnf_p:	dc.w	str_val,int_ret			*29
m_switch_p:	dc.w	char_omt,str_omt,int_ret	*30
sc55_vr_p:	dc.w	ary1_c,char_omt,int_ret		*31
sc55_rvb_p:	dc.w	ary1_c,char_omt,int_ret		*32
sc55_cho_p:	dc.w	ary1_c,char_omt,int_ret		*33
sc55_pst_p:	dc.w	char_val,ary1_c			*34
		dc.w	char_omt,int_ret
sc55_dpr_p:	dc.w	char_val,char_val,ary1_c	*35
		dc.w	char_omt,int_ret
sc55_prt_p:	dc.w	str_val,char_omt,int_ret	*36
sc55_dsp_p:	dc.w	ary1_i,char_omt,int_ret		*37
adpcm_blk_p:	dc.w	str_val,int_ret			*38
mt32_pr_p:	dc.w	ary1_c,char_omt,int_ret		*39
mt32_rvb_p:	dc.w	ary1_c,char_omt,int_ret		*40
mt32_pst_p:	dc.w	ary1_c,char_omt,int_ret		*41
mt32_dst_p:	dc.w	char_val,ary1_c,char_omt	*42
		dc.w	int_ret
mt32_cmn_p:	dc.w	char_val,str_val,ary1_c		*43
		dc.w	char_omt,int_ret
mt32_ptch_p:	dc.w	char_val,ary1_c,char_omt	*44
		dc.w	int_ret
mt32_prtl_p:	dc.w	char_val,char_val,ary1_c	*45
		dc.w	char_omt,int_ret
mt32_prt_p:	dc.w	str_val,char_omt,int_ret	*46
m_prt_p:	dc.w	str_val,int_ret			*47
u220_prt_p:	dc.w	str_val,char_omt,int_ret	*48
u220_setup_p:	dc.w	ary1_c,char_omt,int_ret		*49
u220_cmn_p:	dc.w	ary1_c,char_omt,int_ret		*50
u220_dst_p:	dc.w	ary1_c,char_omt,int_ret		*51
u220_pst_p:	dc.w	char_val,ary1_c,char_omt	*52
		dc.w	int_ret
u220_tmb_p:	dc.w	char_val,str_val,ary1_c		*53
		dc.w	char_omt,int_ret
u220_dis_p:	dc.w	char_val,ary1_c			*54
		dc.w	char_omt,int_ret
m1_mdch_p:	dc.w	ary1_c,int_ret			*55
m1_ptst_p:	dc.w	ary1_c,int_ret			*56
m1_efct_p:	dc.w	ary1_c,int_ret			*57
m1_prt_p:	dc.w	str_val,int_ret			*58
send_p:		dc.w	char_omt,int_ret		*59
zmd_play_p:	dc.w	str_val,int_ret			*60
j_f_p:		dc.w	char_val,int_ret		*61
m_c_p:		dc.w	char_val,int_ret		*62
fm_mstr_p	dc.w	char_val,int_ret		*63
m_wf_p:		dc.w	char_val,char_val,int_omt	*66
		dc.w	ary1_i,int_ret
sc55_init_p	dc.w	char_omt,int_ret		*67
mt32_init_p	dc.w	char_omt,int_ret		*68
adpcm_to_pcm_p:	dc.w	ary1_c,int_omt,ary1_i,int_ret	*69
pcm_to_adpcm_p:	dc.w	ary1_i,int_omt,ary1_c,int_ret	*70
exec_zms_p:	dc.w	str_val,int_ret			*71
m_inp_p:	dc.w	char_omt,int_ret		*72
zm_ver_p:	dc.w	char_omt,int_ret		*73
m_trk2_p:	dc.w	str_val,char_val		*74
		dcb.w	7,char_omt
		dc.w	int_ret
zm_work_p:	dc.w	char_val,int_val,int_ret	*75

exec_address:
	dc.l	m_assign,m_alloc,m_vget,m_vset		*1-4
	dc.l	m_trk,m_play,m_stop,m_cont		*5-8
	dc.l	m_init,m_stat,m_free,m_tempo		*9-12
	dc.l	m_atoi,m_assign2,m_ch,m_pcmset		*13-16
	dc.l	m_pcmplay,m_rec,m_save,m_rstop		*17-20
	dc.l	m_trans,m_fmvset,m_out,m_dirout		*21-24
	dc.l	m_exc,m_roland,m_total,m_fadeout	*25-28
	dc.l	m_pcmcnf,m_switch,sc55_vr,sc55_rvb	*29-32
	dc.l	sc55_cho,sc55_pst,sc55_dst,sc55_prt	*33-36
	dc.l	sc55_dsp,adpcm_block,mt32_pr,mt32_rvb	*37-40
	dc.l	mt32_pst,mt32_dst,mt32_cmn,mt32_ptch	*41-44
	dc.l	mt32_prtl,mt32_prt,m_print,u220_prt	*45-48
	dc.l	u220_setup,u220_cmn,u220_dst,u220_pst	*49-52
	dc.l	u220_tmb,u220_dis,m1_mdch,m1_ptst	*53-56
	dc.l	m1_effect,m1_prt,send_m1,zmd_play	*57-60
	dc.l	jump_flg,set_mclk,fm_master		*61-63
	dc.l	m_mute,m_solo,m_wave_form		*64-66
	dc.l	sc55_init,mt32_init			*67-68
	dc.l	adpcm_to_pcm,pcm_to_adpcm		*69-70
	dc.l	exec_zms,m_inp,zm_ver,m_trk2,zm_work	*71-75

init:
	lea	work(pc),a1
	clr.b	out_flg-work(a1)	*ファイル書き出しスイッチクリア
	move.w	#-1,sv_fh-work(a1)	*ファイルハンドル初期化
	DOS	_ALLCLOSE		*念のため

	DOS	_V2_GETPDB		*環境取得
	move.l	d0,a0
	move.l	(a0),env_bak-work(a1)

	tst.l	str_buffer-work(a1)
	bne	@f
	move.l	#$2000,-(sp)
	DOS	_MALLOC
	addq.w	#4,sp
	move.l	d0,str_buffer-work(a1)	*ストリングバッファ確保
	add.l	#$2000,d0
	move.l	d0,stbf_end-work(a1)
@@:
	bsr	chk_drv			*ドライバ常駐チェック
	bpl	@f
	move.b	#1,err_flg-work(a1)	*未登録
	rts
@@:
	clr.b	err_flg-work(a1)
	lea	OPM(PC),a2
	moveq.l	#1,d3
	lea	i_data(pc),a5
	bra	self_output

chk_drv:			*デバイス名のcheck
	* > eq=no error
	* > mi=error
	clr.l	-(sp)
	DOS	_SUPER
	addq.w	#4,sp
	move.l	d0,-(sp)

	movea.l	$88.w,a0	*PCM8常駐チェック
	move.l	-8(a0),d0
	move.b	#$20,d0
	cmpi.l	#'PCM ',d0
	seq.b	pcm8_flg

	move.l	$8c.w,a0
	subq.w	#8,a0
	cmpi.l	#'ZmuS',(a0)+
	bne	chk_drv_err
	cmpi.w	#'iC',(a0)+
	bne	chk_drv_err
	move.w	(a0)+,zm_ver_buf

	DOS	_SUPER		*見付けた
	addq.w	#4,sp
	moveq.l	#0,d0
	rts
chk_drv_err:
	DOS	_SUPER		*見付けた
	addq.w	#4,sp
	moveq.l	#-1,d0
	rts

run:
	lea	work(pc),a1
	clr.l	rec_address-work(a1)
	clr.l	rec_size-work(a1)
	st.b	running-work(a1)
	move.w	sv_fh(pc),d0	*一応
	bmi	@f
	move.w	d0,-(sp)
	DOS	_CLOSE
	addq.w	#2,sp
@@:
	tst.b	out_flg-work(a1)
	beq	@f
	move.w	#32,-(sp)
	pea	gene_fn(pc)
	DOS	_CREATE
	addq.w	#6,sp
	move.w	d0,sv_fh-work(a1)
@@:
	rts

ctrl_d:
	lea	work(pc),a1
	tst.b	err_flg-work(a1)
	bne	exit_ctrl_d
	moveq.l	#0,d2		*all
	moveq.l	#0,d3		*all
	moveq.l	#0,d4		*all
	Z_MUSIC	#$0a
exit_ctrl_d:
	rts

end:
	lea	work(pc),a1
	clr.b	running-work(a1)
	move.w	sv_fh(pc),d0
	bmi	yobi		*error
	move.w	d0,-(sp)
	DOS	_CLOSE
	addq.w	#2,sp
	move.w	#-1,sv_fh-work(a1)	*clear fh
	rts

break:
system:
yobi:
	rts

m_alloc:			*トラックバッファの確保
	bsr	error?		*return:ジェネレートモードならa2=str buffer
	move.b	out_flg(pc),d0
	beq	@f
	move.w	#'(m',(a2)+
	move.l	12(sp),d0
	bsr	wrt_num
	move.b	#',',(a2)+
	move.l	22(sp),d0
	cmpi.l	#65535,d0
	bhi	m_err4
	bsr	wrt_num
	move.b	#')',(a2)+
	bra	wrt_dsk
@@:
	move.l	12(sp),d2	*get trk number
	move.l	22(sp),d1
	cmpi.l	#65535,d1
	bhi	m_err4		*illegal size
*	subq.l	#1,d1
	swap	d2
	move.w	d1,d2		*hi=trk,low=size
	Z_MUSIC	#$01
	tst.l	d0
	bne	error
	bra	ok

m_assign:			*チャンネルアサイン
	bsr	error?
	move.b	out_flg(pc),d0
	beq	@f
	move.w	#'(a',(a2)+
	move.l	12(sp),d0
	bsr	wrt_num
	move.b	#',',(a2)+
	move.l	22(sp),d0
	bsr	wrt_num
	move.b	#')',(a2)+
	bra	wrt_dsk
@@:
	move.l	12(sp),d2	*get ch
	move.l	22(sp),d1	*get tr
	swap	d2
	move.w	d1,d2
	Z_MUSIC	#$02
	tst.l	d0
	bne	error
	bra	ok

m_vget:
	bsr	error?
	move.b	out_flg(pc),d0
	bne	m_err10		*can't compile
	move.l	12(sp),d2	*sound number
	movea.l	22(sp),a1	*配列
	cmpi.w	#4,8(a1)
	bne	m_err7		*配列の次元が違う
	cmpi.w	#10,10(a1)
	bne	m_err7
	lea	$10(a1),a1
	Z_MUSIC	#$03
	tst.l	d0
	bne	error
	bra	ok

m_vset:
	bsr	error?
	move.b	out_flg(pc),d0
	beq	@f
	move.w	#'(v',(a2)+
	move.l	12(sp),d0
	bsr	wrt_num
	move.b	#',',(a2)+
	move.b	#'0',(a2)+
	bsr	do_crlf
	move.l	22(sp),a1
	cmpi.w	#4,8(a1)
	bne	m_err7		*配列の次元が違う
	cmpi.w	#10,10(a1)
	bne	m_err7
	lea	$10(a1),a1
	lea	rem1(pc),a0
	bsr	wrt_str
	moveq.l	#4,d1
mvslp00:
	cmpi.b	#3,d1
	bne	non_rem2
	lea	rem2(pc),a0
	bsr	wrt_str
non_rem2:
	move.b	#9,(a2)+
	moveq.l	#11-1,d2
mvslp01:
	moveq.l	#0,d0
	move.b	(a1)+,d0
	bsr	wrt_num2
	move.b	#',',(a2)+
	dbra	d2,mvslp01
	subq.w	#1,a2
	bsr	do_crlf
	dbra	d1,mvslp00
	subq.w	#2,a2
	move.b	#')',(a2)+
	bra	wrt_dsk
@@:
	move.l	12(sp),d2	*sound number
	movea.l	22(sp),a1	*配列
	cmpi.w	#4,8(a1)
	bne	m_err7		*配列の次元が違う
	cmpi.w	#10,10(a1)
	bne	m_err7
	lea	$10(a1),a1
	Z_MUSIC	#$04
	tst.l	d0
	bne	error
	bra	ok

m_tempo:
	bsr	error?
	move.b	out_flg(pc),d0
	beq	@f
	tst.w	6(sp)
	bmi	m_err10		*コンパイル出来ません
	move.w	#'(o',(a2)+
	move.l	12(sp),d0
	bsr	wrt_num
	move.b	#')',(a2)+
	bra	wrt_dsk
@@:
	tst.w	6(sp)
	bpl	@f
	moveq.l	#-1,d2		*request
	Z_MUSIC	#$05
	move.l	a0,d1
	beq	error
	bra	ok_ret		*戻り値有りのケース
@@:
	move.l	12(sp),d2
	Z_MUSIC	#$05
	tst.l	d0
	bne	error
	bra	ok

m_trk:
	bsr	error?
	move.b	out_flg(pc),d0
	beq	normal_m_trk
	move.w	#'(t',(a2)+
	move.l	12(sp),d0	*trk num
	bsr	wrt_num
	move.b	#')',(a2)+
	move.l	22(sp),a1	*mml str addres
@@:
	move.b	(a1)+,(a2)+
	bne	@b
	subq.w	#1,a2
	bra	wrt_dsk
normal_m_trk:
	move.l	12(sp),d2	*get trk
	movea.l	22(sp),a1	*get str address
	Z_MUSIC	#$06
	tst.l	d0
	bne	error
	bra	ok

m_free:
	bsr	error?
	move.b	out_flg(pc),d0
	bne	m_err10		*can't compile
	move.l	12(sp),d2	*get trk
	Z_MUSIC	#$07
	move.l	a0,d1
	beq	error
	bra	ok_ret

m_play:
	bsr	error?
	move.b	out_flg(pc),d0
	beq	@f
	move.w	#'(p',(a2)+
	bra	wrt_chs		*チャンネル番号を書く
@@:
	bsr	bit_pat_set
	Z_MUSIC	#$08
	tst.l	d0
	bne	error
	bra	ok

m_stat:
	bsr	error?
	lea	out_flg(pc),a6
	tst.b	(a6)
	bne	m_err10		*can't compile
	moveq.l	#0,d2
	tst.w	6(sp)
	bmi	go_mst
	move.l	12(sp),d0
	subq.b	#1,d0
	bmi	go_mst
	tst.b	pcm8_flg-out_flg(a6)
	bne	@f
	cmpi.l	#24,d0
	bhi	m_err6
@@:
	bset.l	d0,d2
go_mst:
	Z_MUSIC	#$09
	move.l	a0,d1
	beq	error
	bra	ok_ret

m_stop:
	bsr	error?
	move.b	out_flg(pc),d0
	beq	@f
	move.w	#'(s',(a2)+
	bra	wrt_chs		*チャンネル番号を書く
@@:
	bsr	bit_pat_set
	Z_MUSIC	#$0a
	tst.l	d0
	bne	error
	bra	ok

m_cont:
	bsr	error?
	move.b	out_flg(pc),d0
	beq	@f
	move.w	#'(c',(a2)+
	bra	wrt_chs		*チャンネル番号を書く
@@:
	bsr	bit_pat_set
	Z_MUSIC	#$0b
	tst.l	d0
	bne	error
	bra	ok

m_init:
	bsr	error?		*ドライバがアクティブかどうかチェック
	move.b	out_flg(pc),d0
	beq	@f
	move.w	#'(i',(a2)+
	move.b	#')',(a2)+
	bra	wrt_dsk
@@:
	Z_MUSIC	#$00
	tst.l	d0
	bne	error
	bra	ok

m_atoi:
	bsr	error?
	move.b	out_flg(pc),d0
	bne	m_err10		*can't compile
	move.l	12(sp),d2
	Z_MUSIC	#$0c
	move.l	a0,d1
	beq	error
	bra	ok_ret

m_assign2:
	bsr	error?
	move.b	out_flg(pc),d0
	beq	@f
	move.w	#'(a',(a2)+
	move.l	12(sp),a0
	bsr	wrt_str
	move.b	#',',(a2)+
	move.l	22(sp),d0
	bsr	wrt_num
	move.b	#')',(a2)+
	bra	wrt_dsk
@@:
	movea.l	12(sp),a0	*文字列アドレス
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
	bhi	m_err6		*illegal ch
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
	bne	m_err6		*illegal ch
	moveq.l	#9,d2
gsc_srch_num:
	bsr	srch_num
	bmi	m_err6
	bsr	get_num
	move.l	d0,d1
	cmpi.l	#1,d1
	blt	m_err6
	add.l	d2,d1
	subq.w	#1,d1
gsc:
	move.l	d1,d3		*save ch into d3
	Z_MUSIC	#$3a		*d0=real_ch_tbl
	movea.l	d0,a0
	move.l	d3,d1
	moveq.l	#32-1,d2
	moveq.l	#1,d3
gsc1_lp:
	cmp.b	(a0)+,d1
	beq	exit_gsc1
	addq.w	#1,d3
	dbra	d2,gsc1_lp
	bra	m_err6
exit_gsc1:
	move.w	d3,d2
	swap	d2
	tst.l	16(sp)
	bmi	m_err6
	move.l	22(sp),d1
	move.w	d1,d2
	Z_MUSIC	#$02
	tst.l	d0
	bne	error
	bra	ok

m_ch:				*ベースチャンネル切り換え
	bsr	error?
	move.b	out_flg(pc),d0
	beq	@f
	move.w	#'(b',(a2)+
	move.l	12(sp),a0
	moveq.l	#0,d2
	move.b	(a0)+,d0
	bsr	mk_capital
	cmpi.b	#'M',d0
	bne	mchmd
	addq.b	#1,d2		*midiならd2=1
mchmd:
	move.l	d2,d0		*fmならd2=0
	bsr	wrt_num
	move.b	#')',(a2)+
	bra	wrt_dsk
@@:
	move.l	12(sp),a0	*文字列アドレス
	move.b	(a0)+,d0
	andi.b	#$df,d0
	moveq.l	#0,d2
	cmpi.b	#'M',d0
	seq	d2		*Mならd2=$ff
	Z_MUSIC	#$15
	tst.l	d0
	bne	error
	bra	ok

m_pcmset:			*ＡＤＰＣＭデータリード
	bsr	error?
	move.b	out_flg(pc),d0
	beq	normal_pcmset	*ジェネレートモードじゃないときの普通の処理へ
	move.l	12(sp),d0	*set note number
	bsr	wrt_num
	move.b	#'=',(a2)+
	move.l	22(sp),a0
	bsr	wrt_str		*write filename
	tst.w	par3(sp)
	bmi	@f
	move.b	#',',(a2)+
	move.b	#'p',(a2)+	*pitch header
	move.l	par3+val(sp),d0
	bsr	wrt_num
@@:
	tst.w	par4(sp)
	bmi	@f
	move.b	#',',(a2)+
	move.b	#'v',(a2)+	*volume header
	move.l	par4+val(sp),d0
	bsr	wrt_num
@@:
	tst.w	par5(sp)
	bmi	@f
	move.b	#',',(a2)+
	move.b	#'m',(a2)+	*mix header
	move.l	par5+val(sp),d0
	bsr	wrt_num
@@:
	tst.w	par6(sp)
	bmi	@f
	move.b	#',',(a2)+
	move.b	#'d',(a2)+	*delay header
	move.l	par6+val(sp),d0
	bsr	wrt_num
@@:
	tst.w	par7(sp)
	bmi	@f
	move.b	#',',(a2)+
	move.b	#'c',(a2)+	*cut header
	move.l	par7+val(sp),d0
	move.l	d0,d1
	clr.w	d0
	swap	d0
	bsr	wrt_num		*write offset
	move.b	#',',(a2)+
	move.w	d1,d0
	bsr	wrt_num		*write cut size
@@:
	tst.w	par8(sp)
	bmi	@f
	move.l	par8+val(sp),d0
	beq	@f
	move.b	#',',(a2)+
	move.b	#'r',(a2)+	*reverse
@@:
	tst.w	par9(sp)
	bmi	@f
	move.b	#',',(a2)+
	move.b	#'f',(a2)+	*fade in/out header
	move.l	par9+val(sp),d0
	move.l	d0,d1
	clr.w	d0
	swap	d0
	tst.w	d1		*check mode
	bpl	wrt_fp
	move.b	#'-',(a2)+	*case:fade in
wrt_fp:
	bsr	wrt_num		*fade point
	move.b	#',',(a2)+
	moveq.l	#0,d0
	move.b	d1,d0
	bsr	wrt_num		*fade level
@@:
	bra	wrt_dsk
normal_pcmset:			*セーブモードじゃないとき
	move.l	12(sp),d2	*d2=to set note number
	move.l	22(sp),a1	*a1=file name
	moveq.l	#0,d3
	move.l	#$ffff,d4
	moveq.l	#0,d5
	moveq.l	#0,d6
	moveq.l	#0,d7
	moveq.l	#-1,d0
	and.w	par3(sp),d0
	and.w	par4(sp),d0
	and.w	par5(sp),d0
	and.w	par6(sp),d0
	and.w	par7(sp),d0
	bmi	go_zm10		*パラメータ省略
	move.l	#$000c_0064,d3	*non pitch and 100%
	tst.w	par3(sp)
	bmi	@f
	move.l	par3+val(sp),d0	*get pitch param.
	swap	d3
	move.w	d0,d3
	swap	d3
@@:
	tst.w	par4(sp)
	bmi	@f
	move.l	par4+val(sp),d0	*get vol. param.
	move.w	d0,d3
@@:
	tst.w	par5(sp)
	bmi	@f
	move.l	par5+val(sp),d0	*get mix param.
	move.w	d0,d4
@@:
	tst.w	par6(sp)
	bmi	@f
	move.l	par6+val(sp),d0	*get delay param.
	swap	d4
	move.w	d0,d4
	swap	d4
@@:
	tst.w	par7(sp)
	bmi	@f
	move.l	par7+val(sp),d5	*get cut param.
@@:
	tst.w	par8(sp)
	bmi	@f
	move.l	par8+val(sp),d6	*get reverse param.
@@:
	tst.w	par9(sp)
	bmi	@f
	move.l	par9+val(sp),d7	*get fade param.
@@:
go_zm10:
	Z_MUSIC	#$10
	tst.l	d0
	bne	error
	bra	ok

m_pcmplay:			*adpcm dataの演奏
	bsr	error?
	move.b	out_flg(pc),d0
	bne	m_err10		*can't compile
	move.l	12(sp),d2	*get note
	move.l	22(sp),d3	*get pan
	move.l	32(sp),d1	*get frq
	lsl.w	#8,d1
	or.w	d1,d3
	Z_MUSIC	#$14
	bra	ok

m_rec:				*MIDIデータ録音
	bsr	error?
	move.b	out_flg(pc),d0
	bne	m_err10		*can't compile
	Z_MUSIC	#$16
	tst.l	d0
	bne	error
	bra	ok

m_rstop:			*MIDIデータ録音停止
	bsr	error?
	lea	out_flg(pc),a6
	tst.b	(a6)
	bne	m_err10		*can't compile
	moveq.l	#0,d2		*ascii data作成モード
	Z_MUSIC	#$17
	move.l	a0,d1
	beq	error
	move.b	#$1a,(a0,d0.l)
	move.l	a0,rec_address-out_flg(a6)
	addq.l	#1,d0
	move.l	d0,rec_size-out_flg(a6)
	bra	ok

m_save:				*MIDIデータセーブ
	bsr	error?
	lea	out_flg(pc),a6
	tst.b	(a6)
	bne	m_err10		*can't compile
	move.l	rec_size(pc),d4		*size
	bne	@f
	moveq.l	#0,d2		*ascii data作成モード
	Z_MUSIC	#$17
	move.l	a0,d1
	beq	error
	move.b	#$1a,(a0,d0.l)
	move.l	a0,rec_address-out_flg(a6)
	addq.l	#1,d0
	move.l	d0,rec_size-out_flg(a6)
	move.l	d0,d4
	beq	m_err72		*file size=0
@@:
	move.l	12(sp),a0
	move.w	#32,-(sp)
	pea	(a0)		*move.l	12(sp),-(sp)	*filename
	DOS	_CREATE
	addq.w	#6,sp
	move.l	d0,d5		*d5.w=file handle
	bmi	m_err71

	move.l	d4,-(sp)		*data size
	move.l	rec_address(pc),-(sp)	*data addr
	move.w	d5,-(sp)
	DOS	_WRITE
	lea	10(sp),sp
	tst.l	d0
	bmi	m_err71

	move.w	d5,-(sp)
	DOS	_CLOSE
	addq.w	#2,sp
	bra	ok

m_trans:			*MIDIデータファイルの送信
	bsr	error?
	lea	midi_dump(pc),a0
	tst.b	out_flg-midi_dump(a0)
	beq	@f
	bsr	wrt_str
	move.l	12(sp),a0
	bsr	wrt_str
	bra	wrt_dsk
@@:
	move.l	12(sp),a2	*a2=file name
	bsr	fopen
	tst.l	d5
	bmi	m_err62		*read error
	bsr	read		*return=a5:address,d3:size
	moveq.l	#0,d2		*ascii mode
	movea.l	a5,a1		*address
	Z_MUSIC	#$18
	pea	(a5)
	DOS	_MFREE
	addq.w	#4,sp
	bra	ok

m_fmvset:			*ＦＭ音源音色登録その2
	bsr	error?
	lea	rem2(pc),a0
	tst.b	out_flg-rem2(a0)
	beq	@f
	move.b	#9,(a2)+	*tab
	bsr	wrt_str
	lea	fmvset(pc),a0	*cmd header
	bsr	wrt_str
	move.l	12(sp),d0
	bsr	wrt_num		*tone number
	move.b	#'=',(a2)+
	move.b	#'{',(a2)+
	move.l	22(sp),a1
	cmpi.w	#4,8(a1)
	bne	m_err7		*配列の次元が違う
	cmpi.w	#10,10(a1)
	bne	m_err7
	lea	$10(a1),a1
	moveq.l	#4,d1
fmvslp00:
	tst.b	d1
	bne	non_rem3
	lea	rem3(pc),a0
	bsr	wrt_str
non_rem3:
	cmpi.b	#4,d1
	beq	only1tab
	move.b	#9,(a2)+
only1tab:
	move.b	#9,(a2)+
*	tst.b	d1
*	bne	_11dt
*	moveq.l	#4-1,d2
*	bra	fmvslp01	*最後はal,fb,om,panだけ
_11dt:
	moveq.l	#11-1,d2
fmvslp01:
	moveq.l	#0,d0
	move.b	(a1)+,d0
	bsr	wrt_num2
	move.b	#',',(a2)+
	dbra	d2,fmvslp01
	subq.w	#1,a2
	bsr	do_crlf
	dbra	d1,fmvslp00
	subq.w	#2,a2
	move.b	#'}',(a2)+
	bra	wrt_dsk
@@:
	move.l	12(sp),d2	*sound number
	movea.l	22(sp),a1	*配列
	cmpi.w	#4,8(a1)
	bne	m_err7		*配列の次元が違う
	cmpi.w	#10,10(a1)
	bne	m_err7
	lea	$10(a1),a1
	Z_MUSIC	#$1b
	tst.l	d0
	bne	error
	bra	ok

m_out:				*ＭＩＤＩデータ出力
	bsr	error?
	lea	midi_data(pc),a0
	tst.b	out_flg-midi_data(a0)
	beq	normal_mout	*セーブモードじゃない
	bsr	wrt_str
	lea	6(sp),a0
	moveq.l	#10-1,d1
_moutlp:
	tst.w	(a0)		*省略?
	bmi	@f
	move.l	6(a0),d0
	bsr	wrt_num3	*16進数書き込み
	move.b	#',',(a2)+
@@:
	lea	10(a0),a0
	dbra	d1,_moutlp
	subq.w	#1,a2
	move.b	#'}',(a2)+
	bra	wrt_dsk

normal_mout:
	lea	md_buf(pc),a1
	lea	6(sp),a0
	moveq.l	#10-1,d1
moutlp:
	tst.w	(a0)		*省略?
	bmi	@f
	move.l	6(a0),d0
	move.b	d0,(a1)+	*値を一時的にバッファへ書き込み
@@:
	lea	10(a0),a0
	dbra	d1,moutlp
	lea	md_buf(pc),a0
	suba.l	a0,a1
	move.l	a1,d2
	movea.l	a0,a1
	Z_MUSIC	#$18
	tst.l	d0
	bne	error
	bra	ok

m_dirout:
	bsr	error?
	move.b	out_flg(pc),d0
	beq	normal_mdir
	movea.l	12(sp),a1	*配列
	moveq.l	#0,d2
	move.w	8(a1),d2
	addq.l	#1,d2		*サイズに直す
	lea	10(a1),a1
	tst.w	16(sp)
	bmi	@f
	move.l	22(sp),d1	*get size
	beq	m_err70		*illegal parameter
	cmp.l	d2,d1
	bhi	m_err70		*サイズが添え字より大きい!?
	move.l	d1,d2
@@:
	lea	midi_data(pc),a0
kyoutsu:
	bsr	wrt_str
	move.b	#9,(a2)+
kyoutsu2:
	moveq.l	#0,d1		*一行のデータ数カウンタ
	moveq.l	#0,d0
	subq.l	#1,d2		*for dbra
mdirlp00:
	move.b	(a1)+,d0
	bsr	wrt_num3	*hex
	move.b	#',',(a2)+
	addq.b	#1,d1
	cmpi.b	#16,d1		*16データごと改行
	bne	@f
	tst.l	d2		*最終ループの時は無視
	beq	@f
	moveq.l	#0,d1
	move.b	#13,-1(a2)
	move.b	#10,(a2)+
	move.b	#9,(a2)+	*tabx2
	move.b	#9,(a2)+
@@:
	dbra	d2,mdirlp00
	move.b	#'}',-1(a2)
	bra	wrt_dsk

normal_mdir:
	movea.l	12(sp),a1	*配列
	moveq.l	#0,d2
	move.w	8(a1),d2
	addq.l	#1,d2		*サイズに直す
	lea	10(a1),a1
	tst.w	16(sp)
	bmi	@f
	move.l	22(sp),d1	*get size
	beq	m_err70		*illegal parameter
	cmp.l	d2,d1
	bhi	m_err70		*サイズが添え字より大きい!?
	move.l	d1,d2
@@:
	Z_MUSIC	#$18
	tst.l	d0
	bne	error
	bra	ok

m_exc:				*エクスクルーシブ転送
	bsr	error?
	move.b	out_flg(pc),d0
	beq	normal_mexc
	movea.l	12(sp),a1	*配列
	moveq.l	#0,d2
	move.w	8(a1),d2	*d2=添え字の大きさ
	addq.l	#1,d2		*サイズに直す
	lea	10(a1),a1
	tst.w	16(sp)
	bmi	@f		*省略のケース
	move.l	22(sp),d1	*get size
	beq	m_err70		*illegal parameter
	cmp.l	d2,d1
	bhi	m_err70		*サイズが添え字より大きい!?
	move.l	d1,d2
@@:
	lea	exclusive(pc),a0
	bra	kyoutsu		*後はdiroutと共通
normal_mexc:
	movea.l	12(sp),a1	*配列
	moveq.l	#0,d2
	move.w	8(a1),d2	*d2=添え字の大きさ
	addq.l	#1,d2		*サイズに直す
	lea	10(a1),a1
	tst.w	16(sp)
	bmi	@f		*省略のケース
	move.l	22(sp),d1	*get size
	beq	m_err70		*illegal parameter
	cmp.l	d2,d1
	bhi	m_err70		*サイズが添え字より大きい!?
	move.l	d1,d2
@@:
	Z_MUSIC	#$1d
	tst.l	d0
	bne	error
	bra	ok

m_roland:			*ROLANDエクスクルーシブ転送
	bsr	error?
	move.b	out_flg(pc),d0
	beq	normal_roland
	move.l	12(sp),d3	*dev id
	lsl.w	#8,d3
	move.l	22(sp),d1	*model id
	move.b	d1,d3
	movea.l	32(sp),a1	*配列
	moveq.l	#0,d2
	move.w	8(a1),d2	*d2=添え字の大きさ
	addq.l	#1,d2		*サイズに直す
	lea	10(a1),a1	*a1=data address
	tst.w	36(sp)
	bmi	@f		*省略のケース
	move.l	42(sp),d1	*get size
	beq	m_err70		*illegal parameter
	cmp.l	d2,d1
	bhi	m_err70		*サイズが添え字より大きい!?
	move.l	d1,d2
@@:
	lea	roland(pc),a0
	bsr	wrt_str
	moveq.l	#0,d0
	ror.w	#8,d3
	move.b	d3,d0
	bsr	wrt_num3	*dev
	move.b	#',',(a2)+
	lsr.w	#8,d3
	move.b	d3,d0
	bsr	wrt_num3	*model
	move.b	#'=',(a2)+
	move.b	#'{',(a2)+
	bsr	do_crlf
	move.b	#9,(a2)+
	move.b	#9,(a2)+
	bra	kyoutsu2	*後はdiroutと共通

normal_roland:
	move.l	12(sp),d3	*dev id
	lsl.w	#8,d3
	move.l	22(sp),d1	*model id
	move.b	d1,d3
	movea.l	32(sp),a1	*配列
	moveq.l	#0,d2
	move.w	8(a1),d2	*d2=添え字の大きさ
	addq.l	#1,d2		*サイズに直す
	lea	10(a1),a1	*a1=data address
	tst.w	36(sp)
	bmi	@f		*省略のケース
	move.l	42(sp),d1	*get size
	beq	m_err70		*illegal parameter
	cmp.l	d2,d1
	bhi	m_err70		*サイズが添え字より大きい!?
	move.l	d1,d2
@@:
	Z_MUSIC	#$1c
	tst.l	d0
	bne	error
	bra	ok

m_total:			*ステップタイムのトータル値表示
	bsr	error?
	move.b	out_flg(pc),d0
	bne	m_err10		*can't compile
	moveq.l	#0,d2
	Z_MUSIC	#$19
	tst.l	d0
	bne	error
	bra	ok

m_fadeout:			*フェードアウト
	bsr	error?
	move.b	out_flg(pc),d0
	bne	m_err10		*can't compile
	moveq.l	#16,d2
	tst.w	6(sp)		*省略?
	bmi	@f
	move.l	12(sp),d2
@@:
	Z_MUSIC	#$1a
	tst.l	d0
	bne	error
	bra	ok

m_pcmcnf:			*ＡＤＰＣＭコンフィギュレーションファイルセット
	bsr	error?
	lea	adpcm_list(pc),a0
	tst.b	out_flg-adpcm_list(a0)
	beq	@f
	bsr	wrt_str
	move.l	12(sp),a0
	bsr	wrt_str
	bra	wrt_dsk
@@:
	move.l	12(sp),a2
	bsr	fopen
	tst.l	d5
	bmi	m_err62		*read error
	bsr	read		*return a5=address,d3=size
	lea	OPM(PC),a2
	bsr	self_output
	pea	(a5)
	DOS	_MFREE
	addq.w	#4,sp
	bra	ok

m_switch:			*ファイル書き出しモード
	move.b	running(pc),d0
	bmi	m_err10		*m_switch()はステートメントではありません
	move.b	out_flg(pc),d0
	tst.w	6(sp)		*省略時
	bmi	@f
	move.l	12(sp),d0
	sne	out_flg
@@:
	tst.w	16(sp)
	bmi	_default_fn	*ファイルネーム省略時はデフォルトを生成
	move.l	22(sp),a0
	bra	@f
_default_fn:
	lea	dflt_fn(pc),a0
@@:
	lea	gene_fn(pc),a1
	moveq.l	#0,d1
gfnlp:
	move.b	(a0)+,d0
	cmpi.b	#'.',d0
	bne	@f
	st	d1
@@:
	move.b	d0,(a1)+
	cmpi.b	#' ',d0
	bhi	gfnlp		*終了
	subq.w	#1,a1
	tst.b	d1
	bne	ok
	move.b	#'.',(a1)+	*拡張子をセット
	move.b	#'Z',(a1)+
	move.b	#'M',(a1)+
	move.b	#'S',(a1)+
	clr.b	(a1)
	bra	ok

sc55_vr:			*sc55 voice resereve
	bsr	error?
	move.b	out_flg(pc),d0
	beq	normal_sc55vr
	movea.l	12(sp),a1
	cmpi.w	#15,8(a1)
	bne	m_err7		*配列のサイズが違う
	lea	10(a1),a1	*a1=data pointer
	moveq.l	#-1,d3
	tst.w	16(sp)
	bmi	@f
	move.l	22(sp),d3	*get dev id
@@:
	lea	sc55_vr_(pc),a0
	bsr	wrt_str
	move.l	d3,d0
	bmi	@f
	bsr	wrt_num3	*dev id
@@:
	move.b	#'=',(a2)+
	move.b	#'{',(a2)+
	moveq.l	#0,d0
	moveq.l	#16-1,d1
@@:
	move.b	(a1)+,d0
	bsr	wrt_num
	move.b	#',',(a2)+
	dbra	d1,@b
	move.b	#'}',-1(a2)
	bra	wrt_dsk
normal_sc55vr:
	movea.l	12(sp),a1
	cmpi.w	#15,8(a1)
	bne	m_err7		*配列のサイズが違う
	lea	10(a1),a1	*a1=data pointer
	moveq.l	#-1,d3
	tst.w	16(sp)
	bmi	@f
	move.l	22(sp),d3	*get dev id
@@:
	moveq.l	#16,d2
	Z_MUSIC	#$1e
	tst.l	d0
	bne	error
	bra	ok

sc55_rvb:			*sc55 reverb parameter
	bsr	error?
	move.b	out_flg(pc),d0
	beq	normal_sc55rvb
	movea.l	12(sp),a1
	cmpi.w	#7-1,8(a1)
	bhi	m_err7		*配列のサイズが違う
	move.w	8(a1),d2	*d2=添え字の大きさ(=dbra counter)
	lea	10(a1),a1	*a1=data pointer
	moveq.l	#-1,d3
	tst.w	16(sp)
	bmi	@f
	move.l	22(sp),d3	*get dev id
@@:
	lea	sc55_rvb_(pc),a0
	bsr	wrt_str
	move.l	d3,d0
	bmi	@f
	bsr	wrt_num3	*dev id
@@:
	move.b	#'=',(a2)+
	move.b	#'{',(a2)+
	moveq.l	#0,d0
@@:
	move.b	(a1)+,d0
	bsr	wrt_num
	move.b	#',',(a2)+
	dbra	d2,@b
	move.b	#'}',-1(a2)
	bra	wrt_dsk
normal_sc55rvb:
	movea.l	12(sp),a1
	cmpi.w	#7-1,8(a1)
	bhi	m_err7		*配列のサイズが違う
	moveq.l	#0,d2
	move.w	8(a1),d2	*d2=添え字の大きさ
	addq.l	#1,d2		*サイズに直す
	lea	10(a1),a1	*a1=data pointer
	moveq.l	#-1,d3
	tst.w	16(sp)
	bmi	@f
	move.l	22(sp),d3	*get dev id
@@:
	Z_MUSIC	#$1f
	tst.l	d0
	bne	error
	bra	ok

sc55_cho:			*sc55 chorus parameter
	bsr	error?
	move.b	out_flg(pc),d0
	beq	normal_sc55cho
	movea.l	12(sp),a1
	cmpi.w	#8-1,8(a1)
	bhi	m_err7		*配列のサイズが違う
	move.w	8(a1),d2	*d2=添え字の大きさ(=dbra counter)
	lea	10(a1),a1	*a1=data pointer
	moveq.l	#-1,d3
	tst.w	16(sp)
	bmi	@f
	move.l	22(sp),d3	*get dev id
@@:
	lea	sc55_cho_(pc),a0
	bsr	wrt_str
	move.l	d3,d0
	bmi	@f
	bsr	wrt_num3	*dev id
@@:
	move.b	#'=',(a2)+
	move.b	#'{',(a2)+
	moveq.l	#0,d0
@@:
	move.b	(a1)+,d0
	bsr	wrt_num
	move.b	#',',(a2)+
	dbra	d2,@b
	move.b	#'}',-1(a2)
	bra	wrt_dsk
normal_sc55cho:
	movea.l	12(sp),a1
	cmpi.w	#8-1,8(a1)
	bhi	m_err7		*配列のサイズが違う
	moveq.l	#0,d2
	move.w	8(a1),d2	*d2=添え字の大きさ
	addq.l	#1,d2		*サイズに直す
	lea	10(a1),a1	*a1=data pointer
	moveq.l	#-1,d3
	tst.w	16(sp)
	bmi	@f
	move.l	22(sp),d3	*get dev id
@@:
	Z_MUSIC	#$20
	tst.l	d0
	bne	error
	bra	ok

sc55_pst:			*sc55 part parameter
	bsr	error?
	move.b	out_flg(pc),d0
	beq	normal_sc55pst
	move.l	12(sp),d1	*get part number
	cmpi.l	#16,d1
	bhi	m_err70		*16以上はエラー
	movea.l	22(sp),a1
	cmpi.w	#119-1,8(a1)
	bhi	m_err7		*配列のサイズが違う
	move.w	8(a1),d2	*d2=添え字の大きさ(=dbra counter)
	lea	10(a1),a1	*a1=data pointer
	moveq.l	#-1,d3
	tst.w	26(sp)
	bmi	@f
	move.l	32(sp),d3	*get dev id
@@:
	lea	sc55_pst_(pc),a0
	bsr	wrt_str
	move.l	d1,d0
	bsr	wrt_num3	*part number
	move.l	d3,d0
	bmi	@f
	move.b	#',',(a2)+
	bsr	wrt_num3	*dev id
@@:
	move.b	#'=',(a2)+
	move.b	#'{',(a2)+
	moveq.l	#0,d0
@@:
	move.b	(a1)+,d0
	bsr	wrt_num
	move.b	#',',(a2)+
	dbra	d2,@b
	move.b	#'}',-1(a2)
	bra	wrt_dsk
normal_sc55pst:
	move.l	12(sp),d3
	cmpi.l	#16,d3
	bhi	m_err70		*16以上はエラー
	swap	d3
	movea.l	22(sp),a1
	cmpi.w	#119-1,8(a1)
	bhi	m_err7		*配列のサイズが違う
	moveq.l	#0,d2
	move.w	8(a1),d2	*d2=添え字の大きさ
	addq.l	#1,d2		*サイズに直す
	lea	10(a1),a1	*a1=data pointer
	moveq.l	#-1,d1
	tst.w	26(sp)
	bmi	@f
	move.l	32(sp),d1	*get dev id
@@:
	move.w	d1,d3
	Z_MUSIC	#$21
	tst.l	d0
	bne	error
	bra	ok

sc55_dst:			*sc55 drum parameter
	bsr	error?
	move.b	out_flg(pc),d0
	beq	normal_sc55dst
	move.l	12(sp),d0	*get map number
	cmpi.l	#1,d0
	bhi	m_err70		*1以上はエラー
	move.l	22(sp),d1	*get key code
	cmpi.l	#127,d1
	bhi	m_err70
	movea.l	32(sp),a1
	cmpi.w	#8-1,8(a1)
	bhi	m_err7		*配列のサイズが違う
	move.w	8(a1),d2	*d2=添え字の大きさ(=dbra counter)
	lea	10(a1),a1	*a1=data pointer
	moveq.l	#-1,d3
	tst.w	36(sp)
	bmi	@f
	move.l	42(sp),d3	*get dev id
@@:
	lea	sc55_dst_(pc),a0
	bsr	wrt_str
	bsr	wrt_num3	*map number
	move.b	#',',(a2)+
	move.l	d1,d0
	bsr	wrt_num3	*key number
	move.l	d3,d0
	bmi	@f
	move.b	#',',(a2)+
	bsr	wrt_num3	*dev id
@@:
	move.b	#'=',(a2)+
	move.b	#'{',(a2)+
	moveq.l	#0,d0
@@:
	move.b	(a1)+,d0
	bsr	wrt_num
	move.b	#',',(a2)+
	dbra	d2,@b
	move.b	#'}',-1(a2)
	bra	wrt_dsk
normal_sc55dst:
	move.l	12(sp),d3	*get map number
	cmpi.l	#1,d3
	bhi	m_err70		*1以上はエラー
	move.l	22(sp),d0	*get note number
	cmpi.l	#127,d0
	bhi	m_err70
	lsl.w	#8,d3
	move.b	d0,d3
	swap	d3
	movea.l	32(sp),a1
	cmpi.w	#8-1,8(a1)
	bhi	m_err7		*配列のサイズが違う
	moveq.l	#0,d2
	move.w	8(a1),d2	*d2=添え字の大きさ
	addq.l	#1,d2		*サイズに直す
	lea	10(a1),a1	*a1=data pointer
	moveq.l	#-1,d1
	tst.w	36(sp)
	bmi	@f
	move.l	42(sp),d1	*get dev id
@@:
	move.w	d1,d3
	Z_MUSIC	#$22
	tst.l	d0
	bne	error
	bra	ok

sc55_prt:			*SC55に文字列を表示する
	bsr	error?
	lea	sc55_prt_(pc),a0
	tst.b	out_flg-sc55_prt_(a0)
	beq	normal_sc55prt
	bsr	wrt_str
	tst.w	16(sp)		*dev id省略?
	bmi	@f
	move.l	22(sp),d0
	bsr	wrt_num3	*dev id
@@:
	move.b	#':',(a2)+
	move.b	#'"',(a2)+
	move.l	12(sp),a0
	tst.b	(a0)
	beq	m_err70		*length=0は駄目
	bsr	wrt_str
	move.b	#'"',(a2)+
	bra	wrt_dsk
normal_sc55prt:
	move.l	12(sp),a1
	move.l	a1,a0
	moveq.l	#0,d2
@@:
	addq.w	#1,d2		*count length
	tst.b	(a0)+
	bne	@b
	subq.w	#1,d2
	beq	m_err70
	cmpi.w	#32,d2
	bhi	m_err70		*文字データ多すぎ
	moveq.l	#-1,d3
	tst.w	16(sp)
	bmi	@f
	move.l	22(sp),d3	*get dev id
@@:
	Z_MUSIC	#$23
	tst.l	d0
	bne	error
	bra	ok

sc55_dsp:			*SC55のレベル・ディスプレイに表示する
	bsr	error?
	move.b	out_flg(pc),d0
	beq	normal_sc55dsp
	movea.l	12(sp),a1
	cmpi.w	#16-1,8(a1)
	bne	m_err7		*配列のサイズが違う
	lea	10(a1),a1	*a1=data pointer
	moveq.l	#-1,d3
	tst.w	16(sp)
	bmi	@f
	move.l	22(sp),d3	*get dev id
@@:
	lea	sc55_dsp_(pc),a0
	bsr	wrt_str
	move.l	d3,d0
	bmi	@f
	bsr	wrt_num3	*dev id
@@:
	move.b	#'=',(a2)+
	move.b	#'{',(a2)+
	moveq.l	#0,d0
	moveq.l	#16-1,d2	*for dbra
sc55dsplp:
	move.l	(a1)+,d0
	bsr	wrt_num
	move.b	#',',(a2)+
	cmpi.b	#8,d2
	bne	@f
	move.b	#13,-1(a2)
	move.b	#10,(a2)+
	move.b	#9,(a2)+
	move.b	#9,(a2)+
@@:
	dbra	d2,sc55dsplp
	move.b	#'}',-1(a2)
	bra	wrt_dsk
normal_sc55dsp
	movea.l	12(sp),a0
	cmpi.w	#16-1,8(a0)
	bne	m_err7		*配列のサイズが違う
	lea	10(a0),a0	*a0=data pointer
	moveq.l	#-1,d3
	tst.w	16(sp)
	bmi	@f
	move.l	22(sp),d3	*get dev id
@@:
	lea	env_work(pc),a1
	moveq.l	#16-1,d2
@@:
	move.l	(a0)+,d0
	move.w	d0,(a1)+
	dbra	d2,@b
	lea	env_work(pc),a1
	Z_MUSIC	#$24
	tst.l	d0
	bne	error
	bra	ok

adpcm_block:			*ブロックデータの読み込み
	bsr	error?
	lea	adpcm_block_(pc),a0
	tst.b	out_flg-adpcm_block_(a0)
	beq	@f
	bsr	wrt_str
	move.l	12(sp),a0
	bsr	wrt_str
	bra	wrt_dsk
@@:
	move.l	12(sp),a1
	Z_MUSIC	#$39
	tst.l	d0
	bne	error
	bra	ok

mt32_pr:			*mt32パーシャルリザーブ
	bsr	error?
	move.b	out_flg(pc),d0
	beq	normal_mt32pr
	movea.l	12(sp),a1
	cmpi.w	#9-1,8(a1)
	bne	m_err7		*配列のサイズが違う
	lea	10(a1),a1	*a1=data pointer
	moveq.l	#-1,d3
	tst.w	16(sp)
	bmi	@f
	move.l	22(sp),d3	*get dev id
@@:
	lea	mt32_pr_(pc),a0
	bsr	wrt_str
	move.l	d3,d0
	bmi	@f
	bsr	wrt_num3	*dev id
@@:
	move.b	#'=',(a2)+
	move.b	#'{',(a2)+
	moveq.l	#0,d0
	moveq.l	#9-1,d1
@@:
	move.b	(a1)+,d0
	bsr	wrt_num
	move.b	#',',(a2)+
	dbra	d1,@b
	move.b	#'}',-1(a2)
	bra	wrt_dsk
normal_mt32pr:
	movea.l	12(sp),a1
	cmpi.w	#9-1,8(a1)
	bne	m_err7		*配列のサイズが違う
	lea	10(a1),a1	*a1=data pointer
	moveq.l	#-1,d3
	tst.w	16(sp)
	bmi	@f
	move.l	22(sp),d3	*get dev id
@@:
	moveq.l	#9,d2
	Z_MUSIC	#$25
	tst.l	d0
	bne	error
	bra	ok

mt32_rvb:			*mt32リバーブ・パラメータ設定
	bsr	error?
	move.b	out_flg(pc),d0
	beq	normal_mt32rvb
	movea.l	12(sp),a1
	move.w	8(a1),d2	*添え字
	cmpi.w	#3-1,d2
	bhi	m_err7		*配列のサイズが違う
	lea	10(a1),a1	*a1=data pointer
	moveq.l	#-1,d3
	tst.w	16(sp)
	bmi	@f
	move.l	22(sp),d3	*get dev id
@@:
	lea	mt32_rvb_(pc),a0
	bsr	wrt_str
	move.l	d3,d0
	bmi	@f
	bsr	wrt_num3	*dev id
@@:
	move.b	#'=',(a2)+
	move.b	#'{',(a2)+
	moveq.l	#0,d0
@@:
	move.b	(a1)+,d0
	bsr	wrt_num
	move.b	#',',(a2)+
	dbra	d2,@b
	move.b	#'}',-1(a2)
	bra	wrt_dsk
normal_mt32rvb:
	movea.l	12(sp),a1
	moveq.l	#0,d2
	move.w	8(a1),d2	*添え字
	cmpi.w	#3-1,d2
	bhi	m_err7		*配列のサイズが違う
	addq.l	#1,d2		*make d2 size
	lea	10(a1),a1	*a1=data pointer
	moveq.l	#-1,d3
	tst.w	16(sp)
	bmi	@f
	move.l	22(sp),d3	*get dev id
@@:
	Z_MUSIC	#$26
	tst.l	d0
	bne	error
	bra	ok

mt32_pst:			*mt32パートＭＩＤＩチャンネル設定
	bsr	error?
	move.b	out_flg(pc),d0
	beq	normal_mt32pst
	movea.l	12(sp),a1
	move.w	8(a1),d2
	cmpi.w	#9-1,d2
	bhi	m_err7		*配列のサイズが違う
	lea	10(a1),a1	*a1=data pointer
	moveq.l	#-1,d3
	tst.w	16(sp)
	bmi	@f
	move.l	22(sp),d3	*get dev id
@@:
	lea	mt32_pst_(pc),a0
	bsr	wrt_str
	move.l	d3,d0
	bmi	@f
	bsr	wrt_num3	*dev id
@@:
	move.b	#'=',(a2)+
	move.b	#'{',(a2)+
	moveq.l	#0,d0
	moveq.l	#9-1,d1
@@:
	move.b	(a1)+,d0
	bsr	wrt_num
	move.b	#',',(a2)+
	dbra	d2,@b
	move.b	#'}',-1(a2)
	bra	wrt_dsk
normal_mt32pst:
	movea.l	12(sp),a1
	moveq.l	#0,d2
	move.w	8(a1),d2
	cmpi.w	#9-1,d2
	bhi	m_err7		*配列のサイズが違う
	addq.l	#1,d2
	lea	10(a1),a1	*a1=data pointer
	moveq.l	#-1,d3
	tst.w	16(sp)
	bmi	@f
	move.l	22(sp),d3	*get dev id
@@:
	Z_MUSIC	#$27
	tst.l	d0
	bne	error
	bra	ok

mt32_dst:			*mt32ドラムセットアップ設定
	bsr	error?
	move.b	out_flg(pc),d0
	beq	normal_mt32dst
	move.l	12(sp),d1	*note number
	cmpi.l	#87,d1
	bhi	m_err70
	cmpi.l	#24,d1
	bcs	m_err70
	movea.l	22(sp),a1
	move.w	8(a1),d2	*添え字
	cmpi.w	#4-1,d2
	bhi	m_err7		*配列のサイズが違う
	lea	10(a1),a1	*a1=data pointer
	moveq.l	#-1,d3
	tst.w	26(sp)
	bmi	@f
	move.l	32(sp),d3	*get dev id
@@:
	lea	mt32_dst_(pc),a0
	bsr	wrt_str
	move.l	d1,d0
	bsr	wrt_num		*note num
	move.l	d3,d0
	bmi	@f
	move.b	#',',(a2)+
	bsr	wrt_num3	*dev id
@@:
	move.b	#'=',(a2)+
	move.b	#'{',(a2)+
	moveq.l	#0,d0
@@:
	move.b	(a1)+,d0
	bsr	wrt_num
	move.b	#',',(a2)+
	dbra	d2,@b
	move.b	#'}',-1(a2)
	bra	wrt_dsk
normal_mt32dst:
	move.l	12(sp),d3
	swap	d3
	movea.l	22(sp),a1
	moveq.l	#0,d2
	move.w	8(a1),d2	*添え字
	cmpi.w	#4-1,d2
	bhi	m_err7		*配列のサイズが違う
	addq.l	#1,d2		*make d2 size
	lea	10(a1),a1	*a1=data pointer
	moveq.l	#-1,d1
	tst.w	26(sp)
	bmi	@f
	move.l	32(sp),d1	*get dev id
@@:
	move.w	d1,d3
	Z_MUSIC	#$28
	tst.l	d0
	bne	error
	bra	ok

mt32_cmn:			*mt32音色コモンパラメータ設定
	bsr	error?
	move.b	out_flg(pc),d0
	beq	normal_mt32cmn
	move.l	12(sp),d1	*timbre number
	cmpi.l	#64,d1
	bhi	m_err70
	tst.l	d1
	beq	m_err70		*1～64以外はエラー
	movea.l	22(sp),a3	*name address
	tst.b	(a3)
	beq	m_err70
	movea.l	32(sp),a1
	move.w	8(a1),d2	*添え字
	cmpi.w	#4-1,d2
	bhi	m_err7		*配列のサイズが違う
	lea	10(a1),a1	*a1=data pointer
	moveq.l	#-1,d3
	tst.w	36(sp)
	bmi	@f
	move.l	42(sp),d3	*get dev id
@@:
	lea	mt32_cmn_(pc),a0
	bsr	wrt_str
	move.l	d1,d0
	bsr	wrt_num		*timbre num
	move.l	d3,d0
	bmi	@f
	move.b	#',',(a2)+
	bsr	wrt_num3	*dev id
@@:
	move.b	#'=',(a2)+
	move.b	#'{',(a2)+
	move.b	#'"',(a2)+
@@:
	move.b	(a3)+,(a2)+	*set timbre name
	bne	@b
	move.b	#'"',-1(a2)
	move.b	#',',(a2)+
	moveq.l	#0,d0
@@:
	move.b	(a1)+,d0
	bsr	wrt_num
	move.b	#',',(a2)+
	dbra	d2,@b
	move.b	#'}',-1(a2)
	bra	wrt_dsk
normal_mt32cmn:
	move.l	12(sp),d3	*timbre number
	swap	d3
	movea.l	22(sp),a3	*name address
	movea.l	32(sp),a1
	moveq.l	#0,d2
	move.w	8(a1),d2	*添え字
	cmpi.w	#4-1,d2
	bhi	m_err7		*配列のサイズが違う
	lea	10(a1),a1	*a1=data pointer
	moveq.l	#-1,d1
	tst.w	36(sp)
	bmi	@f
	move.l	42(sp),d1	*get dev id
@@:
	move.w	d1,d3		*set id to d3
	lea	env_work(pc),a0
	moveq.l	#0,d0
@@:
	addq.w	#1,d0
	move.b	(a3)+,(a0)+	*set timbre name
	bne	@b
	subq.w	#1,d0
	beq	m_err70
	cmpi.b	#10,d0
	bhi	m_err70
@@:
	move.b	(a1)+,(a0)+	*set parameters
	dbra	d2,@b
	lea	env_work(pc),a1	*a1=data address
	move.l	a0,d2
	sub.l	a1,d2
	Z_MUSIC	#$29
	tst.l	d0
	bne	error
	bra	ok

mt32_ptch:			*mt32パッチパラメータ設定
	bsr	error?
	move.b	out_flg(pc),d0
	beq	normal_mt32ptch
	move.l	12(sp),d1	*patch number
	cmpi.l	#128,d1
	bhi	m_err70
	tst.l	d1		*1～128以外はエラー
	beq	m_err70
	movea.l	22(sp),a1
	move.w	8(a1),d2	*添え字
	cmpi.w	#7-1,d2
	bhi	m_err7		*配列のサイズが違う
	lea	10(a1),a1	*a1=data pointer
	moveq.l	#-1,d3
	tst.w	26(sp)
	bmi	@f
	move.l	32(sp),d3	*get dev id
@@:
	lea	mt32_ptch_(pc),a0
	bsr	wrt_str
	move.l	d1,d0
	bsr	wrt_num		*patch num
	move.l	d3,d0
	bmi	@f
	move.b	#',',(a2)+
	bsr	wrt_num3	*dev id
@@:
	move.b	#'=',(a2)+
	move.b	#'{',(a2)+
	moveq.l	#0,d0
@@:
	move.b	(a1)+,d0
	bsr	wrt_num
	move.b	#',',(a2)+
	dbra	d2,@b
	move.b	#'}',-1(a2)
	bra	wrt_dsk
normal_mt32ptch:
	move.l	12(sp),d3
	swap	d3
	movea.l	22(sp),a1
	moveq.l	#0,d2
	move.w	8(a1),d2	*添え字
	cmpi.w	#7-1,d2
	bhi	m_err7		*配列のサイズが違う
	addq.l	#1,d2		*make d2 size
	lea	10(a1),a1	*a1=data pointer
	moveq.l	#-1,d1
	tst.w	26(sp)
	bmi	@f
	move.l	32(sp),d1	*get dev id
@@:
	move.w	d1,d3
	Z_MUSIC	#$2b
	tst.l	d0
	bne	error
	bra	ok

mt32_prtl:			*mt32音色パーシャルパラメータ設定
	bsr	error?
	move.b	out_flg(pc),d0
	beq	normal_mt32prtl
	move.l	12(sp),d0	*timbre number
	cmpi.l	#64,d0
	bhi	m_err70
	tst.l	d0
	beq	m_err70		*1～64以外はエラー
	move.l	22(sp),d1	*name address
	cmpi.l	#4,d1
	bhi	m_err70
	tst.l	d1
	beq	m_err70		1～4以外はエラー
	movea.l	32(sp),a1
	move.w	8(a1),d2	*添え字
	cmpi.w	#58-1,d2
	bhi	m_err7		*配列のサイズが違う
	lea	10(a1),a1	*a1=data pointer
	moveq.l	#-1,d3
	tst.w	36(sp)
	bmi	@f
	move.l	42(sp),d3	*get dev id
@@:
	lea	mt32_prtl_(pc),a0
	bsr	wrt_str
	bsr	wrt_num		*timbre number
	move.b	#',',(a2)+
	move.l	d1,d0
	bsr	wrt_num		*partial number
	move.l	d3,d0
	bmi	@f
	move.b	#',',(a2)+
	bsr	wrt_num3	*dev id
@@:
	move.b	#'=',(a2)+
	move.b	#'{',(a2)+
	bsr	do_crlf
	lea	rem4(pc),a0
	bsr	wrt_str
	move.b	#9,(a2)+	*tab
	moveq.l	#0,d0
	moveq.l	#0,d1
prtllp:
	cmpi.b	#8,d1
	bne	@f
	subq.w	#1,a2
	lea	rem5(pc),a0
	bsr	wrt_str
	move.b	#9,(a2)+
@@:
	cmpi.b	#$14,d1
	bne	@f
	subq.w	#1,a2
	lea	rem6(pc),a0
	bsr	wrt_str
	move.b	#9,(a2)+
@@:
	cmpi.b	#$17,d1
	bne	@f
	subq.w	#1,a2
	lea	rem7(pc),a0
	bsr	wrt_str
	move.b	#9,(a2)+
@@:
	cmpi.b	#$1C,d1
	bne	@f
	subq.w	#1,a2
	lea	rem8(pc),a0
	bsr	wrt_str
	move.b	#9,(a2)+
@@:
	cmpi.b	#$29,d1
	bne	@f
	subq.w	#1,a2
	lea	rem9(pc),a0
	bsr	wrt_str
	move.b	#9,(a2)+
@@:
	cmpi.b	#$2f,d1
	bne	@f
	subq.w	#1,a2
	lea	rem10(pc),a0
	bsr	wrt_str
	move.b	#9,(a2)+
@@:
	move.b	(a1)+,d0
	bsr	wrt_num2
	move.b	#',',(a2)+
	addq.b	#1,d1
	dbra	d2,prtllp
	move.b	#'}',-1(a2)
	bra	wrt_dsk
normal_mt32prtl:
	move.l	12(sp),d3	*timbre number
	lsl.w	#8,d3
	move.l	22(sp),d0	*partial number
	move.b	d0,d3
	swap	d3
	movea.l	32(sp),a1
	moveq.l	#0,d2
	move.w	8(a1),d2	*添え字
	cmpi.w	#58-1,d2
	bhi	m_err7		*配列のサイズが違う
	addq.l	#1,d2		*d2をサイズへ
	lea	10(a1),a1	*a1=data pointer
	moveq.l	#-1,d1
	tst.w	36(sp)
	bmi	@f
	move.l	42(sp),d1	*get dev id
@@:
	move.w	d1,d3		*set id to d3
	Z_MUSIC	#$2a
	tst.l	d0
	bne	error
	bra	ok

mt32_prt:			*MT32に文字列を表示する
	bsr	error?
	lea	mt32_prt_(pc),a0
	tst.b	out_flg-mt32_prt_(a0)
	beq	normal_mt32prt
	bsr	wrt_str
	tst.w	16(sp)		*dev id省略?
	bmi	@f
	move.l	22(sp),d0
	bsr	wrt_num3	*dev id
@@:
	move.b	#':',(a2)+
	move.b	#'"',(a2)+
	move.l	12(sp),a0
	tst.b	(a0)
	beq	m_err70		*length=0は駄目
	bsr	wrt_str
	move.b	#'"',(a2)+
	bra	wrt_dsk
normal_mt32prt:
	move.l	12(sp),a1
	move.l	a1,a0
	moveq.l	#0,d2
@@:
	addq.w	#1,d2		*count length
	tst.b	(a0)+
	bne	@b
	subq.w	#1,d2
	beq	m_err70
	cmpi.w	#20,d2
	bhi	m_err70		*文字データ多すぎ
	moveq.l	#-1,d3
	tst.w	16(sp)
	bmi	@f
	move.l	22(sp),d3	*get dev id
@@:
	Z_MUSIC	#$2c
	tst.l	d0
	bne	error
	bra	ok

m_print:			*文字列を表示する
	bsr	error?
	lea	m_prt_(pc),a0
	tst.b	out_flg-m_prt_(a0)
	beq	normal_mprt
	bsr	wrt_str
	move.b	#':',(a2)+
	move.b	#'"',(a2)+
	move.l	12(sp),a0
	tst.b	(a0)
	beq	m_err70		*length=0は駄目
	bsr	wrt_str
	move.b	#'"',(a2)+
	bra	wrt_dsk
normal_mprt:
	move.l	12(sp),a1
	move.l	a1,a0
	moveq.l	#0,d2
@@:
	addq.w	#1,d2		*count length
	tst.b	(a0)+
	bne	@b
	subq.w	#1,d2
	beq	m_err70
	cmpi.w	#96,d2
	bhi	m_err70		*文字データ多すぎ
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
	bra	ok

u220_prt:			*U220に文字列を表示する
	bsr	error?
	lea	u220_prt_(pc),a0
	tst.b	out_flg-u220_prt_(a0)
	beq	normal_u220prt
	bsr	wrt_str
	tst.w	16(sp)		*dev id省略?
	bmi	@f
	move.l	22(sp),d0
	bsr	wrt_num3	*dev id
@@:
	move.b	#':',(a2)+
	move.b	#'"',(a2)+
	move.l	12(sp),a0
	tst.b	(a0)
	beq	m_err70		*length=0は駄目
	bsr	wrt_str
	move.b	#'"',(a2)+
	bra	wrt_dsk
normal_u220prt:
	move.l	12(sp),a1
	move.l	a1,a0
	moveq.l	#0,d2
@@:
	addq.w	#1,d2		*count length
	tst.b	(a0)+
	bne	@b
	subq.w	#1,d2
	beq	m_err70
	cmpi.w	#12,d2
	bhi	m_err70		*文字データ多すぎ
	moveq.l	#-1,d3
	tst.w	16(sp)
	bmi	@f
	move.l	22(sp),d3	*get dev id
@@:
	Z_MUSIC	#$31
	tst.l	d0
	bne	error
	bra	ok

u220_setup:			*u220セットアップパラメータの設定
	bsr	error?
	move.b	out_flg(pc),d0
	beq	normal_u220st
	movea.l	12(sp),a1
	cmpi.w	#7-1,8(a1)
	bne	m_err7		*配列のサイズが違う
	lea	10(a1),a1	*a1=data pointer
	moveq.l	#-1,d3
	tst.w	16(sp)
	bmi	@f
	move.l	22(sp),d3	*get dev id
@@:
	lea	u220_setup_(pc),a0
	bsr	wrt_str
	move.l	d3,d0
	bmi	@f
	bsr	wrt_num3	*dev id
@@:
	move.b	#'=',(a2)+
	move.b	#'{',(a2)+
	moveq.l	#0,d0
	moveq.l	#7-1,d1
@@:
	move.b	(a1)+,d0
	bsr	wrt_num
	move.b	#',',(a2)+
	dbra	d1,@b
	move.b	#'}',-1(a2)
	bra	wrt_dsk
normal_u220st:
	movea.l	12(sp),a1
	cmpi.w	#7-1,8(a1)
	bne	m_err7		*配列のサイズが違う
	lea	10(a1),a1	*a1=data pointer
	moveq.l	#-1,d3
	tst.w	16(sp)
	bmi	@f
	move.l	22(sp),d3	*get dev id
@@:
	moveq.l	#7,d2
	Z_MUSIC	#$2d
	tst.l	d0
	bne	error
	bra	ok

u220_cmn:			*u220テンポラリパッチ・コモンパラメータの設定
	bsr	error?
	move.b	out_flg(pc),d0
	beq	normal_u220cmn
	movea.l	12(sp),a1
	cmpi.w	#18-1,8(a1)
	bne	m_err7		*配列のサイズが違う
	lea	10(a1),a1	*a1=data pointer
	moveq.l	#-1,d3
	tst.w	16(sp)
	bmi	@f
	move.l	22(sp),d3	*get dev id
@@:
	lea	u220_cmn_(pc),a0
	bsr	wrt_str
	move.l	d3,d0
	bmi	@f
	bsr	wrt_num3	*dev id
@@:
	move.b	#'=',(a2)+
	move.b	#'{',(a2)+
	moveq.l	#0,d0
	moveq.l	#18-1,d1
@@:
	move.b	(a1)+,d0
	bsr	wrt_num
	move.b	#',',(a2)+
	dbra	d1,@b
	move.b	#'}',-1(a2)
	bra	wrt_dsk
normal_u220cmn:
	movea.l	12(sp),a1
	cmpi.w	#18-1,8(a1)
	bne	m_err7		*配列のサイズが違う
	lea	10(a1),a1	*a1=data pointer
	moveq.l	#-1,d3
	tst.w	16(sp)
	bmi	@f
	move.l	22(sp),d3	*get dev id
@@:
	moveq.l	#18,d2
	Z_MUSIC	#$2e
	tst.l	d0
	bne	error
	bra	ok

u220_dst:			*u220テンポラリ・ドラム・セットアップパラメータの設定
	bsr	error?
	move.b	out_flg(pc),d0
	beq	normal_u220dst
	movea.l	12(sp),a1
	cmpi.w	#7-1,8(a1)
	bne	m_err7		*配列のサイズが違う
	lea	10(a1),a1	*a1=data pointer
	moveq.l	#-1,d3
	tst.w	16(sp)
	bmi	@f
	move.l	22(sp),d3	*get dev id
@@:
	lea	u220_dst_(pc),a0
	bsr	wrt_str
	move.l	d3,d0
	bmi	@f
	bsr	wrt_num3	*dev id
@@:
	move.b	#'=',(a2)+
	move.b	#'{',(a2)+
	moveq.l	#0,d0
	moveq.l	#7-1,d1
@@:
	move.b	(a1)+,d0
	bsr	wrt_num
	move.b	#',',(a2)+
	dbra	d1,@b
	move.b	#'}',-1(a2)
	bra	wrt_dsk
normal_u220dst:
	movea.l	12(sp),a1
	cmpi.w	#7-1,8(a1)
	bne	m_err7		*配列のサイズが違う
	lea	10(a1),a1	*a1=data pointer
	moveq.l	#-1,d3
	tst.w	16(sp)
	bmi	@f
	move.l	22(sp),d3	*get dev id
@@:
	moveq.l	#7,d2
	Z_MUSIC	#$2f
	tst.l	d0
	bne	error
	bra	ok

u220_pst:			*u220テンポラリパート・セットアップパラメータ設定
	bsr	error?
	move.b	out_flg(pc),d0
	beq	normal_u220pst
	move.l	12(sp),d1	*part number
	cmpi.l	#6,d1
	bhi	m_err70
	tst.l	d1		*1～6以外はエラー
	beq	m_err70
	movea.l	22(sp),a1
	move.w	8(a1),d2	*添え字
	cmpi.w	#13-1,d2
	bne	m_err7		*配列のサイズが違う
	lea	10(a1),a1	*a1=data pointer
	moveq.l	#-1,d3
	tst.w	26(sp)
	bmi	@f
	move.l	32(sp),d3	*get dev id
@@:
	lea	u220_pst_(pc),a0
	bsr	wrt_str
	move.l	d1,d0
	bsr	wrt_num		*patch num
	move.l	d3,d0
	bmi	@f
	move.b	#',',(a2)+
	bsr	wrt_num3	*dev id
@@:
	move.b	#'=',(a2)+
	move.b	#'{',(a2)+
	moveq.l	#0,d0
@@:
	move.b	(a1)+,d0
	bsr	wrt_num
	move.b	#',',(a2)+
	dbra	d2,@b
	move.b	#'}',-1(a2)
	bra	wrt_dsk
normal_u220pst:
	move.l	12(sp),d3
	swap	d3
	movea.l	22(sp),a1
	moveq.l	#0,d2
	move.w	8(a1),d2	*添え字
	cmpi.w	#13-1,d2
	bne	m_err7		*配列のサイズが違う
	addq.l	#1,d2		*make d2 size
	lea	10(a1),a1	*a1=data pointer
	moveq.l	#-1,d1
	tst.w	26(sp)
	bmi	@f
	move.l	32(sp),d1	*get dev id
@@:
	move.w	d1,d3
	Z_MUSIC	#$30
	tst.l	d0
	bne	error
	bra	ok

u220_tmb:			*U220音色パラメータ設定
	bsr	error?
	move.b	out_flg(pc),d0
	beq	normal_u220tmb
	move.l	12(sp),d1	*timbre number
	cmpi.l	#128,d1
	bhi	m_err70
	tst.l	d1
	beq	m_err70		*1～128以外はエラー
	movea.l	22(sp),a3	*name address
	tst.b	(a3)
	beq	m_err70
	movea.l	32(sp),a1
	move.w	8(a1),d2	*添え字
	cmpi.w	#26-1,d2
	bne	m_err7		*配列のサイズが違う
	lea	10(a1),a1	*a1=data pointer
	moveq.l	#-1,d3
	tst.w	36(sp)
	bmi	@f
	move.l	42(sp),d3	*get dev id
@@:
	lea	u220_tmb_(pc),a0
	bsr	wrt_str
	move.l	d1,d0
	bsr	wrt_num		*timbre num
	move.l	d3,d0
	bmi	@f
	move.b	#',',(a2)+
	bsr	wrt_num3	*dev id
@@:
	move.b	#'=',(a2)+
	move.b	#'{',(a2)+
	move.b	#'"',(a2)+
	moveq.l	#0,d0
@@:
	addq.b	#1,d0
	move.b	(a3)+,(a2)+	*set timbre name
	bne	@b
	cmpi.b	#12+1,d0	*end codeもいれて１３字以上はエラー
	bhi	m_err70
	move.b	#'"',-1(a2)
	bsr	do_crlf
	lea	rem11(pc),a0
	bsr	wrt_str
	move.b	#9,(a2)+
	moveq.l	#0,d0
	moveq.l	#0,d1
tmblp:
	cmpi.b	#9,d1
	bne	@f
	subq.w	#1,a2
	lea	rem12(pc),a0
	bsr	wrt_str
	move.b	#9,(a2)+
@@:
	cmpi.b	#18,d1
	bne	@f
	subq.w	#1,a2
	lea	rem13(pc),a0
	bsr	wrt_str
	move.b	#9,(a2)+
@@:
	move.b	(a1)+,d0
	bsr	wrt_num2
	move.b	#',',(a2)+
	addq.b	#1,d1
	dbra	d2,tmblp
	move.b	#'}',-1(a2)
	bra	wrt_dsk
normal_u220tmb:
	move.l	12(sp),d3	*timbre number
	swap	d3
	movea.l	22(sp),a3	*name address
	movea.l	32(sp),a1
	moveq.l	#0,d2
	move.w	8(a1),d2	*添え字
	cmpi.w	#26-1,d2
	bne	m_err7		*配列のサイズが違う
	lea	10(a1),a1	*a1=data pointer
	moveq.l	#-1,d1
	tst.w	36(sp)
	bmi	@f
	move.l	42(sp),d1	*get dev id
@@:
	move.w	d1,d3		*set id to d3
	lea	env_work(pc),a0
	moveq.l	#0,d0
@@:
	addq.w	#1,d0
	move.b	(a3)+,(a0)+	*set timbre name
	bne	@b
	subq.w	#1,d0
	beq	m_err70
	cmpi.w	#12,d0
	bhi	m_err70
@@:
	move.b	(a1)+,(a0)+	*set parameters
	dbra	d2,@b
	lea	env_work(pc),a1	*a1=data address
	move.l	a0,d2
	sub.l	a1,d2
	Z_MUSIC	#$32
	tst.l	d0
	bne	error
	bra	ok

u220_dis:			*u220ドラムインストパラメータ設定
	bsr	error?
	move.b	out_flg(pc),d0
	beq	normal_u220dis
	move.l	12(sp),d1	*note number
	cmpi.l	#99,d1
	bhi	m_err70
	cmpi.l	#35,d1
	bcs	m_err70
	movea.l	22(sp),a1
	move.w	8(a1),d2	*添え字
	cmpi.w	#20-1,d2
	bhi	m_err7		*配列のサイズが違う
	lea	10(a1),a1	*a1=data pointer
	moveq.l	#-1,d3
	tst.w	26(sp)
	bmi	@f
	move.l	32(sp),d3	*get dev id
@@:
	lea	u220_dis_(pc),a0
	bsr	wrt_str
	move.l	d1,d0
	bsr	wrt_num		*note num
	move.l	d3,d0
	bmi	@f
	move.b	#',',(a2)+
	bsr	wrt_num3	*dev id
@@:
	move.b	#'=',(a2)+
	move.b	#'{',(a2)+
	moveq.l	#0,d0
@@:
	move.b	(a1)+,d0
	bsr	wrt_num
	move.b	#',',(a2)+
	dbra	d2,@b
	move.b	#'}',-1(a2)
	bra	wrt_dsk
normal_u220dis:
	move.l	12(sp),d3
	swap	d3
	movea.l	22(sp),a1
	moveq.l	#0,d2
	move.w	8(a1),d2	*添え字
	cmpi.w	#20-1,d2
	bhi	m_err7		*配列のサイズが違う
	addq.l	#1,d2		*make d2 size
	lea	10(a1),a1	*a1=data pointer
	moveq.l	#-1,d1
	tst.w	26(sp)
	bmi	@f
	move.l	32(sp),d1	*get dev id
@@:
	move.w	d1,d3
	Z_MUSIC	#$33
	tst.l	d0
	bne	error
	bra	ok

m1_mdch:			*Ｍ１・ＳＯＮＧ０のＭＩＤＩチャンネル設定
	bsr	error?
	move.b	out_flg(pc),d0
	beq	normal_m1mdch
	movea.l	12(sp),a1
	cmpi.w	#8-1,8(a1)
	bne	m_err7		*配列のサイズが違う
	lea	10(a1),a1	*a1=data pointer
	lea	m1_mdch_(pc),a0
	bsr	wrt_str
	moveq.l	#0,d0
	moveq.l	#8-1,d1
@@:
	move.b	(a1)+,d0
	bsr	wrt_num
	move.b	#',',(a2)+
	dbra	d1,@b
	move.b	#'}',-1(a2)
	bra	wrt_dsk
normal_m1mdch:
	movea.l	12(sp),a1
	cmpi.w	#8-1,8(a1)
	bne	m_err7		*配列のサイズが違う
	lea	10(a1),a1	*a1=data pointer
	Z_MUSIC	#$34
	tst.l	d0
	bne	error
	bra	ok

m1_ptst:			*Ｍ１・ＳＯＮＧ０のパートパラメータ設定
	bsr	error?
	move.b	out_flg(pc),d0
	beq	normal_m1ptst
	movea.l	12(sp),a1
	cmpi.w	#40-1,8(a1)
	bne	m_err7		*配列のサイズが違う
	lea	10(a1),a1	*a1=data pointer
	lea	rem14(pc),a0
	bsr	wrt_str
	move.b	#'1',(a2)+
	move.b	#9,(a2)+
	move.b	#9,(a2)+	*TABx2
	lea	rem15(pc),a0
	bsr	wrt_str
	lea	m1_ptst_(pc),a0
	bsr	wrt_str
	moveq.l	#0,d0
	moveq.l	#40-1,d1
	moveq.l	#2,d2
	moveq.l	#6,d3
m1ptst_lp:
	subq.b	#1,d3
	bne	@f
	moveq.l	#5,d3
	subq.w	#1,a2
	bsr	do_crlf
	lea	rem14(pc),a0
	bsr	wrt_str
	move.l	d2,d0
	bsr	wrt_num
	addq.b	#1,d2
	move.b	#9,(a2)+
	move.b	#9,(a2)+	*TABx2
	lea	rem15(pc),a0
	bsr	wrt_str
	move.b	#9,(a2)+
	move.b	#9,(a2)+	*TABx2
@@:
	move.b	(a1)+,d0
	bsr	wrt_num2
	move.b	#',',(a2)+
	dbra	d1,m1ptst_lp
	move.b	#'}',-1(a2)
	bra	wrt_dsk
normal_m1ptst:
	movea.l	12(sp),a1
	cmpi.w	#40-1,8(a1)
	bne	m_err7		*配列のサイズが違う
	lea	10(a1),a1	*a1=data pointer
	Z_MUSIC	#$36
	tst.l	d0
	bne	error
	bra	ok

m1_effect:			*Ｍ１・ＳＯＮＧ０のエフェクトパラメータ設定
	bsr	error?
	move.b	out_flg(pc),d0
	beq	normal_m1efct
	movea.l	12(sp),a1
	cmpi.w	#25-1,8(a1)
	bne	m_err7		*配列のサイズが違う
	lea	10(a1),a1	*a1=data pointer
	lea	m1_efct_(pc),a0
	bsr	wrt_str
	moveq.l	#0,d0
	moveq.l	#25-1,d1
@@:
	move.b	(a1)+,d0
	bsr	wrt_num
	move.b	#',',(a2)+
	dbra	d1,@b
	move.b	#'}',-1(a2)
	bra	wrt_dsk
normal_m1efct:
	movea.l	12(sp),a1
	cmpi.w	#25-1,8(a1)
	bne	m_err7		*配列のサイズが違う
	lea	10(a1),a1	*a1=data pointer
	Z_MUSIC	#$37
	tst.l	d0
	bne	error
	bra	ok

m1_prt:				*Ｍ１・ＳＯＮＧ０のソングネーム設定
	bsr	error?
	lea	m1_prt_(pc),a0
	tst.b	out_flg-m1_prt_(a0)
	beq	normal_m1prt
	bsr	wrt_str
	move.l	12(sp),a0
	tst.b	(a0)
	beq	m_err70		*length=0は駄目
	bsr	wrt_str
	move.b	#'"',(a2)+
	bra	wrt_dsk
normal_m1prt:
	move.l	12(sp),a1
	move.l	a1,a0
	moveq.l	#0,d2
@@:
	addq.w	#1,d2		*count length
	tst.b	(a0)+
	bne	@b
	subq.w	#1,d2
	beq	m_err70
	cmpi.w	#10,d2
	bhi	m_err70		*文字データ多すぎ
	Z_MUSIC	#$38
	tst.l	d0
	bne	error
	bra	ok

send_m1:			*Ｍ１へ送信
	bsr	error?
	move.b	out_flg(pc),d0
	beq	normal_sendm1
	moveq.l	#-1,d3
	tst.w	6(sp)
	bmi	@f
	move.l	12(sp),d3	*get dev id
@@:
	lea	send_m1_(pc),a0
	bsr	wrt_str
	move.l	d3,d0
	bmi	@f
	move.b	#' ',(a2)+
	bsr	wrt_num3	*dev id
	bra	wrt_dsk
normal_sendm1:
	moveq.l	#-1,d3
	tst.w	6(sp)
	bmi	@f
	move.l	12(sp),d3	*get dev id
@@:
	Z_MUSIC	#$35
	tst.l	d0
	bne	error
	bra	ok

zmd_play:			*ブロックデータの読み込み
	bsr	error?
	lea	filename(pc),a2
	tst.b	out_flg-filename(a2)
	bne	m_err10		*can't compile
	move.l	12(sp),a1	*a1=file name
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
	bmi	m_err62		*read error
	bsr	read		*return=a5:address,d3:size
	moveq.l	#0,d2		*ascii mode
	movea.l	a5,a1		*address
	cmp.l	#$105a6d75,(a1)+
	bne	m_err66
	cmp.w	#'Si',(a1)+
	bne	m_err66
	cmp.b	#'C',(a1)+
	bne	m_err66
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
	bra	ok

jump_flg:			*[!]コマンドの有効/無効化
	bsr	error?
	move.b	out_flg(pc),d0
	beq	@f
	move.w	#'(d',(a2)+
	move.l	12(sp),d0
	bsr	wrt_num
	move.b	#')',(a2)+
	bra	wrt_dsk
@@:
	move.l	12(sp),d2
	Z_MUSIC	#$41
	tst.l	d0
	bne	error
	bra	ok

set_mclk:			*全音符の絶対音長の設定
	bsr	error?
	move.b	out_flg(pc),d0
	beq	@f
	move.w	#'(z',(a2)+
	move.l	12(sp),d0
	bsr	wrt_num
	move.b	#')',(a2)+
	bra	wrt_dsk
@@:
	move.l	12(sp),d2
	Z_MUSIC	#$42
	tst.l	d0
	bne	error
	bra	ok

fm_master:			*FM音源の出力バランス設定
	bsr	error?
	lea	fm_mstr_(pc),a0
	tst.b	out_flg-fm_mstr_(a0)
	beq	@f
	bsr	wrt_str
	move.l	12(sp),d0
	bsr	wrt_num
	bra	wrt_dsk
@@:
	move.l	12(sp),d2
	Z_MUSIC	#$3e
	tst.l	d0
	bne	error
	bra	ok

m_mute:
	bsr	error?
	move.b	out_flg(pc),d0
	bne	m_err10		*コンパイル出来ません
	bsr	bit_pat_set
	Z_MUSIC	#$44
	tst.l	d0
	bne	error
	bra	ok

m_solo:
	bsr	error?
	move.b	out_flg(pc),d0
	bne	m_err10		*コンパイル出来ません
	bsr	bit_pat_set
	tst.l	d2
	beq	@f
	not.l	d2
@@:
	Z_MUSIC	#$44
	tst.l	d0
	bne	error
	bra	ok

m_wave_form:			*波形メモリセット
	bsr	error?
	lea	wvfm_(pc),a0
	tst.b	out_flg-wvfm_(a0)
	beq	mwf
	bsr	wrt_str
	move.l	12(sp),d0	*波形番号
	bsr	wrt_num
	move.b	#',',(a2)+
	move.l	22(sp),d0	*ループタイプ
	bsr	wrt_num
	moveq.l	#0,d0
	tst.w	par3(sp)
	bmi	@f
	move.b	#',',(a2)+
	move.l	32(sp),d0	*ループポイント
@@:
	move.l	42(sp),a1
	addq.w	#8,a1
	move.w	(a1)+,d2	*添え字をゲット
	addq.w	#1,d2		*データカウントにする
	bsr	wrt_num		*write loop point
	move.b	#'=',(a2)+
	move.b	#'{',(a2)+
wvf_lp01:
	move.l	(a1)+,d0
	bsr	wrt_num
	subq.w	#1,d2
	beq	mwf_owari
	move.b	#',',(a2)+
	move.l	a2,d1
	sub.l	str_buffer(pc),d1
	cmp.l	#78,d1
	bls	wvf_lp01
	subq.l	#1,a2		*delete ','
	bsr	do_crlf
	movea.l	str_buffer(pc),a0
	suba.l	a0,a2
	pea	(a2)
	pea	(a0)
	move.w	sv_fh(pc),-(sp)
	DOS	_WRITE
	lea	10(sp),sp
	tst.l	d0
	bmi	m_err71
	movea.l	str_buffer(pc),a2
	move.b	#9,(a2)+
	move.b	#9,(a2)+
	bra	wvf_lp01
mwf_owari:
	move.b	#'}',(a2)+
	bra	wrt_dsk
mwf:				*コマンド実行処理
	move.l	12(sp),d3	*wave number
	lsl.w	#8,d3
	move.l	22(sp),d1	*loop type
	move.b	d1,d3
	swap	d3
	moveq.l	#0,d1
	tst.w	par3(sp)
	bmi	@f
	move.l	32(sp),d1	*loop point
@@:
	move.w	d1,d3
	movea.l	42(sp),a1	*配列
	addq.w	#8,a1
	move.w	(a1)+,d2	*添え字をゲット
	addq.w	#1,d2		*データカウントにする

	move.l	a1,a0		*配列加工
	move.l	a1,a2
	move.l	d2,d1
	subq.w	#1,d1
@@:
	move.l	(a0)+,d0
	move.w	d0,(a2)+
	dbra	d1,@b

	Z_MUSIC	#$4a
	tst.l	d0
	bne	error
	bra	ok

sc55_init:			*SC55初期化
	bsr	error?
	move.b	out_flg(pc),d0
	beq	normal_initsc55
	moveq.l	#-1,d3
	tst.w	6(sp)
	bmi	@f
	move.l	12(sp),d3	*get dev id
@@:
	lea	sc55_init_(pc),a0
	bsr	wrt_str
	move.l	d3,d0
	bmi	wrt_dsk
	move.b	#' ',(a2)+
	bsr	wrt_num3	*dev id
	bra	wrt_dsk
normal_initsc55:
	moveq.l	#-1,d3
	tst.w	6(sp)
	bmi	@f
	move.l	12(sp),d3	*get dev id
@@:
	Z_MUSIC	#$51
	tst.l	d0
	bne	error
	bra	ok

mt32_init:			*MT32初期化
	bsr	error?
	move.b	out_flg(pc),d0
	beq	normal_initmt32
	moveq.l	#-1,d3
	tst.w	6(sp)
	bmi	@f
	move.l	12(sp),d3	*get dev id
@@:
	lea	mt32_init_(pc),a0
	bsr	wrt_str
	move.l	d3,d0
	bmi	wrt_dsk
	move.b	#' ',(a2)+
	bsr	wrt_num3	*dev id
	bra	wrt_dsk
normal_initmt32:
	moveq.l	#-1,d3
	tst.w	6(sp)
	bmi	@f
	move.l	12(sp),d3	*get dev id
@@:
	Z_MUSIC	#$52
	tst.l	d0
	bne	error
	bra	ok

adpcm_to_pcm:
	bsr	error?
	move.b	out_flg(pc),d0
	bne	m_err10		*can't compile
	movea.l	12(sp),a0	*配列
	moveq.l	#0,d0
	move.w	8(a0),d0
	addq.l	#1,d0		*size
	lea	10(a0),a0
	tst.w	16(sp)
	bmi	@f		*省略のケース
	move.l	22(sp),d1	*get size
	beq	m_err70		*illegal parameter
	cmp.l	d0,d1
	bhi	m_err70		*サイズが添え字より大きい!?
	move.l	d1,d0
@@:
	movea.l	32(sp),a1	*配列
	moveq.l	#0,d1
	move.w	8(a1),d1
	addq.l	#1,d1		*size
	lea	10(a1),a1
	move.l	d0,d2
	lsl.l	#1,d2
	cmp.l	d2,d1
	bcs	m_err70
	movem.l	d0-d7/a0-a6,-(sp)
	bsr	just_adpcm_to_pcm
	movem.l	(sp)+,d0-d7/a0-a6
	bra	ok

pcm_to_adpcm:
	bsr	error?
	move.b	out_flg(pc),d0
	bne	m_err10		*can't compile
	movea.l	12(sp),a1	*配列
	moveq.l	#0,d0
	move.w	8(a1),d0
	addq.l	#1,d0		*size
	lea	10(a1),a1
	tst.w	16(sp)
	bmi	@f		*省略のケース
	move.l	22(sp),d1	*get size
	beq	m_err70		*illegal parameter
	cmp.l	d0,d1
	bhi	m_err70		*サイズが添え字より大きい!?
	move.l	d1,d0
@@:
	movea.l	32(sp),a0	*配列
	moveq.l	#0,d1
	move.w	8(a0),d1
	addq.l	#1,d1		*size
	lea	10(a0),a0
	move.l	d0,d2
	lsr.l	#1,d2
	cmp.l	d2,d1
	bcs	m_err70
	movem.l	d0-d7/a0-a6,-(sp)
	bsr	pcm_to_adpcm_
	movem.l	(sp)+,d0-d7/a0-a6
	bra	ok

exec_zms:			*ＺＭＳコマンド実行
	bsr	error?
	move.b	out_flg(pc),d0
	beq	@f
	move.l	12(sp),a0	*そのまま書き出す
	bsr	wrt_str
	bra	wrt_dsk
@@:
	move.l	12(sp),a1
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
	bra	ok

m_inp:				*ＭＩＤＩデータ入力
	bsr	error?
	move.b	out_flg(pc),d0
	bne	m_err10		*can't compile
	moveq.l	#0,d2
	tst.w	6(sp)		*省略?
	bmi	@f
	move.l	12(sp),d2
@@:
	Z_MUSIC	#$55
	bra	ok_ret

zm_ver:				*ZMUSICバージョン
	bsr	error?
	lea	work(pc),a0
	tst.b	out_flg-work(a0)
	bne	m_err10		*can't compile
	moveq.l	#0,d0
	tst.b	pcm8_flg-work(a0)
	beq	@f
	moveq.l	#-1,d0
@@:
	move.w	zm_ver_buf(pc),d0
	bra	ok_ret

m_trk2:				*トラック書き込み #2
	bsr	error?
	move.b	out_flg(pc),d0
	beq	normal_m_trk2
	move.w	#'(t',(a2)+
	move.l	12(sp),a1	*mml str addres
	lea.l	16(sp),a3
	moveq.l	#8-1,d3
mt2_lp:
	tst.w	(a3)+
	bmi	end_mt2l
	addq.w	#4,a3
	move.l	(a3)+,d0
	cmpi.b	#8-1,d3
	beq	@f
	move.b	#',',(a2)+
@@:
	bsr	wrt_num
	dbra	d3,mt2_lp
end_mt2l:
	move.b	#')',(a2)+
@@:
	move.b	(a1)+,(a2)+
	bne	@b
	subq.w	#1,a2
	bra	wrt_dsk
normal_m_trk2:
	movea.l	12(sp),a1	*get str address
	lea.l	16(sp),a3
	moveq.l	#8-1,d3
@@:
	tst.w	(a3)+
	bmi	ok
	addq.w	#4,a3
	move.l	(a3)+,d2	*get trk
	Z_MUSIC	#$06
	tst.l	d0
	bne	error
	dbra	d3,@b
	bra	ok

zm_work:
	bsr	error?
	move.b	out_flg(pc),d0
	bne	m_err10		*can't compile
	move.l	12(sp),d2
	Z_MUSIC	#$3c
	move.l	22(sp),d2
	moveq.l	#0,d0
	move.b	(a0,d2.l),d0
	bra	ok_ret

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
	move.l	d2,(a1)+	*add pcm data to buffer
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

pcm_to_adpcm_:			*ＰＣＭデータをＡＤＰＣＭデータへ変換する
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
	move.l	(a1)+,d3	*d3=pcm data

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

do_crlf:
	move.b	#13,(a2)+
	move.b	#10,(a2)+
	rts

wrt_str:			*文字データのバッファへの書き込み
	* < a0=source str pointer
	* < a2=destination str pointer
	* - all
	move.b	(a0)+,(a2)+
	bne	wrt_str
	subq.w	#1,a2
	rts

bit_pat_set:			*ビットパターンを作る
	moveq.l	#4,d1		*offset(bsrの時点でspが-4になっているから)
	moveq.l	#0,d2
	moveq.l	#0,d3
	moveq.l	#0,d4
	moveq.l	#10-1,d5	*basicのパラメータマックスが１０個のため
bps_lp:
	tst.w	6(sp,d1.w)	*省略?
	bmi	next_bps
	move.l	12(sp,d1.w),d0	*get trk number
	subq.b	#1,d0
	cmpi.b	#tr_max-1,d0
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
	add.w	#10,d1		*次へ
	dbra	d5,bps_lp
	rts

wrt_chs:			*チャンネル番号を書く
	* < a2=str_buffer
	moveq.l	#0,d1
	moveq.l	#0,d2
	moveq.l	#10-1,d3	*basicのパラメータマックスが１０個のため
	moveq.l	#0,d4		*reset marker
chs_lp:
	tst.w	6(sp,d1.w)	*省略?
	bmi	next_chs
	move.l	12(sp,d1.w),d0	*get trk number
	st.b	d4		*mark
	bsr	wrt_num
	move.b	#',',(a2)+
next_chs:
	add.w	#10,d1		*次へ
	dbra	d3,chs_lp
	tst.b	d4		*check marker
	beq	@f
	subq.w	#1,a2		*最後の','を抹殺
@@:
	move.b	#')',(a2)+
	bra	wrt_dsk

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
	move.l	env_bak(pc),a1	*環境変数文字列群
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
	bra	m_err72		*file size=0
@@:
	move.l	d0,-(sp)
	move.w	#2,-(sp)
	DOS	_V2_MALLOC2
	addq.w	#6,sp
	tst.l	d0
	bpl	@f
	addq.w	#4,sp
	bra	m_err5		*OUT OF MEMORY
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
	bra	m_err62		*読み込み失敗
@@:
	move.w	d5,-(sp)	*close
	DOS	_CLOSE
	addq.l	#2,sp
	rts

wrt_dsk:			*ファイル出力
	bsr	do_crlf
	movea.l	str_buffer(pc),a0
	suba.l	a0,a2
	pea	(a2)
	pea	(a0)
	move.w	sv_fh(pc),-(sp)
	DOS	_WRITE
	lea	10(sp),sp
	tst.l	d0
	bmi	m_err71
	bra	ok

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

wrt_num:			*d0の値を文字数列に変換しバッファへ書き込む
	* < d0.l=value
	* < a2.l=buffer address
	* - all
	movem.l	d1-d4/a0-a1,-(sp)
	clr.b	d4
	move.l	a2,a0
	lea	exp_tbl(pc),a1
	moveq.l	#10-1,d1
ex_loop0:
	moveq.l	#0,d2
	move.l	(a1)+,d3
ex_loop1:
	or	d3,d3		*clr carry
	sub.l	d3,d0
	bcs	xbcd_str
	addq.b	#1,d2
	bra	ex_loop1
xbcd_str:
	add.l	d3,d0
	tst.b	d2
	bne	nml_ktset
	tst.b	d4
	beq	nml_lp_ope
nml_ktset:
	st	d4
	add.b	#'0',d2
	move.b	d2,(a2)+
nml_lp_ope:
	dbra	d1,ex_loop0
	cmpa.l	a0,a2
	bne	set_suji_end
	move.b	#'0',(a2)+
set_suji_end:
	movem.l	(sp)+,d1-d4/a0-a1
	rts

wrt_num2:			*d0の値を文字数列に変換しバッファへ書き込む
	* < d0.l=value		*3桁固定
	* < a2.l=buffer address
	* - all
	movem.l	d1-d4/a0-a1,-(sp)
	clr.b	d4
	move.l	a2,a0
	lea	exp_tbl+28(pc),a1
	moveq.l	#3-1,d1
_ex_loop0:
	moveq.l	#0,d2
	move.l	(a1)+,d3
_ex_loop1:
	or	d3,d3		*clr carry
	sub.l	d3,d0
	bcs	_xbcd_str
	addq.b	#1,d2
	bra	_ex_loop1
_xbcd_str:
	add.l	d3,d0
	tst.b	d2
	bne	_nml_ktset
	tst.b	d4
	bne	_nml_ktset
	move.b	#$20,(a2)+
	bra	_nml_lp_ope
_nml_ktset:
	st	d4
	add.b	#'0',d2
	move.b	d2,(a2)+
_nml_lp_ope:
	dbra	d1,_ex_loop0
	tst.b	d4
	bne	_set_suji_end
	move.b	#'0',-1(a2)
_set_suji_end:
	movem.l	(sp)+,d1-d4/a0-a1
	rts

wrt_num3:			*16進数文字列(2bytes)で書き込み
	* < d0=data value
	* < a2=格納したいアドレス
	* - all
	move.l	d1,-(sp)
	move.b	#'$',(a2)+
	move.b	d0,d1
	lsr.b	#4,d1
	add.b	#$30,d1
	cmpi.b	#'9',d1
	bls	its_hex8
	addq.b	#7,d1
its_hex8:
	move.b	d1,(a2)+

	andi.b	#$0f,d0
	add.b	#$30,d0
	cmpi.b	#'9',d0
	bls	its_hex8_
	addq.b	#7,d0
its_hex8_:
	move.b	d0,(a2)+
	move.l	(sp)+,d1
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

error?:				*ドライバ状態チェック
	lea	out_flg(pc),a6
	tst.b	(a6)		*ジェネレートモード?
	beq	@f
	movea.l	str_buffer(pc),a2
	tst.w	sv_fh-out_flg(a6)	*ファイルハンドル検査
	bpl	exit_err?
	moveq.l	#71,d0		*ファイル出力失敗
	addq.w	#4,sp
	bra	error
@@:
	tst.b	err_flg-out_flg(a6)
	beq	exit_err?
	bmi	teishi
	moveq.l	#3,d0		*ドライバ未登録
	addq.w	#4,sp		*スタック補正
	bra	error
exit_err?:
	rts
teishi:
	moveq.l	#8,d0		*ドライバ停止中
	addq.w	#4,sp		*スタック補正
	bra	error

ok:				*戻り値無しでリータン
	moveq.l	#0,d0
ok_ret:				*戻り値有りでリータン
	lea	ret_buf(pc),a0	*戻り値ポインタ設定
	move.l	d0,6(a0)	*戻り値書き込み(low long word)
	moveq.l	#0,d0
	rts

m_err1:
	moveq.l	#1,d0		*省略不可
	bra	error
m_err4:
	moveq.l	#4,d0		*トラックバッファのサイズが異常
	bra	error
m_err5:
	moveq.l	#5,d0		*out of memory
	bra	error
m_err6:
	moveq.l	#6,d0		*チャンネル番号が異常
	bra	error
m_err7:
	moveq.l	#7,d0		*配列の型が違う
	bra	error
m_err10:
	moveq.l	#10,d0		*コンパイル出来ません
	bra	error
m_err62:
	moveq.l	#62,d0		*read error
	bra	error
m_err66:
	moveq.l	#66,d0		*illegal version number
	bra	error
m_err70:
	moveq.l	#70,d0		*パラメータが異常
	bra	error
m_err71:
	moveq.l	#71,d0		*ファイルの書き込みに失敗
	bra	error
m_err72:
	moveq.l	#72,d0		*ファイルサイズが異常です
	bra	error

max_err:	equ	(emte-err_mes_tbl)/2
error:				*エラー発生時の処理
	lea	ret_buf(pc),a0		*戻り値ポインタ
	clr.b	running-ret_buf(a0)
	move.l	d0,-(sp)
	cmpi.l	#max_err-1,d0
	bls	@f
	moveq.l	#65,d0		*unknown(=65)
@@:
	add.w	d0,d0
	move.w	err_mes_tbl(pc,d0.w),d0	*エラーメッセージポインタ
	lea	err_mes_tbl(pc,d0.w),a1
	move.l	#-1,6(a0)
	move.l	(sp)+,d0
	rts

err_mes_tbl:
	dc.w	err_mes65-err_mes_tbl	*0
	dc.w	err_mes1-err_mes_tbl	*1
	dc.w	err_mes2-err_mes_tbl	*2
	dc.w	err_mes3-err_mes_tbl	*3
	dc.w	err_mes4-err_mes_tbl	*4
	dc.w	err_mes5-err_mes_tbl	*5
	dc.w	err_mes6-err_mes_tbl	*6
	dc.w	err_mes7-err_mes_tbl	*7
	dc.w	err_mes8-err_mes_tbl	*8
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
	dc.w	err_mes20-err_mes_tbl	*20
	dc.w	err_mes21-err_mes_tbl	*21
	dc.w	err_mes22-err_mes_tbl	*22
	dc.w	err_mes23-err_mes_tbl	*23
	dc.w	err_mes24-err_mes_tbl	*24
	dc.w	err_mes25-err_mes_tbl	*25
	dc.w	err_mes26-err_mes_tbl	*26
	dc.w	err_mes27-err_mes_tbl	*27
	dc.w	err_mes28-err_mes_tbl	*28
	dc.w	err_mes29-err_mes_tbl	*29
	dc.w	err_mes30-err_mes_tbl	*30
	dc.w	err_mes31-err_mes_tbl	*31
	dc.w	err_mes32-err_mes_tbl	*32
	dc.w	err_mes33-err_mes_tbl	*33
	dc.w	err_mes34-err_mes_tbl	*34
	dc.w	err_mes35-err_mes_tbl	*35
	dc.w	err_mes36-err_mes_tbl	*36
	dc.w	err_mes37-err_mes_tbl	*37
	dc.w	err_mes38-err_mes_tbl	*38
	dc.w	err_mes39-err_mes_tbl	*39
	dc.w	err_mes40-err_mes_tbl	*40
	dc.w	err_mes41-err_mes_tbl	*41
	dc.w	err_mes42-err_mes_tbl	*42
	dc.w	err_mes43-err_mes_tbl	*43
	dc.w	err_mes44-err_mes_tbl	*44
	dc.w	err_mes45-err_mes_tbl	*45
	dc.w	err_mes46-err_mes_tbl	*46
	dc.w	err_mes47-err_mes_tbl	*47
	dc.w	err_mes48-err_mes_tbl	*48
	dc.w	err_mes49-err_mes_tbl	*49
	dc.w	err_mes50-err_mes_tbl	*50
	dc.w	err_mes51-err_mes_tbl	*51
	dc.w	err_mes52-err_mes_tbl	*52
	dc.w	err_mes53-err_mes_tbl	*53
	dc.w	err_mes54-err_mes_tbl	*54
	dc.w	err_mes55-err_mes_tbl	*55
	dc.w	err_mes56-err_mes_tbl	*56
	dc.w	err_mes57-err_mes_tbl	*57
	dc.w	err_mes58-err_mes_tbl	*58
	dc.w	err_mes59-err_mes_tbl	*59
	dc.w	err_mes60-err_mes_tbl	*60
	dc.w	err_mes61-err_mes_tbl	*61
	dc.w	err_mes62-err_mes_tbl	*62
	dc.w	err_mes63-err_mes_tbl	*63
	dc.w	err_mes64-err_mes_tbl	*64
	dc.w	err_mes65-err_mes_tbl	*65
	dc.w	err_mes66-err_mes_tbl	*66
	dc.w	err_mes67-err_mes_tbl	*67
	dc.w	err_mes68-err_mes_tbl	*68
	dc.w	err_mes69-err_mes_tbl	*69
	dc.w	err_mes70-err_mes_tbl	*70
	dc.w	err_mes71-err_mes_tbl	*71
	dc.w	err_mes72-err_mes_tbl	*72
	dc.w	err_mes73-err_mes_tbl	*73
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
emte:

err_mes1:	dc.b	'パラメータの省略は出来ません',0
err_mes2:	dc.b	'トラック番号が規定外です',0
err_mes3:	dc.b	'ＺＭＵＳＩＣ．Ｘが未登録です',0
err_mes4:	dc.b	'トラックバッファのサイズの指定が規定外です',0
err_mes5:	dc.b	'トラックバッファの確保が出来ません',0
err_mes6:	dc.b	'チャンネル番号が規定外です',0
err_mes7:	dc.b	'配列の型が規定外です',0
err_mes8:	dc.b	'ＺＭＵＳＩＣ．Ｘは現在動作を停止中です',0
err_mes9:	dc.b	'Ｉコマンドの使用法に誤りがあります',0
err_mes10:	dc.b	'この命令はコンパイル出来ません',0
err_mes11:	dc.b	'＠Ｘコマンドの使用法に誤りがあります',0
err_mes12:	dc.b	'＠Ｉコマンドの使用法に誤りがあります',0
err_mes13:	dc.b	'相対ボリュームの指定に誤りがあります',0
err_mes14:	dc.b	'＠Ｃコマンドの使用法に誤りがあります',0
err_mes15:	dc.b	'＠Ａコマンドの使用法に誤りがあります',0
err_mes16:	dc.b	'内蔵音源には無関係のコマンドです',0
err_mes17:	dc.b	'＠Ｈコマンドの使用法に誤りがあります',0
err_mes18:	dc.b	'＠Ｓコマンドの使用法に誤りがあります',0
err_mes19:	dc.b	'未定義のＭＭＬです',0
err_mes20:	dc.b	'未定義の［～］コマンドです',0
err_mes21:	dc.b	'］がありません',0
err_mes22:	dc.b	'繰り返し回数が規定外です',0
err_mes23:	dc.b	'繰り返し構造が異常です',0
err_mes24:	dc.b	'繰り返し番号が規定外です',0
err_mes25:	dc.b	'オクターブ値がありません',0
err_mes26:	dc.b	'オクターブ値が規定外です',0
err_mes27:	dc.b	'音長が規定外です',0
err_mes28:	dc.b	'トラックバッファが不足しています',0
err_mes29:	dc.b	'＠Ｂコマンドの使用法に誤りがあります',0
err_mes30:	dc.b	'ＡＤＰＣＭには無関係のコマンドです',0
err_mes31:	dc.b	'タイの指定方法に誤りがあります',0
err_mes32:	dc.b	'Ｔコマンドの使用法に誤りがあります',0
err_mes33:	dc.b	'＠Ｔコマンドの使用法に誤りがあります',0
err_mes34:	dc.b	'音長値がありません',0
err_mes35:	dc.b	'音量値がありません',0
err_mes36:	dc.b	'音量値が規定外です',0
err_mes37:	dc.b	'＠Ｋコマンドの使用法に誤りがあります',0
err_mes38:	dc.b	'キーコード／ノート番号の値が規定外です',0
err_mes39:	dc.b	'音色番号が規定外です',0
err_mes40:	dc.b	'｝がありません',0
err_mes41:	dc.b	'｛～｝内に音符が多すぎます',0
err_mes42:	dc.b	'｛～｝内に音符がありません',0
err_mes43:	dc.b	'Ｑ／＠Ｑコマンドの使用法に誤りがあります',0
err_mes44:	dc.b	'｛～｝の中に規定外のものが存在します',0
err_mes45:	dc.b	'Ｙコマンドの使用法に誤りがあります',0
err_mes46:	dc.b	'Ｊコマンドの使用法に誤りがあります',0
err_mes47:	dc.b	'Ｐ／＠Ｐコマンドの使用法に誤りがあります',0
err_mes48:	dc.b	'Ｋコマンドの使用法に誤りがあります',0
err_mes49:	dc.b	'和音コマンドの使用法に誤りがあります',0
err_mes50:	dc.b	"和音の数が多すぎるか、あるいは後ろの’がありません",0
err_mes51:	dc.b	'＠Ｖコマンドの使用法に誤りがあります',0
err_mes52:	dc.b	'ポルタメントコマンドの使用法に誤りがあります',0
err_mes53:	dc.b	'ベロシティの値が規定外です',0
err_mes54:	dc.b	'Ｎ／＠Ｎコマンドの使用法に誤りがあります',0
err_mes55:	dc.b	'＠Ｍコマンドの使用法に誤りがあります',0
err_mes56:	dc.b	'Ｈコマンドの使用法に誤りがあります',0
err_mes57:	dc.b	'＠Ｚコマンドの使用法に誤りがあります',0
err_mes58:	dc.b	'パラメータの指定方法が規定外です',0
err_mes59:	dc.b	'ディレイの値が長すぎます',0
err_mes60:	dc.b	'ＡＤＰＣＭ用のバッファが不足しています',0
err_mes61:	dc.b	'ＡＤＰＣＭの使用は出来ません',0
err_mes62:	dc.b	'ディスクの読み込みに失敗しました',0
err_mes63:	dc.b	'Ｗコマンドの使用法に誤りがあります',0
err_mes64:	dc.b	'ワークエリアが不足しています',0
err_mes65:	dc.b	'正体不明のエラーが発生しました',0
err_mes66:	dc.b	'バージョンナンバーが違うか、あるいはZMUSIC用のデータではありません',0
err_mes67:	dc.b	'ＡＤＰＣＭ音色定義コマンドの使用法に誤りがあります',0
err_mes68:	dc.b	'ＭＩＤＩインターフェースボードが未装着です',0
err_mes69:	dc.b	'ＭＩＤＩ楽器に対するパラメータが規定外です',0
err_mes70:	dc.b	'パラメータ値が規定外です',0
err_mes71:	dc.b	'ファイルの書き出しに失敗しました',0
err_mes72:	dc.b	'Ｘコマンドの使用法に誤りがあります',0
err_mes73:	dc.b	'データは受信されていません',0
err_mes74:	dc.b	'波形番号が規定外です',0
err_mes75:	dc.b	'Ｍコマンドの使用法に誤りがあります',0
err_mes76:	dc.b	'波形メモリ登録コマンドの使用法に誤りがあります',0
err_mes77:	dc.b	'；コマンドの使用法に誤りがあります',0
err_mes78:	dc.b	'￥コマンドの使用法に誤りがあります',0
err_mes79:	dc.b	'？コマンドの使用法に誤りがあります',0
err_mes80:	dc.b	'＠Ｆコマンドの使用法に誤りがあります',0
err_mes81:	dc.b	'＠Ｇコマンドの使用法に誤りがあります',0
err_mes82:	dc.b	'＠Ｙコマンドの使用法に誤りがあります',0
err_mes83:	dc.b	'＠Ｅコマンドの使用法に誤りがあります',0
	.even

	if	dbg<>0

get_hex32:			*値→16進数文字列(4bytes)
	* < d0=data value
	* < a1=格納したいアドレス
	* > (a1)=ascii numbers
	* - all
	movem.l	d0-d1/d4/a1,-(sp)
	addq.w	#8,a1
	clr.b	(a1)
	moveq.l	#8-1,d4
gh_lp32:
	move.b	d0,d1
	andi.b	#$0f,d1
	add.b	#$30,d1
	cmpi.b	#'9',d1
	bls	its_hex32
	addq.b	#7,d1
its_hex32:
	move.b	d1,-(a1)
	lsr.l	#4,d0
	dbra	d4,gh_lp32
	movem.l	(sp)+,d0-d1/d4/a1
	rts

debug2:				*デバグ用ルーチン(レジスタ値を表示／割り込み対応)
*	move.w	sr,db_work2	*save sr	(サブルーチンget_hex32が必要)
*	andi.w	#$f8ff,sr	*int on
	movem.l	d0-d7/a0-a7,db_work
	move.l	(a7),pc_work

	moveq.l	#%0011,d1
	IOCS	_B_COLOR

	lea	CRLF_(PC),a1
	IOCS	_B_PRINT

	moveq.l	#8-1,d7
	lea	db_work(pc),a6
dbg2_lp01:
	move.l	(a6)+,d0
	lea	suji(pc),a1
	bsr	get_hex32
	lea	suji(pc),a1
	IOCS	_B_PRINT
	lea	SPC_(pc),a1
	IOCS	_B_PRINT
	dbra	d7,dbg2_lp01

	lea	CRLF_(PC),a1
	IOCS	_B_PRINT

	moveq.l	#8-1,d7
dbg2_lp02:
	move.l	(a6)+,d0
	lea	suji(pc),a1
	bsr	get_hex32
	lea	suji(pc),a1
	IOCS	_B_PRINT
	lea	SPC_(pc),a1
	IOCS	_B_PRINT
	dbra	d7,dbg2_lp02

	lea	SPC_(pc),a1
	IOCS	_B_PRINT

	lea	suji(pc),a1	*PC
	move.l	pc_work(pc),d0
	bsr	get_hex32
	lea	suji(pc),a1
	IOCS	_B_PRINT

	lea	push_any(PC),a1
	IOCS	_B_PRINT
klop:
	IOCS	_B_KEYINP
	tst.l	d0
	beq	klop

	movem.l	db_work(pc),d0-d7/a0-a7
*	move.w	db_work2,sr	*get back sr
	rts
		*デバッグ用ワーク
push_any:
	dc.b	13,10,$1b,'[47mPUSH ANY KEY',$1b,'[33m',13,10,0
baka:
	dc.b	'ここまでＯＫざんす/',0		*for debug message
baka2:
	dc.b	'えらーざんすよ～!!/',0		*for debug message
debug_wk:	dc.b   256-8		*for debug
SPC_:	dc.b	' ',0
CRLF_:	dc.b	13,10,0
	.even
	dc.b	'REGI'
db_work:		dcb.l	16,0		*for debug
db_work2:		dc.l	0
pc_work:		dc.l	0
	endif

	.data
work:
dev_name:	.dc.b	'OPM     '
dev_name2:	.dc.b	'MIDI    '
getname:	.dc.b	'zmusic',0,0
OPM:		.dc.b	'OPM',0
CRLF:		dc.b	13,10,0
dflt_fn:	.dc.b	'ZMUSIC.ZMS',0
rem1:	dc.b	"/",9," AF  OM  WF  SY  SP PMD AMD PMS AMS PAN",13,10,0
rem2:	dc.b	"/",9," AR  DR  SR  RR  SL  OL  KS  ML DT1 DT2 AME",13,10,0
rem3:	dc.b	9,"/",9," AL  FB  OM PAN  WF  SY  SP PMD AMD PMS AMS",13,10,0
rem4:	dc.b	"/  WG ",9," PC  PF PKF PBS WFM PCM PLW PWVS",13,10,0
rem5:	dc.b	13,10,"/ P-ENV",9,"DPT VLS TMK TM1 TM2 TM3 TM4 LV0 LV1 LV2 STL EDL",13,10,0 
rem6:	dc.b	13,10,"/ P-LFO",9," RT DPT MDS",13,10,0
rem7:	dc.b	13,10,"/  TVF ",9,"CUT RES KYF BSP BSL",13,10,0
rem8:	dc.b	13,10,"/TVFENV",9,"DPT VLS DPT TMK TM1 TM2 TM3 TM4 TM5 LV1 LV2 LV3 STL",13,10,0
rem9:	dc.b	13,10,"/  TVA ",9,"LVL VLS BP1 BL1 BP2 BL2",13,10,0
rem10:	dc.b	13,10,"/TVAENV",9,"TMK TMV TM1 TM2 TM3 TM4 TM5 LV1 LV2 LV3 STL",13,10,0
rem11:	dc.b	"/",9,"TMD TMN TML VLS CHP EAR EDR ESL ERR",13,10,0
rem12:	dc.b	13,10,"/",9,"PSC PSF BRL BRU CHA PAS ABD ABR DTD",13,10,0
rem13:	dc.b	13,10,"/",9,"VRT WFM DPT DLY RST MDP CHA PAS",13,10,0
rem14:	dc.b	"/  Tr",0
rem15:	dc.b	"PRG VOL KTR DTN PAN",13,10,0
i_data:	dc.b	$1a
fmvset:		dc.b	'.FM_VSET ',0
midi_dump:	dc.b	'.MIDI_DUMP=',0
midi_data:	dc.b	'.MIDI_DATA={',0
exclusive:	dc.b	'.EXCLUSIVE={',0
roland:		dc.b	'.ROLAND_EXCLUSIVE ',0
adpcm_list:	dc.b	'.ADPCM_LIST=',0
sc55_vr_:	dc.b	'.SC55_V_RESERVE ',0
sc55_rvb_:	dc.b	'.SC55_REVERB ',0
sc55_cho_:	dc.b	'.SC55_CHORUS ',0
sc55_pst_:	dc.b	'.SC55_PART_SETUP ',0
sc55_dst_:	dc.b	'.SC55_DRUM_SETUP ',0
sc55_prt_:	dc.b	'.SC55_PRINT ',0
sc55_dsp_:	dc.b	'.SC55_DISPLAY ',0
adpcm_block_:	dc.b	'.ADPCM_BLOCK_DATA=',0
mt32_pr_:	dc.b	'.MT32_P_RESERVE ',0
mt32_rvb_:	dc.b	'.MT32_REVERB ',0
mt32_pst_:	dc.b	'.MT32_PART_SETUP ',0
mt32_dst_:	dc.b	'.MT32_DRUM_SETUP ',0
mt32_cmn_:	dc.b	'.MT32_COMMON ',0
mt32_ptch_:	dc.b	'.MT32_PATCH ',0
mt32_prtl_:	dc.b	'.MT32_PARTIAL ',0
mt32_prt_:	dc.b	'.MT32_PRINT ',0
u220_prt_:	dc.b	'.U220_PRINT ',0
m_prt_:		dc.b	'.PRINT ',0
u220_setup_:	dc.b	'.U220_SETUP ',0
u220_cmn_:	dc.b	'.U220_COMMON ',0
u220_dst_:	dc.b	'.U220_DRUM_SETUP ',0
u220_pst_:	dc.b	'.U220_PART_SETUP ',0
u220_tmb_:	dc.b	'.U220_TIMBRE ',0
u220_dis_:	dc.b	'.U220_DRUM_INST ',0
m1_mdch_:	dc.b	'.M1_MIDI_CH={',0
m1_ptst_:	dc.b	'.M1_PART_SETUP={',0
m1_efct_:	dc.b	'.M1_EFFECT_SETUP={',0
m1_prt_:	dc.b	'.M1_PRINT "',0
send_m1_:	dc.b	'.SEND_TO_M1 ',0
fm_mstr_:	dc.b	'.FM_MASTER_VOLUME=',0
wvfm_:		dc.b	'.WAVE_FORM ',0
sc55_init_:	dc.b	'.SC55_INIT ',0
mt32_init_:	dc.b	'.MT32_INIT ',0
	.even
ret_buf:	dc.w	0	*+0
ret_buf_h:	dc.l	0	*+2 hi
ret_buf_l:	dc.l	0	*+6 low
zmd_play_wk:	dc.l	0
zm_ver_buf:	dc.w	0	*ZMUSICバージョンバッファ

exp_tbl:
	dc.l	1000000000	*0
	dc.l	100000000	*4
	dc.l	10000000	*8
	dc.l	1000000		*12
	dc.l	100000		*16
	dc.l	10000		*20
	dc.l	1000		*24
	dc.l	100		*28
	dc.l	10		*32
	dc.l	1		*36

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

	dc.b	'MUSICZ.FNC V2.02 (C)ZENJI SOFT'
	.even
str_buffer:	dc.l	0	*文字データバッファ
stbf_end:	dc.l	0	*文字データバッファ最終アドレス+1
	.bss
err_flg:	ds.b	1
out_flg:	ds.b	1	*generate mode on/off
suji:		ds.b	20
md_buf:		ds.b	16
	.even
env_work:	ds.b	256	*必ず偶数
open_fn:	ds.b	91
gene_fn:	ds.b	256
filename:	ds.b	91
running:	ds.b	1
pcm8_flg:	ds.b	1
	.even
env_bak:	ds.l	1
last_val:	ds.w	1
rec_address:	ds.l	1
rec_size:	ds.l	1
sv_fh:		ds.w	1
