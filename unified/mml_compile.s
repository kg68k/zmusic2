
**************** MML COMPILE エラー ***************

	*エラーを増やしたら
	*prt_err_codeとerr_mes nも作り加える
	*特にスタックを気にしなくて良い
m_err6:		m_err	#6		*illegal ch
m_err9:		m_err	#9		*I error
m_err10:	m_err	#10		*can not compile
m_err11:	m_err	#11		*@X command error
m_err12:	m_err	#12		*@I command error
m_err13:	m_err	#13		*_~command error
m_err14:	m_err	#14		*ARCC cnf cmd error
m_err15:	m_err	#15		*ARCC error
m_err16:	m_err	#16		*内蔵音源には関係ないコマンドです
m_err17:	m_err	#17		*@Hコマンドエラー
m_err18:	m_err	#18		*@Sコマンドエラー
m_err19:	m_err	#19		*文法エラー
m_err20:	m_err	#20		*[] error
m_err21:	m_err	#21		*there is no ']'
m_err22:	m_err	#22		*繰り返し回数が異常
m_err23:	m_err	#23		*繰り返し構造が異常です *new
m_err24:	m_err	#24		*繰り返し番号が異常
m_err25:	m_err	#25		*オクターブ値が無い
m_err26:	m_err	#26		*オクターブ値が異常
m_err27:	m_err	#27		*音長エラー
m_err28:	m_err	#28		*OUT OF MEMORY
m_err29:	m_err	#29		*@b com error
m_err30:	m_err	#30		*no effect for adpcm
m_err31:	m_err	#31		*タイの指定の誤り
m_err32:	m_err	#32		*t error
m_err33:	m_err	#33		*@t error
m_err34:	m_err	#34		*音長値が無い
m_err35:	m_err	#35		*音量値がない
m_err36:	m_err	#36		*音量値が異常
m_err37:	m_err	#37		*@k error
m_err38:	m_err	#38		*キーコードの値が異常
m_err39:	m_err	#39		*音色番号が異常
m_err40:	m_err	#40		*}がない
m_err41:	m_err	#41		*音符が多すぎ
m_err42:	m_err	#42		*音符が無い
m_err43:	m_err	#43		*illegal q
m_err44:	m_err	#44		*{}に異常なものあり
m_err45:	m_err	#45		*y com error
m_err46:	m_err	#46		*j com error
m_err47:	m_err	#47		*panpot error
m_err48:	m_err	#48		*key transpose error
m_err49:	m_err	#49		*和音エラー
m_err50:	m_err	#50		*和音内にキーが多すぎる
m_err51:	m_err	#51		*@v error
m_err52:	m_err	#52		*portament error
m_err53:	m_err	#53		*@u error(velocity value error)
m_err54:	m_err	#54		*@n error
m_err55:	m_err	#55		*@m error
m_err56:	m_err	#56		*h error
m_err57:	m_err	#57		*@z error
m_err58:	m_err	#58		*illegal parameter format
m_err59:	m_err	#59		*delay too long
m_err63:	m_err	#63		*w error
m_err70:	m_err	#70		*illegal parameter
m_err72:	m_err	#72		*x error
m_err74:	m_err	#74		*s error
m_err75:	m_err	#75		*m error
m_err77:	m_err	#77		*; error
m_err78:	m_err	#78		*\ error
m_err79:	m_err	#79		*? error
m_err80:	m_err	#80		*@f error
m_err81:	m_err	#81		*@g error
m_err82:	m_err	#82		*@y error
m_err83:	m_err	#83		*@e error
	*	d0,a0は破壊禁止
case_m_err:
	move.b	d0,mml_cnv_err-work(a6)
	bsr	set_err_code
m_err_s:			*ncp!!
	bsr	prt_err_code	*print error code
	addq.l	#1,line_number-work(a6)
m_err_s0:
	bsr	goto_crlf	*改行までスキップ
	movea.l	_sp_buf(pc),sp	*後始末
	rts

mmlc_exit:			*normal cases
	move.b	renp_flg(pc),d0	*d0=0ならt_dat_okと同じ
	bne	m_err40
	rts			*d0=0(no problem)

goto_crlf:			*改行までスキップ
	move.l	d0,-(sp)
@@:
	move.b	(a0)+,d0
	cmpi.b	#$1a,d0
	beq	@f
	cmpi.b	#$0a,d0
	bne	@b
@@:
	move.l	(sp)+,d0
	rts

mml_conv:			*ＭＭＬ→中間コードコンパイルルーチン
	* < d0.l=trk number
	* < a0.l=MML DATA ADDRESS
	* 汎用レジスタ群        d0,d1,d2,d3,d4  a2 a4 a5
	* グローバルレジスタ群  d5              a0 a1 a3 a6
	move.l	sp,_sp_buf-work(a6)
	moveq.l	#0,d1
	move.b	d1,mml_cnv_err-work(a6)
	move.w	d1,renp_flg-work(a6)		*onkai_flgも一緒に初期化
	move.l	d1,renp_cnt-work(a6)
*	move.l	d0,mml_tr_no-work(a6)		*save trk number 0-79
	bsr	calc_cnv_wk			*a1=work addr.
	lea	cv_len(a1),a3			*use in wrt_data

	add.w	d0,d0
	add.w	d0,d0
	movea.l	trk_len_tbl(pc),a2
	move.l	(a2,d0.w),d5	*d5=trk buffer max length
	beq	m_err28		*バッファが確保されてない
mml_lp:
	bsr	skip_spc2
	move.b	(a0)+,d0
	bmi	skip_cmmt	*漢字はスキップ
	beq	mmlc_exit	*data end
	cmpi.b	#'^',d0		*通常はありえない
	beq	m_err31
	cmpi.b	#'&',d0
	beq	tie???
	cmp.b	#OUP,d0		*octave up
	beq	oct_up
	cmp.b	#ODWN,d0	*octave down
	beq	oct_dwn
	cmpi.b	#' ',d0
	bcs	mmlc_exit	*SPC以下は改行と見なす
	cmpi.b	#'/',d0
	beq	skip_cmmt
	cmpi.b	#'{',d0		*連符スタート
	beq	_renp_st
	cmpi.b	#'}',d0		*連符エンド
	beq	_renp_ed
	bsr	mk_capital	*d0がalphabetなら大文字にして

	move.b	onkai_flg(pc),onkai_flg_-work(a6)
	clr.b	onkai_flg-work(a6)	*フラグ・クリア

	moveq.l	#0,d1
	move.b	d0,d1
	subi.b	#$41,d1		*'A'を引いて
	bmi	not_alpha_cmd	*英字以外のコマンド
	cmpi.b	#25,d1
	bhi	not_alpha_cmd
	add.w	d1,d1
	move.w	mml_cnv_jmp(pc,d1.w),a2
	jmp	mml_cnv_jmp(pc,a2.w)

mml_cnv_jmp:			*jump table
	dc.w	mml_ag-mml_cnv_jmp	*音階
	dc.w	mml_ag-mml_cnv_jmp	*音階
	dc.w	mml_ag-mml_cnv_jmp	*音階
	dc.w	mml_ag-mml_cnv_jmp	*音階
	dc.w	mml_ag-mml_cnv_jmp	*音階
	dc.w	mml_ag-mml_cnv_jmp	*音階
	dc.w	mml_ag-mml_cnv_jmp	*音階
	dc.w	mml_h-mml_cnv_jmp	*モジュレーションホールド
	dc.w	mml_i-mml_cnv_jmp	*バンクセレクト
	dc.w	mml_j-mml_cnv_jmp	*強制再演奏
	dc.w	mml_k-mml_cnv_jmp	*キートランスポーズ
	dc.w	mml_l-mml_cnv_jmp	*音長
	dc.w	mml_m-mml_cnv_jmp	*モジュレーションモード
	dc.w	mml_n-mml_cnv_jmp	*チャンネルアサイン
	dc.w	mml_o-mml_cnv_jmp	*オクターブ
	dc.w	mml_p-mml_cnv_jmp	*パン
	dc.w	mml_q-mml_cnv_jmp	*ゲートタイム
	dc.w	mml_r-mml_cnv_jmp	*休符
	dc.w	mml_s-mml_cnv_jmp	*波形セレクト
	dc.w	mml_t-mml_cnv_jmp	*テンポ
	dc.w	mml_u-mml_cnv_jmp	*ベロシティ
	dc.w	mml_v-mml_cnv_jmp	*ボリューム
	dc.w	mml_w-mml_cnv_jmp	*同期
	dc.w	mml_x-mml_cnv_jmp	*ローランドエクスクルーシブ
	dc.w	mml_y-mml_cnv_jmp	*Ｙコマンド
	dc.w	mml_z-mml_cnv_jmp	*Ｚコマンド

not_alpha_cmd:			*アルファベット以外のコマンド
	cmpi.b	#'@',d0		*@ commands
	beq	cmd_at
	cmpi.b	#'|',d0		*repeat command
	beq	mml_rep1
	cmpi.b	#':',d0		*repeat start or end
	beq	mml_rep2
	cmpi.b	#';',d0		*ZMD直接書き込み
	beq	mml_direct
	cmpi.b	#'(',d0		*portament
	beq	mml_port
	cmpi.b	#"'",d0		*和音コマンド
	beq	mml_waon
	cmpi.b	#'[',d0		*special commands
	beq	_command
	cmpi.b	#'_',d0		*相対ボリューム down
	beq	vol_dwn
	cmpi.b	#'~',d0		*相対ボリューム up
	beq	vol_up
	cmpi.b	#'`',d0		*強制キーオフ
	beq	kill_kon
	cmpi.b	#'=',d0		*特殊コマンドスイッチ
	beq	mml_switch
	cmpi.b	#'\',d0		*fade_out
	beq	fo_set
	cmpi.b	#'?',d0		*poke command
	beq	mml_poke
	cmpi.b	#'"',d0		*スペシャル・タイ・モード
	beq	mml_dbq

*	move.l	mml_tr_no,d6	*デバッグ用
*	move.l	line_number,d7	*デバッグ用
*	bsr	debug2		*デバッグ用

	bra	m_err19		*文法エラー

skip_cmmt:			*コメント行をスキップ
	bsr	goto_crlf
	addq.l	#1,line_number-work(a6)
	bra	mmlc_exit	*data end

wrt_data_d4:
	move.b	d4,d0
	bra	wrt_data
wrt_data_d3:
	move.b	d3,d0
	bra	wrt_data
wrt_data_d2:
	move.b	d2,d0
	bra	wrt_data
wrt_data_d1:
	move.b	d1,d0
wrt_data:			*コンパイルデータをバッファへ書き込む
	* < d0.b=data
	* < a1=cnv wk n
	* < a3=cv_len(a1)
	* > a2=compiled data address
	* - all (except a2)
	addq.l	#1,(a3)
	cmp.l	(a3),d5		*確保したトラックバッファをオーバーしてないか
	bls	m_err28
	movea.l	(a1),a2
	move.b	d0,(a2)+	*write data
	st.b	(a2)		*set end code
	move.l	a2,(a1)
	rts

mml_p:				*パンポット command P
	bsr	chk_0_9
	bmi	m_err47
	bsr	get_num
	cmpi.l	#3,d0
	bhi	m_err47
	ori.b	#$b0,d0
	bsr	wrt_data	*panpot code
	bra	mml_lp

mml_y:				*Y コマンド
	bsr	do_e_velo	*臨時ベロシティ後処理
	bsr	chk_0_9
	bmi	m_err45
	bsr	get_num		*get register number
	move.l	d0,d1

	bsr	skip_sep2	*skip ','
	bsr	chk_0_9
	bmi	m_err45
	bsr	get_num		*get data
	move.l	d0,d2
	move.l	#$ff,d0
	cmp.l	d0,d1
	bhi	m_err45

	tst.b	cv_device(a1)	*case_midi
	bmi	normal_y

	cmpi.b	#$02,d1		*emulate opmd.x y2,n
	beq	opmd_y2
	cmp.l	d0,d2
	bhi	m_err45
	cmpi.b	#$03,d1		*emulate opmd.x y3,n
	beq	opmd_y3
	cmpi.b	#$08,d1
	beq	koff_y
	cmpi.b	#13,d1		*emulate opmd.x y13,n
	beq	opmd_y13
	cmpi.b	#14,d1		*emulate opmd.x y14,n
	beq	opmd_y14
	cmpi.b	#15,d1		*noise
	beq	case_noise
	cmpi.b	#$30,d1
	bcs	normal_y
	cmpi.b	#$37,d1
	bls	detune_y	*yコマンドによるデチューン

normal_y:
	moveq.l	#$b5,d0		*command code
	bsr	wrt_data
wrt_d1d2:
	bsr	wrt_data_d1	*reg number
	bsr	wrt_data_d2	*data
	bra	mml_lp

opmd_y2:			*ADPCM PLAY
	cmp.l	#adpcm_n_max_default,d2
	bcc	m_err45
	moveq.l	#$9b,d0
	bsr	wrt_data
	move.l	d2,d0
	bsr	wrt_d0w
	bra	mml_lp

opmd_y3:			*ADPCM PANPOT
	moveq.l	#$b7,d0
	bsr	wrt_data
	move.b	d2,d0
	andi.b	#3,d0
	bsr	wrt_data
	bra	mml_lp

koff_y:
	move.b	d2,d0
	andi.b	#$f8,d0
	bne	normal_y
	moveq.l	#$bf,d0
	bsr	wrt_data
	bra	mml_lp

opmd_y13:			*ADPCM FRQ CHANGE
	moveq.l	#$a2,d0
	bsr	wrt_data
	move.b	d2,d0
	cmpi.b	#4,d0
	bls	set_y13
	moveq.l	#4,d0
set_y13:
	bsr	wrt_data
	bra	mml_lp
opmd_y14:			*ADPCM SE MODE
	moveq.l	#$b8,d0
	bsr	wrt_data
	bsr	wrt_data_d2
	bra	mml_lp
detune_y:			*Yｺﾏﾝﾄﾞによるﾃﾞﾁｭｰﾝを@Kｺﾏﾝﾄﾞに変換
	lsr.b	#2,d2		*@k fm value(0-63)
	move.l	d2,d1
	subq.w	#5,d2		*4MHz BIAS minus
	muls	#683,d1		*d1=683*d1
	asr.l	#6,d1		*d1=d1/64 d1=-8192～8191
	bra	do_wrt_k

case_noise:			*NOISE
	tst.b	d2		*btst.l	#7,d2
	bpl	@f		*beq	@f
	moveq.l	#$a5,d0		*noise set
	bsr	wrt_data
	bsr	wrt_data_d2
	bra	mml_lp
@@:
	moveq.l	#$82,d0		*noise off
	bsr	wrt_data
	bra	mml_lp

mml_h:				*モジュレーション波形のホールド
	cmpi.b	#1,cv_device(a1)
	beq	m_err30		*ADPCMには関係ないコマンドです
	moveq.l	#-1,d1
	bsr	chk_0_9
	bmi	@f
	moveq.l	#1,d1
	bsr	get_num
	tst.l	d0
	bne	@f
	moveq.l	#0,d1
@@:
	moveq.l	#-1,d2
	bsr	skip_sep2	*skip ','
	bsr	chk_0_9
	bmi	@f
	moveq.l	#1,d2
	bsr	get_num
	tst.l	d0
	bne	@f
	moveq.l	#0,d2
@@:
	move.l	d1,d0
	and.l	d2,d0
	bmi	m_err56		*両方省略はダメ
	moveq.l	#$9c,d0
	bsr	wrt_data
	bra	wrt_d1d2

mml_m:				*モジュレーションモード選択
	tst.b	cv_device(a1)
	bpl	m_err16		*FM/ADPCMには関係ないコマンドです
	moveq.l	#-1,d1
	bsr	chk_0_9
	bmi	@f
	bsr	get_num
	move.l	d0,d1
	cmp.l	#2,d1
	bhi	m_err75		*規定外
@@:
	moveq.l	#-1,d2
	bsr	skip_sep2	*skip ','
	bsr	chk_0_9
	bmi	@f
	bsr	get_num
	move.l	d0,d2
	cmp.l	#1,d2
	bhi	m_err75		*規定外
@@:
	move.l	d1,d0
	and.l	d2,d0
	bmi	m_err75		*両方省略はダメ
	moveq.l	#$99,d0
	bsr	wrt_data
	bra	wrt_d1d2

mml_s:				*モジュレーション波形タイプ(FM ONLY)
	moveq.l	#wv_max+wv_def_max-1,d4
	cmpi.b	#1,cv_device(a1)
	beq	m_err30		*ADPCMには関係ないコマンドです
	moveq.l	#-1,d1
	bsr	chk_0_9
	bmi	@f
	bsr	get_num
	move.l	d0,d1
	cmp.l	d4,d1
	bhi	m_err74		*規定外
	cmpi.b	#4,d1
	bcs	@f
	cmpi.b	#7,d1
	bls	m_err74		*4～7はリザーブ
@@:
	moveq.l	#-1,d2
	bsr	skip_sep2	*skip ','
	bsr	chk_0_9
	bmi	@f
	bsr	get_num
	move.l	d0,d2
	cmp.l	d4,d2
	bhi	m_err74		*規定外
	cmpi.b	#4,d2
	bcs	@f
	cmpi.b	#7,d2
	bls	m_err74		*4～7はリザーブ
@@:
	move.l	d1,d0
	and.l	d2,d0
	bmi	m_err74		*両方省略はダメ
	moveq.l	#$98,d0
	bsr	wrt_data
	bra	wrt_d1d2

_renp_st:			*連符スタート
	move.l	(a1),psp_buf-work(a6)	*trk data addr
	st	renp_flg-work(a6)	*mark
	bra	mml_lp

_renp_ed:			*連符エンド
	lea	renp_cnt(pc),a4
	clr.b	renp_flg-work(a6)
	bsr	chk_0_9
	beq	rp_get_l	*指定してある時…
	cmpi.b	#wstep,(a0)
	bne	@f
	addq.w	#1,a0
	bsr	get_num
	cmpi.l	#65534,d0
	bhi	m_err27		*illegal length
	bra	_renp_ed1
@@:
	moveq.l	#0,d0
	move.w	cv_l_com(a1),d0	*省略のケース
	bra	_renp_ed1
rp_get_l:
	bsr	get_length
	bsr	futen_ope
	andi.l	#$ffff,d0
_renp_ed1:
	move.l	(a4),d1		*number of keycodes
	beq	m_err42		*there is no keycode inside!
	cmp.l	d1,d0
	bcs	m_err41		*too many keycodes inside!
	divu	d1,d0
	cmpi.w	#254,d0
	bhi	m_err27		*illegal length
	move.l	d0,d1
	swap	d0
	move.w	d0,rest_buf-work(a6)	*save rest
	ext.l	d1		*d1=answer
	move.l	d1,d3
	movea.l	psp_buf(pc),a2	*get renp start addr.
_renp_lp01:
	move.b	(a2),d0
	bmi	renp_srch_key	*キーコード以外が見付かった
_rnp_gt_st:
	tst.b	2(a2)		*gate time check(0ならばゲートタイム計算必要)
	beq	rp_not_tie
_rnp_gt_tie:			*case tie
	bsr	renp_last?
	move.b	d1,1(a2)	*step time
	bra	_renp_ed2
rp_not_tie:
	bsr	renp_last?
	move.b	d1,1(a2)	*save step time
	move.w	d1,d0
	bsr	calc_gt
	move.b	d0,2(a2)	*save gate time
_renp_ed2:
	addq.w	#3,a2
	subq.l	#1,(a4)
	bne	_renp_lp01
	bra	mml_lp
renp_last?:			*誤差を考慮
	move.l	d3,d1
	subq.w	#1,rest_buf-work(a6)
	bmi	@f
	addq.w	#1,d1
@@:
	rts

renp_srch_key:			*途中に特殊コマンドがあってキーコードを見失った場合
	* < d0.b=zmd code
	lea	rsk_tbl(pc),a5
@@:
	move.b	(a5)+,d1
	lsl.w	#8,d1
	move.b	(a5)+,d1
	tst.w	d1
	beq	m_err44
rsk_lp00:
	move.b	(a5)+,d2
	beq	@b
	cmp.b	d0,d2
	bne	rsk_lp00
	jmp	rsk_tbl(pc,d1.w)


endMarkRequired:=0
RSK_FUNC: .macro label
    .if	endMarkRequired
	.dc.b	0
    .endif
	.dc.b	((label-rsk_tbl)>>8).and.$ff,(label-rsk_tbl).and.$ff
	endMarkRequired:=1
.endm
RSK_TBL_END: .macro
	RSK_FUNC rsk_tbl	*オフセット値が0でテーブル終了
.endm

rsk_tbl:
	RSK_FUNC _rnp_gt_st
	dc.b	$80

	RSK_FUNC _rnp_gt_tie
	dc.b	$d0

	RSK_FUNC rsk_case_port
	dc.b	$e0

	RSK_FUNC rsk_case_waon
	dc.b	$e2

	RSK_FUNC rsk_plus1
	dc.b	$81,$82,$83,$84,$85,$86,$87
	dc.b	$88,$89,$8a,$8b,$8c,$8d,$8e,$8f
	dc.b	$b0,$b1,$b2,$b3
	dc.b	$bf

	RSK_FUNC rsk_plus2
	dc.b	$a0,$a1,$a2,$a3,$a4,$a5,$a6,$a7
	dc.b	$a8,$a9,$aa,$ab,$ac,$ad,$ae,$af
	dc.b	$b4,$b6,$b7,$b8,$b9,$bb,$bc,$bd,$be
	dc.b	$c0,$c1,$c2,$c3,$c4,$c5,$c6,$c7
	dc.b	$c8,$c9,$ca,$cb,$cc,$cd,$ce,$cf
	dc.b	$d9,$da,$db

	RSK_FUNC rsk_plus3
	dc.b	$90,$91,$92,$93,$94,$95,$96,$97
	dc.b	$98,$99,$9b,$9c,$9d,$9e,$9f
	dc.b	$b5
	dc.b	$d3,$d4,$d5,$d7
	dc.b	$d8,$dc,$dd,$de,$df
	dc.b	$e6,$e9

	RSK_FUNC rsk_plus4
	dc.b	$9a
	dc.b	$eb

	RSK_FUNC rsk_plus5
	dc.b	$d1,$d2,$d6
	dc.b	$e8

	RSK_FUNC rsk_plus9
	dc.b	$ba
	dc.b	$e3

	RSK_FUNC rsk_plus10
	dc.b	$ef

	RSK_FUNC rsk_plus12
	dc.b	$e1

	RSK_FUNC rsk_plus18
	dc.b	$ee

	RSK_TBL_END
	.even

rsk_plus18:
	lea	18(a2),a2
	bra	_renp_lp01
rsk_plus12:
	lea	12(a2),a2
	bra	_renp_lp01
rsk_plus10:
	lea	10(a2),a2
	bra	_renp_lp01
rsk_plus9:
	lea	9(a2),a2
	bra	_renp_lp01
rsk_plus5:
	addq.w	#5,a2
	bra	_renp_lp01
rsk_plus4:
	addq.w	#4,a2
	bra	_renp_lp01
rsk_plus3:
	addq.w	#3,a2
	bra	_renp_lp01
rsk_plus2:
	addq.w	#2,a2
	bra	_renp_lp01
rsk_plus1:
	addq.w	#1,a2
	bra	_renp_lp01

rsk_case_port:			*連符内にポルタメント指定があった場合の処理
	movem.l	d2/d4-d6,-(sp)
	move.b	2(a2),d0
	lsl.w	#8,d0
	move.b	3(a2),d0	*d0.w=step
	move.b	6(a2),d5
	lsl.w	#8,d5
	move.b	7(a2),d5	*d5.w=delay
	move.b	8(a2),d2
	lsl.w	#8,d2
	move.b	9(a2),d2	*d2.w=step
	sub.w	d5,d0
	subq.w	#1,d0		*for dbra
	bcs	m_err41
	moveq.l	#0,d4		*range
	moveq.l	#0,d6		*work
	move.b	11(a2),d1
	ext.w	d1
port_simu:			*逆算
	add.w	d2,d4
	add.b	10(a2),d6	*補正パラメータ
	bcc	@f
	add.w	d1,d4
@@:
	dbra	d0,port_simu
	ext.l	d4
	bsr	renp_last?	*get real step
	clr.b	2(a2)
	move.b	d1,3(a2)	*set real step time
	sub.w	d5,d1
	bls	m_err59		*delay too long
	move.b	4(a2),d0
	lsl.w	#8,d0
	move.b	5(a2),d0	*d0.w=gate
	cmpi.w	#-1,d0
	beq	@f		*タイならゲートタイムの計算はしない
	move.l	d1,d0
	bsr	calc_gt
	clr.b	4(a2)
	move.b	d0,5(a2)	*save gate time
@@:
*	subq.w	#1,d1
*	beq	m_err41
*	bmi	m_err41
	divs	d1,d4
	move.l	d4,d2
	clr.w	d2
	swap	d2
	tst.w	d2
	bpl	@f
	neg.w	d2
@@:
	lsl.w	#8,d2
	divu	d1,d2
	swap	d2
	tst.w	d2
	beq	@f
	add.l	#$0001_0000,d2
@@:
	swap	d2
	ror.w	#8,d4
	move.b	d4,8(a2)
	ror.w	#8,d4
	move.b	d4,9(a2)
	move.b	d2,10(a2)
	lea	12(a2),a2	*cmd size
	movem.l	(sp)+,d2/d4-d6
	subq.l	#1,(a4)
	bne	_renp_lp01
	bra	mml_lp

rsk_case_waon:			*連符内に和音があった場合の処理
	move.b	3(a2),d0
	lsl.w	#8,d0
	move.b	4(a2),d0
	cmpi.w	#$ffff,d0	*gate time (tie?)
	bne	rp_not_tie_waon
				*case tie
	bsr	renp_last?
	clr.b	1(a2)		*step upper
	move.b	d1,2(a2)	*step time
	bra	_renp_ed2_waon
rp_not_tie_waon:
	bsr	renp_last?
	clr.b	1(a2)		*step upper
	move.b	d1,2(a2)	*save step time
	move.w	d1,d0
	bsr	calc_gt
	clr.b	3(a2)
	move.b	d0,4(a2)	*save gate time
_renp_ed2_waon:
	moveq.l	#0,d0
	move.b	5(a2),d0
	cmp.w	d1,d0		*ｽﾃｯﾌﾟﾀｲﾑとﾃﾞｨﾚｲどっちが大きいか
	bcc	m_err59		*delay too long
	lea	14(a2),a2	*cmd size
	subq.l	#1,(a4)
	bne	_renp_lp01
	bra	mml_lp

_command:			*[]コマンド
	bsr	do_e_velo	*臨時ベロシティ後処理
	tst.b	renp_flg-work(a6)	*連符内に[]コマンドは使用出来ない
	bne	m_err44
	bsr	skip_spc2
	bsr	get_cmd_no	*get cmd number
	bmi	m_err20		*[]error
	bsr	skip_spc2
	cmpi.w	#14,d0		*14は特別
	beq	@f
	cmpi.b	#']',(a0)+
	bne	m_err21		*can't find ']'
@@:
	add.w	d0,d0
	move.w	_cmd_tbl(pc,d0.w),d0
	jmp	_cmd_tbl(pc,d0.w)
_cmd_tbl:
	dc.w	mmldc-_cmd_tbl		*0
	dc.w	mmlds-_cmd_tbl		*1
	dc.w	mmlsegn-_cmd_tbl	*2
	dc.w	mmlsegn-_cmd_tbl	*3
	dc.w	mmltocd-_cmd_tbl	*4
	dc.w	mmltocd-_cmd_tbl	*5
	dc.w	mmlfine-_cmd_tbl	*6
	dc.w	mmlfine-_cmd_tbl	*7
	dc.w	mmlcoda-_cmd_tbl	*8
	dc.w	mmldo-_cmd_tbl		*9
	dc.w	mmlloop-_cmd_tbl	*10
	dc.w	mml_jump-_cmd_tbl	*11
	dc.w	mml_end-_cmd_tbl	*12
	dc.w	mml_jump2-_cmd_tbl	*13
	dc.w	k_signature-_cmd_tbl	*14
mmldc:
	moveq.l	#3,d1
	bra	wrt_cmd
mmlsegn:
	moveq.l	#4,d1
	bra	wrt_cmd
mmlds:
	moveq.l	#5,d1
	bra	wrt_cmd
mmlcoda:
	moveq.l	#6,d1
	bra	wrt_cmd
mmltocd:
	moveq.l	#7,d1
	bra	wrt_cmd
mmlfine:
	moveq.l	#8,d1
	bra	wrt_cmd
mmldo:
	moveq.l	#9,d1
	bra	wrt_cmd
mmlloop:
	moveq.l	#10,d1
	bra	wrt_cmd
mml_jump:
	tst.b	act_flg-work(a6)
	beq	mml_lp
	moveq.l	#$c0,d0		*command code
	bsr	wrt_data
	moveq.l	#11,d0
	bsr	wrt_data
*	moveq.l	#$80,d0		*ダミー休符
*	bsr	wrt_data
*	moveq.l	#$1,d0
*	bsr	wrt_data
*	bsr	wrt_data
	bra	mml_lp
mml_end:
	tst.b	act_flg-work(a6)	*m_debug()コマンドによるスイッチング
	beq	mml_lp
	moveq.l	#$ff,d0		*終了
	bsr	wrt_data
	bra	mml_lp
mml_jump2:			*cp!!
	tst.b	act_flg-work(a6)
	beq	mml_lp
	moveq.l	#12,d1

wrt_cmd:
	* < d1.b=cmd number
	moveq.l	#$c0,d0		*command code
	bsr	wrt_data
	bsr	wrt_data_d1
	bra	mml_lp

k_signature:			*デフォルト調号指定
	moveq.l	#0,d0
	moveq.l	#0,d3
	lea	cv_k_sig(a1),a2
	move.b	d3,(a2)+
	move.w	d3,(a2)+	*initialize
	move.l	d3,(a2)
ksi_lp:
	bsr	skip_spc2
	move.b	(a0)+,d0
	cmpi.b	#' ',d0
	bcs	m_err21		*can't find ']'
	cmpi.b	#']',d0
	beq	mml_lp
	cmpi.b	#'+',d0
	beq	@f
	cmpi.b	#'#',d0
	bne	ksi_minus?
@@:
	addq.b	#1,d3
	bra	ksi_lp
ksi_minus?:
	cmpi.b	#'-',d0
	bne	@f
	subq.b	#1,d3
	bra	ksi_lp
@@:
	cmpi.b	#' ',d0
	beq	ks_sep
	cmpi.b	#',',d0
	bne	@f
ks_sep:
	moveq.l	#0,d3
	bra	ksi_lp
@@:
	bsr	mk_capital
	sub.b	#'A',d0
	bmi	m_err58
	cmpi.w	#6,d0
	bhi	m_err58
	move.b	d3,-3(a2,d0.w)	*set
	bra	ksi_lp

get_cmd_no:			*[]コマンドを捜索
	movem.l	d1-d3/a1-a2,-(sp)
	moveq.l	#0,d2
	moveq.l	#num_of_seq-1,d3	*number of []cmds
	lea	mus_com_tbl(pc),a1
gcn_lp01:
	bsr	do_get_cmd_no
	beq	exit_gcn
@@:
	tst.b	(a1)+		*次のコマンドへ
	bne	@b
	addq.b	#1,d2		*cmd number
	dbra	d3,gcn_lp01
	moveq.l	#-1,d0		*couldn't find it...
	movem.l	(sp)+,d1-d3/a1-a2
	rts
exit_gcn:
	move.l	d2,d0		*d0=cmd number
	movem.l	(sp)+,d1-d3/a1-a2
	rts

do_get_cmd_no:			*実際に文字列を捜す
	* < a1=source str addr
	* > eq=get it!
	* > mi=can't found
	move.l	a1,-(sp)
	move.l	a0,d1		*save a0 to d1
dgcn_lp:
	move.b	(a0)+,d0
	cmpi.b	#' ',d0
	beq	dgcn_lp
	bsr	mk_capital	*小文字→大文字
	cmp.b	(a1)+,d0
	bne	not_same_dgcn
	tst.b	(a1)
	bne	dgcn_lp
exit_dgcn:
	move.l	(sp)+,a1
	moveq.l	#0,d0		*right!
	rts
not_same_dgcn:
	move.l	d1,a0		*get back a0
	move.l	(sp)+,a1
	moveq.l	#-1,d0		*error!
	rts

mml_rep1:			*リピート関係の処理
	bsr	do_e_velo	*臨時ベロシティ後処理
	tst.b	renp_flg-work(a6)	*連符内にrepeatコマンドは使用出来ない
	bne	m_err44
	bsr	skip_spc2
	cmpi.b	#':',(a0)
	bne	rep_exit	*数字や省略形
	addq.w	#1,a0
	bsr	chk_0_9
	bmi	case_rep_2	*count省略のケースは２回とみなす
	bsr	get_num
	bra	rep_cmd_set
case_rep_2:
	moveq.l	#2,d0
rep_cmd_set:
	tst.l	d0		*0はエラー
	beq	m_err22		*illegal repeat number
	cmpi.l	#255,d0
	bhi	m_err22
	bsr	push_rep_start	*mark repeat start address
	move.b	d0,d1		*d1=repeat cnt
	moveq.l	#$c1,d0		*repeat start cmd code
	bsr	wrt_data
	moveq.l	#$cf,d0		*dummy
	bsr	wrt_data
	bsr	wrt_data_d1	*repeat count
	moveq.l	#-1,d0		*dummy
	bsr	set_jmp_exit
	bra	mml_lp

rep_exit:			*ループを抜け出すコマンド
	bsr	chk_0_9
	bmi	case_re_zero	*最後のリピート時に抜け出すという別コマンドになる
	bsr	get_num
	cmpi.l	#255,d0
	bhi	m_err24		*繰り返し番号が異常

	move.b	d0,d1
	moveq.l	#$c3,d0		*cmd code
	bsr	wrt_data
	bsr	wrt_data_d1	*rept cnt

	move.l	(a1),d0
	bsr	set_jmp_exit
	moveq.l	#0,d0
	bsr	wrt_data	*dummy 後でここに|n処理後の飛先が入る
	bsr	wrt_data	*dummy
	bsr	set_rept_cnt	*回数指定があることを報告
	bra	mml_lp
case_re_zero:			*ループの最後に抜け出すというコマンド
	moveq.l	#$c4,d0		*cmd code
	bsr	wrt_data

	move.l	(a1),d0
	bsr	set_jmp_exit
	moveq.l	#0,d0
	bsr	wrt_data	*dummy 後でここに|n処理後の飛先が入る
	bsr	wrt_data	*dummy
	bra	mml_lp

mml_rep2:			*リピート終了コマンド
	bsr	do_e_velo	*臨時ベロシティ後処理
	cmpi.b	#'|',(a0)	*本当に終了コマンド?
	bne	mml_lp		*nothing!?
	addq.w	#1,a0
	bsr	pop_rep_start	*d0.w=offset addr
	bmi	m_err23		*対応するリピートスタートは無かった
	move.w	d0,d1
	moveq.l	#$c2,d0		*set cmd code
	bsr	wrt_data
wrt_d1w:
	move.w	d1,d0
	bsr	wrt_d0w
	bra	mml_lp

wrt_d0w:
	ror.w	#8,d0
	bsr	wrt_data
	ror.w	#8,d0
	bra	wrt_data

push_rep_start:			*リピートスタートアドレスのセット
	* < d0.l=repeat counter
	movem.l	d1-d2/a2,-(sp)
	move.l	(a1),d2		*d2=copmiled data addr.
	addq.l	#1,d2		*便宜上(ジャンプ先を$cfにあわせるため)
	lea.l	cv_rep_addr(a1),a2
	moveq.l	#8-1,d1
prs_lp01:
	tst.l	(a2)
	beq	vacant
	addq.w	#6,a2
	dbra	d1,prs_lp01
	subq.w	#6,a2		*最後を潰す
	moveq.l	#0,d1
vacant:
	move.l	d2,(a2)+	*push adr
	move.w	#1,(a2)		*clear flag & set dummy repeat counter 0,1
	moveq.l	#7,d2
	sub.w	d1,d2
	move.b	d0,cv_rep_cnt(a1,d2.w)	*save repeat counter
	movem.l	(sp)+,d1-d2/a2
	rts

pop_rep_start:			*リピートスタートアドレスのゲット
	* > d0.l=repeat start address
	movem.l	d1-d2/a2-a4,-(sp)
	movea.l	(a1),a3		*a3=copmiled data addr.
	addq.w	#3,a3		*便宜上
	lea.l	cv_rep_addr+8*6-6(a1),a2
	moveq.l	#8-1,d1
pprs_lp01:
	tst.l	(a2)
	bne	exact
	subq.w	#6,a2
	dbra	d1,pprs_lp01
	moveq.l	#0,d1
	tst.l	cv_rep_start(a1)	*ない場合は非常用を使用
	beq	pprs_err		*それもない
	move.l	cv_rep_start(a1),a2
	bra	exact1
pprs_err:
	moveq.l	#-1,d0		*対応するリピートスタート無し
pprs_exit:
	movem.l	(sp)+,d1-d2/a2-a4
	rts
exact:
	tst.w	d1		*一番底か
	bne	exact1
	move.l	a2,cv_rep_start(a1)	*非常用として保存
exact1:
	move.l	d1,d2
	add.w	d2,d2
	add.w	d2,d2		*d2=d1*4
	tst.l	cv_rep_exit(a1,d2.w)
	beq	@f
	bmi	@f
	move.l	cv_rep_exit(a1,d2.w),a4
	move.l	a3,d0
	sub.l	a4,d0
	subq.l	#2,d0
	ror.w	#8,d0
	move.b	d0,(a4)+
	ror.w	#8,d0
	move.b	d0,(a4)
@@:
	clr.l	cv_rep_exit(a1,d2.w)
	suba.l	(a2),a3
	move.l	a3,d0
	subq.b	#1,5(a2)		*dec repeat counter
	bne	pprs_exit
	clr.l	(a2)			*空きにする
	clr.b	cv_rep_cnt(a1,d1.w)	*clear saved repeat counter
	movem.l	(sp)+,d1-d2/a2-a4
	rts

set_rept_cnt:			*リピートカウンタをセットする
	movem.l	d1/a2-a3,-(sp)
	movea.l	(a1),a3		*a3=copmiled data addr.
	addq.w	#3,a3		*便宜上
	lea.l	cv_rep_addr+8*6-6(a1),a2
	moveq.l	#8-1,d1
@@:
	tst.l	(a2)
	bne	@f
	subq.w	#6,a2
	dbra	d1,@b
	bra	m_err23		*loop構造が異常です
@@:
	move.b	cv_rep_cnt(a1,d1.w),d1
	tst.b	4(a2)
	bne	@f		*既に書いた
	st.b	4(a2)
	move.b	d1,5(a2)	*set repeat counter
@@:
	movem.l	(sp)+,d1/a2-a3
	rts

set_jmp_exit:
	* < d0=rep exit address
	movem.l	d1/a2,-(sp)
	lea.l	cv_rep_addr+8*6-6(a1),a2
	moveq.l	#8-1,d1
sjs_lp01:
	tst.l	(a2)
	bne	vacant_sjs
	subq.w	#6,a2
	dbra	d1,sjs_lp01
	addq.w	#6,a2		*最後を潰す
	moveq.l	#0,d1
vacant_sjs:
	add.w	d1,d1
	add.w	d1,d1
	move.l	d0,cv_rep_exit(a1,d1.w)	*push adr
	movem.l	(sp)+,d1/a2
	rts

mml_q:				*Ｑコマンド
	move.b	onkai_flg_(pc),onkai_flg-work(a6)
	tst.b	renp_flg-work(a6)	*連符内にQコマンドは使用出来ない
	bne	m_err44
	bsr	chk_0_9
	bmi	m_err43		*illegal Q
	bsr	get_num
	cmpi.l	#8,d0
	bhi	m_err43		*illegal Q
set_q:
	move.w	d0,cv_q_com(a1)
	bra	mml_lp

mml_o:				*オクターブ
	bsr	chk_0_9
	bmi	m_err25		*no value
	bsr	get_num		*d0=-1～9
	addq.l	#1,d0		*d0=0～10
	cmpi.l	#10,d0
	bhi	m_err26		*illegal octave
	move.b	d0,cv_oct(a1)
	bra	mml_lp

oct_up:				*オクターブアップ(ここの仕様を変えたら
				*		  PORTAMENTの部分のも変える)
	cmpi.b	#10,cv_oct(a1)
	beq	m_err26		*illegal octave
	addq.b	#1,cv_oct(a1)
	bra	mml_lp

oct_dwn:			*オクターブダウン
	tst.b	cv_oct(a1)
	beq	m_err26		*illegal octave
	subq.b	#1,cv_oct(a1)
	bra	mml_lp

mml_l:				*Ｌコマンド
	tst.b	renp_flg-work(a6)	*連符内にLコマンドは使用出来ない
	bne	m_err44
	bsr	skip_spc2
	cmpi.b	#'*',(a0)
	bne	@f
	addq.w	#1,a0		*skip '*'
get_atl:
	move.b	onkai_flg_(pc),onkai_flg-work(a6)
	bsr	chk_0_9
	bmi	m_err34		*no L value
	bsr	get_num
	move.l	d0,d1
	bra	ftn_?
@@:
	bsr	chk_0_9
	bmi	m_err34		*no L value
	bsr	get_length
ftn_?:
	bsr	futen_ope
	move.w	d0,cv_l_com(a1)
	bra	mml_lp

mml_k:				*キートランスポーズ
	bsr	chk_0_9
	bmi	m_err48		*k com err
	bsr	get_num
	move.b	d0,d1
	bpl	chk_k_abs
	neg.l	d0
chk_k_abs:
	cmpi.l	#127,d0		*絶対値で既定範囲内かをチェック
	bhi	m_err48
	move.b	d1,cv_ktrans(a1)
	bra	mml_lp

mml_t:				*テンポコマンド
	bsr	skip_spc2
	moveq.l	#$94,d2
	move.b	(a0),d0
	cmpi.b	#'-',d0
	beq	mml_rltv_t1
	cmpi.b	#'+',d0
	beq	mml_rltv_t2
	bsr	chk_0_9
	bmi	m_err32		*no T value
	bsr	get_num
	cmpi.l	#20,d0
	bcs	m_err32		*tempo error
	cmpi.l	#tempo_max,d0
	bhi	m_err32
	move.l	d0,d1
	move.b	#$91,d0
	bsr	wrt_data	*write cmd
	bra	wrt_d1w
mml_rltv_t1:
	addq.w	#1,d2
mml_rltv_t2:
	addq.w	#1,a0
	bsr	chk_0_9
	bmi	m_err32		*no T value
	bsr	get_num
	move.l	d0,d1
	bmi	m_err32		*tempo error
	bsr	wrt_data_d2
	bra	wrt_d1w

mml_n2:				*@N
	bsr	chk_0_9
	bmi	m_err54		*no @N value
	bsr	get_num
	subq.l	#1,d0
	moveq.l	#ch_max-1,d4
	tst.b	pcm8_flg-work(a6)
	beq	@f
	addq.b	#7,d4
@@:
	cmp.l	d4,d0
	bls	do_mml_n
	bra	m_err54		*illegal N
mml_n:				*N
	bsr	chk_0_9
	bmi	m_err54		*no N value
	bsr	get_num
	subq.l	#1,d0
	moveq.l	#ch_max-1,d4
	tst.b	pcm8_flg-work(a6)
	beq	@f
	addq.b	#7,d4
@@:
	cmp.l	d4,d0
	bhi	m_err54		*illegal N
	bsr	get_real_ch	* > d0=real ch
do_mml_n:
	cmpi.b	#ch_max-1,d0
	bhi	@f
	cmpi.b	#8,d0
	shi	cv_device(a1)
	bne	mdb0
@@:
	move.b	#1,cv_device(a1)	*adpcm
mdb0:				*mdb!!
	tst.b	cv_device(a1)
	bmi	m_err6
mmlnn:
	move.b	d0,d1
	move.b	#$a3,d0		*cmd code
	bsr	wrt_data
	bsr	wrt_data_d1	*set v value
	bra	mml_lp

mml_v:				*ボリュームコマンド
*	cmpi.b	#1,cv_device(a1)
*	beq	m_err30		*ADPCMには関係ないコマンドです
	bsr	chk_0_9
	bmi	m_err35		*no V value
	bsr	get_num
	cmpi.l	#16,d0
	bhi	m_err36		*illegal V
	tst.b	cv_device(a1)	*check device
	beq	case_v_fm
	move.w	d0,d1
	lsl.w	#3,d1		*MIDI(ADPCM)なら線形変換
	subq.b	#1,d1
	bpl	wrt_v_cmd
	moveq.l	#0,d1		*マイナス値は０に
	bra	wrt_v_cmd
case_v_fm:			*内蔵音源なら指数変換
	move.b	vol_tbl(pc,d0.w),d1
wrt_v_cmd:
	* < d1.b=volume(0-127)
	move.b	#$b6,d0		*cmd code
	bsr	wrt_data
	bsr	calc_master
	tst.b	cv_device(a1)
	bmi	@f
	move.b	d1,cv_velo2(a1)	*内蔵音源のケース
@@:
	moveq.l	#$7f,d0
	sub.b	d1,d0
	bsr	wrt_data	*set v value
	bra	mml_lp

vol_tbl:	*0 1  2  3  4  5  6   7   8   9   10  11  12  13  14  15  16
	dc.b	85,87,90,93,95,98,101,103,106,109,111,114,117,119,122,125,127
	.even

calc_master:
	* < d1.b=0-127
	* X d0
	* > d1.b=answer
	tst.b	cv_device(a1)	*ＦＭ音源のみ有効
	bne	@f
	ext.w	d1
	moveq.l	#0,d0
	move.b	fm_master(pc),d0
	mulu	d0,d1
	divu	#255,d1		*マスターボリューム考慮
@@:
	rts

mml_idset:			*MIDI MODULE ID SET
	moveq.l	#127,d4
	bsr	chk_0_9
	bmi	m_err12		*@I command error
	bsr	get_num
	move.l	d0,d1
	cmp.l	d4,d1		*127より大きいとダメ
	bhi	m_err12
	bsr	skip_sep2

	bsr	chk_0_9
	bmi	m_err12		*2番目の値がない
	bsr	get_num
	move.l	d0,d2
	cmp.l	d4,d2
	bhi	m_err12
	bsr	skip_sep2

	bsr	chk_0_9
	bmi	m_err12		*3番目の値がない
	bsr	get_num
	move.l	d0,d3
	cmp.l	d4,d3
	bhi	m_err12

	moveq.l	#$eb,d0
	bsr	wrt_data
wrt_d1d2d3:
	bsr	wrt_data_d1
	bsr	wrt_data_d2
	bsr	wrt_data_d3
	bra	mml_lp

mml_u:				*uコマンド
	bsr	do_e_velo	*臨時ベロシティ後処理
	moveq.l	#127,d4
*	cmpi.b	#1,cv_device(a1)
*	beq	m_err30		*ADPCMには関係ないコマンドです
	pea	mml_lp(pc)
	bclr.b	#0,cv_cnv_flg(a1)	*velocity sequence stop
	bsr	chk_0_9
	bpl	do_mml_u
reset_velo:
	moveq.l	#$b9,d0
	bsr	wrt_data
	move.b	cv_velo2(a1),d0
	bra	wrt_data
do_mml_u:
	cmpi.b	#'-',(a0)
	beq	@f
	cmpi.b	#'+',(a0)
	bne	mml_atu0
				*up
	bsr	get_num
	move.l	d0,d1
	beq	m_err53
	cmp.l	d4,d1
	bhi	m_err53
	moveq.l	#$ca,d0
	bsr	wrt_data
	bra	wrt_data_d1
@@:				*down
	bsr	get_num
	neg.l	d0
	beq	m_err53
	move.l	d0,d1
	cmp.l	d4,d1
	bhi	m_err53
	moveq.l	#$cb,d0
	bsr	wrt_data
	bra	wrt_data_d1

mml_atu:			*@uコマンド
	bsr	do_e_velo	*臨時ベロシティ後処理
*	cmpi.b	#1,cv_device(a1)
*	beq	m_err30		*ADPCMには関係ないコマンドです
	pea	mml_lp(pc)
	bclr.b	#0,cv_cnv_flg(a1)	*velocity sequence stop
	bsr	chk_0_9
	bmi	rltv_u_dflt	*デフォルト相対
	cmpi.b	#'+',(a0)
	beq	relative_u
	cmpi.b	#'-',(a0)
	beq	relative_u
mml_atu0:
	bsr	get_num
	move.l	d0,d1
mml_atu_:
	tst.b	d1		*計算結果がマイナスになってないか
	bmi	m_err53
	move.b	d1,cv_velo2(a1)
	moveq.l	#$b9,d0
	bsr	wrt_data
	bra	wrt_data_d1

rltv_u_dflt:
	move.b	cv_rltv_velo(a1),d0
	bra	relative_u_
relative_u:
	bsr	get_num
	move.l	d0,d1
	beq	m_err53
	bpl	@f
	neg.l	d1
@@:
	cmpi.l	#127,d1		*絶対値が１２７を越えてる
	bhi	m_err53
	move.b	d0,cv_rltv_velo(a1)
relative_u_:
	move.b	cv_velo2(a1),d1
	add.b	d0,d1
	bpl	mml_atu_
	moveq.l	#0,d1
	tst.b	d0
	bmi	mml_atu_
	moveq.l	#127,d1
	bra	mml_atu_

mml_x:				*MIDI EXCLUSIVE DATA SEND
	tst.b	cv_device(a1)
	bpl	m_err16		*FM/ADPCMには関係ないコマンドです
	tst.b	renp_flg-work(a6)	*連符内には使用出来ない
	bne	m_err44
	bsr	chk_0_9
	bmi	m_err72		*値が一個もない時はｴﾗｰ

	moveq.l	#$ea,d0		*send cmd code
	bsr	wrt_data
	moveq.l	#$7f,d2		*check over $7f
	bsr	wrt_send_data
	moveq.l	#$80,d0
	andi.b	#$7f,d3
	sub.b	d3,d0		*d0=Roland check sum value
	andi.b	#$7f,d0
	bsr	wrt_data
	st.b	d0		*set end code
	bsr	wrt_data
	bra	mml_lp

wrt_send_data:			*転送データをバッファへ
	* < d2.l=0 data max range(127 or 255)
	* > d4.l=data総数
	* X d0-d1,d3-d4,a2
	moveq.l	#0,d3		*init sum
	moveq.l	#0,d4
mmlx_lp00:
	bsr	chk_0_9
	bmi	exit_mmlx_lp
	bsr	get_num
	bsr	skip_sep2
	move.l	d0,d1
	cmp.l	d2,d1
	bls	@f		*値がd2以下ならそのまま転送
mmlx_lp01:			*値がd2以上の時の処理
	move.b	d1,d0
	andi.b	#$7f,d0		*127以下へ
	add.b	d0,d3		*calc total
	bsr	wrt_data
	addq.l	#1,d4
	lsr.l	#7,d1
	beq	mmlx_lp00
	bra	mmlx_lp01
@@:
	move.b	d1,d0
	add.b	d0,d3		*calc total
	bsr	wrt_data
	addq.l	#1,d4
	bra	mmlx_lp00
exit_mmlx_lp:
	rts

cmd_at:				*＠コマンド系
	bsr	chk_0_9
	beq	mml_pgm		*数字が後ろにあるなら音色切り換えへ
	moveq.l	#0,d0
	move.b	(a0)+,d0
	bsr	mk_capital	*d0がalphabetなら大文字にして
	subi.b	#$41,d0		*'A'を引いて
	bmi	m_err19		*英字以外のコマンド
	cmpi.b	#25,d0
	bhi	m_err19
	add.w	d0,d0
	move.w	mml_cmd_at_jmp(pc,d0.w),d0
	jmp	mml_cmd_at_jmp(pc,d0.w)

mml_cmd_at_jmp:
	dc.w	mml_arcc-mml_cmd_at_jmp		*@A ARCC
	dc.w	mml_bend-mml_cmd_at_jmp		*@B detune
	dc.w	mml_arcc_cnf-mml_cmd_at_jmp	*@C ARCC cnf
	dc.w	mml_dumper-mml_cmd_at_jmp	*@D dumper
	dc.w	mml_effect-mml_cmd_at_jmp	*@E effect control
	dc.w	mml_frq-mml_cmd_at_jmp		*@F FRQ change
	dc.w	mml_bnd_rng-mml_cmd_at_jmp	*@G bend range change
	dc.w	mml_ath-mml_cmd_at_jmp		*@H modulation delay
	dc.w	mml_idset-mml_cmd_at_jmp	*@I ID SET
	dc.w	mml_dbq-mml_cmd_at_jmp		*@J special tie mode
	dc.w	mml_detune-mml_cmd_at_jmp	*@K detune
	dc.w	mml_len-mml_cmd_at_jmp		*@L length
	dc.w	mml_modu-mml_cmd_at_jmp		*@M pitch modulation
	dc.w	mml_n2-mml_cmd_at_jmp		*@N assign change
	dc.w	mml_noise-mml_cmd_at_jmp	*@O OPM noise
	dc.w	mml_atp-mml_cmd_at_jmp		*@P midi pan
	dc.w	mml_atq-mml_cmd_at_jmp		*@Q gate
	dc.w	mml_atr-mml_cmd_at_jmp		*@R non key off mode
	dc.w	mml_ats-mml_cmd_at_jmp		*@S modulation speed
	dc.w	mml_timer-mml_cmd_at_jmp	*@T timer value
	dc.w	mml_atu-mml_cmd_at_jmp		*@U velocity
	dc.w	mml_vol-mml_cmd_at_jmp		*@V volume
	dc.w	mml_wait-mml_cmd_at_jmp		*@W wait
	dc.w	mml_atx-mml_cmd_at_jmp		*@X MIDI data send
	dc.w	mml_nrpn-mml_cmd_at_jmp		*@Y NRPN
	dc.w	mml_aftc-mml_cmd_at_jmp		*@Z after touch

mml_pgm:			*音色切り換え
	move.l	#128,d1		*midiはmax128
	tst.b	cv_device(a1)
	bmi	@f
	bne	pgmadpcm
	move.l	#tone_max,d1	*FMはmax200
	bra	@f
pgmadpcm:
	moveq.l	#4,d1
@@:
	bsr	get_num
	tst.l	d0
	beq	m_err39		*illegal sound number
	cmp.l	d1,d0
	bhi	m_err39
	move.b	d0,d1
	move.b	#$a0,d0		*cmd set
	bsr	wrt_data
	bsr	wrt_data_d1	*set pgm number
	bra	mml_lp

mml_bend:			*@bコマンド
	cmpi.b	#1,cv_device(a1)
	beq	m_err30		*ADPCMには関係ないコマンドです
	moveq.l	#0,d4		*数値がない時はｵｰﾄﾍﾞﾝﾄﾞｽｲｯﾁｵﾌのｺﾏﾝﾄﾞに早変わり
	bsr	chk_0_9
	bpl	@f
	pea	mml_lp(pc)	*push return address
	bra	do_wrt_bs	*no @b value
@@:
	bsr	get_num
	move.l	d0,d1
	bsr	chk_k_ovf@b	*chk over flow
	bmi	m_err29		*over flow
	move.l	d1,d2
	asl.l	#6,d2		*d2=d2*64
	divs	#683,d2		*d2=d2/683 (src(fm)d2=-768～768)
	bsr	skip_sep2
	bsr	chk_0_9		*値が１個だけの時は
	bmi	do_wrt_k	*単なるデチューン設定
	move.l	d1,d3		*d3=src(midi) -8192～8191
	bsr	get_num		*destination
	move.l	d0,d1
	bsr	chk_k_ovf@b
	bmi	m_err29		*over flow
	move.l	d1,d4		*d4=dest(midi)
	asl.l	#6,d1		*d1=d1*64
	divs	#683,d1		*d1=d1/683 (dest(fm)d1=-768～768)
atb_:
	sub.w	d2,d1		*相対値に変換(FM)
	sub.w	d3,d4		*相対値に変換(MIDI)
	beq	m_err29		*相対値ゼロはエラー
	bsr	skip_sep2
	bsr	chk_0_9
	bmi	do_wrt_b
	bsr	get_num
	cmpi.l	#32767,d0
	bhi	m_err29
	move.w	d0,cv_bend_dly(a1)
do_wrt_b:
	moveq.l	#$e1,d0
	bsr	wrt_data
	move.w	d2,d0
	bsr	wrt_d0w		*source bend value(fm)
	move.w	d1,d0
	bsr	wrt_d0w		*destination bend value(fm)
	move.w	d3,d0
	bsr	wrt_d0w		*source bend value(midi)
	move.w	d4,d0
	bsr	wrt_d0w		*destination bend value(midi)

	move.w	cv_bend_dly(a1),d0
	bsr	wrt_d0w		*delay value
	moveq.l	#1,d0
	tst.w	d4
	bpl	@f
	neg.b	d0
@@:
	bsr	wrt_data	*ベンド方向
	bra	mml_lp

chk_k_ovf@b:			*デチューン値のover flowチェック
	cmp.l	#-8192,d1
	blt	@f
	cmp.l	#8191,d1
	ble	exit_k_ovf@b
@@:				*case:over flow
	move.w	#8,ccr		*minus
	rts
exit_k_ovf@b:
	move.w	#0,ccr		*plus
	rts

mml_detune:			*@kコマンド(-768～+768)
	cmpi.b	#1,cv_device(a1)
	beq	m_err30		*ADPCMには関係ないコマンドです
	moveq.l	#0,d4		*数値がない時はｵｰﾄﾍﾞﾝﾄﾞｽｲｯﾁｵﾌのｺﾏﾝﾄﾞに早変わり
	bsr	chk_0_9
	bpl	@f
	pea	mml_lp(pc)	*push return address
	bra	do_wrt_bs	*no @k value
@@:
	bsr	get_num		*get source value
	move.l	d0,d2		*FM
	move.l	d0,d1
	bsr	chk_d_value
	bhi	m_err37
	muls	#683,d1
	asr.l	#6,d1
	bsr	chk_k_ovf	*> d1.w=midi detune
	bsr	skip_sep2
	bsr	chk_0_9		*値が１個だけの時は
	bmi	do_wrt_k	*単なるデチューン設定
	move.l	d1,d3		*d3=src(midi) -8192～8191
	bsr	get_num		*destination
	move.l	d0,d1
	move.l	d0,d4
	bsr	chk_d_value
	bhi	m_err37
	muls	#683,d1
	asr.l	#6,d1
	bsr	chk_k_ovf	*> d1.w=midi destination detune
	exg.l	d1,d4		*swap d1,d4
	bra	atb_
do_wrt_k:
	* < d2.w=@k value for fm)
	* < d1.w=@k value for MIDI)
	moveq.l	#$d1,d0		*wrt @k cmd
	bsr	wrt_data
	exg.l	d1,d2
	bra	wrt_d1w_d2w

chk_d_value:
	* X d0.l
	bpl	@f
	neg.l	d0
@@:
	cmpi.l	#64*12*8,d0
	rts

chk_k_ovf:			*デチューン値のover flowチェック
	move.l	d0,-(sp)
	move.l	#-8192,d0
	cmp.l	d0,d1
	blt	@f		*マイナスのmaxを設定
	move.l	#8191,d0
	cmp.l	d0,d1
	ble	exit_k_ovf
@@:				*プラスのmaxを設定
	move.l	d0,d1
	moveq.l	#-1,d0		*warning mark
exit_k_ovf:
	movem.l	(sp)+,d0	*わざとmovem.l
	rts

mml_effect:			*@E エフェクトコントロール
	tst.b	cv_device(a1)
	bpl	m_err16
	moveq.l	#-1,d1
	moveq.l	#-1,d2
	moveq.l	#-1,d3		*一応３パラメータにしておく(後々のため)
	moveq.l	#127,d4
	bsr	chk_0_9
	bmi	@f
	bsr	get_num
	move.l	d0,d1
	cmp.l	d4,d1
	bhi	m_err83
@@:
	bsr	skip_sep2
	bsr	chk_0_9
	bmi	@f
	bsr	get_num
	move.l	d0,d2
	cmp.l	d4,d2
	bhi	m_err83
@@:
	bsr	skip_sep2
	bsr	chk_0_9
	bmi	@f
	bsr	get_num
	move.l	d0,d3
	cmp.l	d4,d3
	bhi	m_err83
@@:
	cmp.l	d1,d2		*全部省略していたらエラー
	bne	@f
	cmp.l	d2,d3
	bne	@f
	cmp.l	d4,d3
	bhi	m_err83
@@:
	moveq.l	#$ed,d0		*cmd code
	bsr	wrt_data
	bra	wrt_d1d2d3	*write param 1,2,3

mml_frq:			*@F ADPCM周波数切り換え
	moveq.l	#4,d1		*default
	bsr	chk_0_9
	bmi	@f
	bsr	get_num
	move.l	d0,d1
	cmpi.l	#31,d1		*7以上はまーきゅりーユニット対応のため
	bhi	m_err80
@@:
	moveq.l	#$a9,d0
	bsr	wrt_data
	bsr	wrt_data_d1
	bra	mml_lp

mml_len:			*@Lコマンド
	tst.b	renp_flg-work(a6)	*連符内にLコマンドは使用出来ない
	bne	m_err44
	bra	get_atl

mml_timer:			*@Tコマンド
	bsr	skip_spc2
	moveq.l	#$92,d2
	move.b	(a0),d0
	cmpi.b	#'-',d0
	beq	mml_rltv_@t1
	cmpi.b	#'+',d0
	beq	mml_rltv_@t2
	bsr	chk_0_9
	bmi	m_err33		*no T value
	bsr	get_num
	move.l	d0,d1
	bmi	m_err33
	move.b	#$90,d0
	bsr	wrt_data	*write cmd
	bra	wrt_d1w
mml_rltv_@t1:
	addq.w	#1,d2
mml_rltv_@t2:
	addq.w	#1,a0
	bsr	chk_0_9
	bmi	m_err33		*no @T value
	bsr	get_num
	move.l	d0,d1
	bmi	m_err33		*@t error
	bsr	wrt_data_d2
	bra	wrt_d1w

mml_atp:			*@pコマンド
	bsr	chk_0_9
	bmi	m_err47
	moveq.l	#$b4,d2		*絶対指定
	cmpi.b	#'+',(a0)
	bne	@f
	moveq.l	#$c8,d2		*相対アップ
	bra	matp0
@@:
	cmpi.b	#'-',(a0)
	bne	@f
	moveq.l	#$c9,d2		*相対ダウン
matp0:
	addq.w	#1,a0		*skip '+','-'
@@:
	bsr	get_num
	move.l	d0,d1
	cmpi.l	#127,d1		*絶対値が１２７以上はエラー
	bhi	m_err47
	bsr	wrt_data_d2	*panpot code
	bsr	wrt_data_d1	*value 0-127
	bra	mml_lp

mml_atq:			*@qコマンド
	move.b	onkai_flg_(pc),onkai_flg-work(a6)
	tst.b	renp_flg-work(a6)	*連符内に@Qコマンドは使用出来ない
	bne	m_err44
	bsr	chk_0_9
	bmi	fixedgtsw			*!2.05
	bsr	get_num
	tst.l	d0
	bne	@f
	moveq.l	#8,d0		*0の場合はQ8と同じ
	bra	set_q
@@:
	cmpi.l	#32768,d0
	bhi	m_err43		*illegal @Q
	neg.w	d0
	move.w	d0,cv_q_com(a1)
fixedgtsw:
	bsr	skip_sep2	*skip ','	*!2.05
	bsr	chk_0_9				*!2.05
	bmi	mml_lp				*!2.05
	bsr	get_num				*!2.05
	tst.l	d0				*!2.05
	beq	@f				*!2.05
	bset.b	#2,cv_cnv_flg(a1)	*@Q mode switch on (fixed gate time)	*!2.05
	bra	mml_lp				*!2.05
@@:
	bclr.b	#2,cv_cnv_flg(a1)	*@Q mode switch off (normal @Q)		*!2.05
	bra	mml_lp				*!2.05

mml_vol:			*@Vコマンド
*	cmpi.b	#1,cv_device(a1)
*	beq	m_err30		*ADPCMには関係ないコマンドです
	bsr	chk_0_9
	bmi	m_err51		*no @V value
	bsr	get_num
	cmpi.l	#127,d0
	bhi	m_err51		*illegal @V
	move.l	d0,d1
	bra	wrt_v_cmd

mml_atx:			*MIDI data send
	tst.b	cv_device(a1)
	bpl	m_err16		*FM/ADPCMには関係ないコマンドです
	tst.b	renp_flg-work(a6)	*連符内には使用出来ない
	bne	m_err44
	bsr	chk_0_9
	bmi	m_err11		*値が一個もない時はｴﾗｰ

	moveq.l	#$ec,d0		*send cmd code
	bsr	wrt_data
	move.l	a2,a4		*save a2 into a4
	bsr	wrt_data	*dummy
	bsr	wrt_data	*dummy
	move.l	#$ff,d2		*max range =$ff
	bsr	wrt_send_data	*d4=data count
	ror.w	#8,d4
	move.b	d4,(a4)+
	ror.w	#8,d4
	move.b	d4,(a4)
	bra	mml_lp

mml_nrpn:				*@Y コマンド
	tst.b	cv_device(a1)
	bpl	m_err16		*FM/ADPCMには関係ないコマンドです
	bsr	chk_0_9
	bmi	m_err82		*値が一個もない時はｴﾗｰ

	moveq.l	#$d2,d0
	bsr	wrt_data

	moveq.l	#$7f,d2
	bsr	wrt_send_data
	cmpi.w	#3,d4		*LSB省略か
	beq	@f
	cmpi.w	#4,d4
	beq	mml_lp
	bra	m_err82		*データがない
@@:
	moveq.l	#-1,d0		*LSBが省略
	bsr	wrt_data
	bra	mml_lp

mml_aftc:			*アフタータッチシーケンス
	cmpi.b	#1,cv_device(a1)
	beq	m_err30		*ADPCMには関係ないコマンドです
	bsr	skip_spc2
	cmpi.b	#',',(a0)
	beq	maftc0
	bsr	chk_0_9
	bpl	maftc0
goto_aftcsof:
	moveq.l	#0,d1
	pea	mml_lp(pc)	*push return address
	bra	do_wrt_aftcs	*数値がない時はスイッチオフと解釈
maftc0:
	move.b	cv_velo2(a1),-(sp)
	lea	aftc_work(pc),a2
	moveq.l	#aftc_max-1,d2
	moveq.l	#-1,d3
mml_aftc_lp01:
	bsr	chk_0_9
	bmi	maftl3
	cmpi.b	#'+',(a0)
	seq	d4		*プラスやマイナス記号がついている時は相対指定
	beq	@f
	cmpi.b	#'-',(a0)
	seq	d4
@@:
	bsr	get_num
	move.l	d0,d1
	bsr	skip_sep2	*skip ','
	tst.b	d4
	beq	@f
				*相対を考慮
	* < d0.b=relative value
	* > d1.b=velocity value
	moveq.l	#0,d1
	move.b	cv_velo2(a1),d1
	add.l	d0,d1
	cmpi.l	#127,d1
	bls	@f
	moveq.l	#0,d1
	tst.l	d0
	bmi	@f
	moveq.l	#127,d1
@@:
	* < d1.b=velocity value
	cmpi.l	#127,d1
	bhi	m_err57		*127より大きいとエラー
	cmp.b	d1,d3
	beq	maftl3
	move.b	d1,(a2)+
	move.b	d1,d3		*save d1 into d3
	move.b	d1,cv_velo2(a1)
	dbra	d2,mml_aftc_lp01
	bra	do_wrt_aftc
maftl3:				*省略したパラメータを$ffとする
	cmpi.b	#',',(a0)
	bne	@f
	addq.w	#1,a0
@@:
	st.b	(a2)+
	dbra	d2,mml_aftc_lp01
do_wrt_aftc:
	move.b	(sp)+,cv_velo2(a1)
	bsr	chk_0_9
	bpl	m_err58		*パラメータ多すぎ
	lea	aftc_work(pc),a4
	cmpi.l	#$00ff_ffff,(a4)	*@z0,0,0,0,0,0,0,0の時は…
	bne	@f
	cmpi.l	#$ffff_ffff,4(a4)
	beq	goto_aftcsof
@@:
	moveq.l	#$e3,d0		*cmd
	bsr	wrt_data
	moveq.l	#aftc_max-1,d2
dwat_lp01:
	move.b	(a4)+,d0
	bsr	wrt_data
	dbra	d2,dwat_lp01
	bra	mml_lp

tie???:				*タイを単独で発見
	moveq.l	#0,d0
	move.b	onkai_flg(pc),d0
	beq	m_err31		*error
	subq.w	#1,d0
	add.w	d0,d0
	pea	mml_lp(pc)
	move.l	(a1),a2		*comiled data address
	move.w	tie_tbl(pc,d0.w),d0
	jmp	tie_tbl(pc,d0.w)
tie_tbl:
	dc.w	t_case_8bit-tie_tbl	*1
	dc.w	t_case_16bit-tie_tbl	*2
	dc.w	t_case_waon-tie_tbl	*3
	dc.w	t_case_port-tie_tbl	*4
	dc.w	t_case_len0-tie_tbl	*5
t_case_8bit:
	st.b	-(a2)
	rts
t_case_16bit:
	st.b	-(a2)
	st.b	-(a2)
	rts
t_case_waon:
	lea	-8-1-2(a2),a2
	st.b	(a2)+
	st.b	(a2)+
	rts
t_case_port:
	subq.w	#1+1+2+2+2,a2
	st.b	(a2)+
	st.b	(a2)+
	rts
t_case_len0:
	move.b	#$cd,-2(a2)
	rts

mml_r:				*休符
	tst.b	renp_flg-work(a6)
	beq	@f
	addq.l	#1,renp_cnt-work(a6)	*連符内なら個数をインクリメント
@@:
	moveq.l	#$80,d0
	bra	knnn

mml_wait:			*@Wコマンド
	tst.b	renp_flg-work(a6)
	beq	@f
	addq.l	#1,renp_cnt-work(a6)	*連符内なら個数をインクリメント
@@:
	moveq.l	#$d0,d0
	bra	knnn

mml_ag:				*キーコード
	* < d1.w=(0～6)*4
	bsr	velocity_seq
	tst.b	renp_flg-work(a6)
	beq	get_kn
	addq.l	#1,renp_cnt-work(a6)	*連符内なら個数をインクリメント
get_kn:
	lsr.w	#1,d1		*/2
	bsr	get_key		*key codeを得る
	cmpi.b	#$7f,d0
	bhi	m_err38		*illegal key code
knnn:
	clr.l	st_buf-work(a6)		*initialize the work
	clr.b	gt_buf-work(a6)		*initialize the work
	move.l	#$ffff_ff00,rinji_buf-work(a6)
	move.b	d0,d2			*save kc
	move.l	#65534,d0
	bsr	get_ag_l0	*音長等のパラメータ取得
	bsr	revive_rinji	*臨時ベロシティ復元処理など
	move.w	st_buf+2(pc),d1
	beq	len0		*length=0?
	cmpi.w	#254,d1		*ショートタイプかロングタイプかを見極める
	bls	short_type
	moveq.l	#$fe,d0		*write cmd code(W step)
	bsr	wrt_data
	bsr	wrt_data_d2	*write kc
	move.w	d1,d0		*d0=d1=step time
	bsr	wrt_d0w		*set step time H L
	bsr	skip_spc2
	cmpi.b	#'&',(a0)	*tieかどうか
	bne	set_gt_
	addq.w	#1,a0
@@:
	moveq.l	#$ff,d0		*tie
	bra	wrt_gt_
wait_gt_:
	moveq.l	#0,d0
	bra	wrt_gt_
set_gt_:
	cmpi.b	#$d0,d2		*@W
	beq	wait_gt_
	cmpi.w	#1,d0
	beq	@b
	andi.l	#$ffff,d0
	bsr	calc_gt		*d0=Qを考慮したカウント数
	move.l	gt_buf(pc),d1
	bpl	@f
	move.l	d1,d0
	bra	wrt_gt_
@@:
	tst.b	renp_flg-work(a6)	*連符内便宜上の処理
	beq	@f
	moveq.l	#0,d0
@@:
wrt_gt_:
	bsr	wrt_d0w		*set gate time H L
	move.b	#2,onkai_flg-work(a6)	*mark onkai_flg
	bra	mml_lp

short_type:			*254以下はショートで足りる
	* < d1.l=abs length
	bsr	wrt_data_d2	*write kc
	move.w	d1,d0
wrt_stt:
	* < d0.b=step time
	bsr	wrt_data	*set step time
	bsr	skip_spc2
	cmpi.b	#'&',(a0)	*tieかどうか
	bne	set_gt
	addq.w	#1,a0		*skip '&'
@@:
	moveq.l	#$ff,d0		*tie
	bra	wrt_gt
wait_gt:
	moveq.l	#0,d0
	bra	wrt_gt
set_gt:
	cmpi.b	#$d0,d2		*@W
	beq	wait_gt
	cmpi.b	#$80,d2		*R
	beq	@f
	cmpi.b	#1,d0		*絶対音長1の時は無条件にTIE
	beq	@b
@@:
	andi.l	#$ff,d0
	bsr	calc_gt		*d0=Qを考慮したカウント数
	move.l	gt_buf(pc),d1
	bpl	@f
	move.l	d1,d0
	bra	wrt_gt
@@:
	tst.b	renp_flg-work(a6)	*連符内便宜上の処理
	beq	@f
	moveq.l	#0,d0
@@:
wrt_gt:
	bsr	wrt_data	*set gate time
	move.b	#1,onkai_flg-work(a6)	*mark onkai_flg
	bra	mml_lp

get_length:			*get 音長
	* > d1.w=length
	* X d0.l
	bsr	get_num		*>d0.l=number value
	tst.l	d0
	beq	m_err27		*0は駄目
	moveq.l	#0,d1
	move.b	mclk(pc),d1
	cmp.l	d1,d0
	bhi	m_err27		*illegal length
	divu	d0,d1
	bvs	m_err27
	rts

futen_ope:			*符点処理
	* < d1.w=length
	* > d0.w=real length
	* X d1.l
	move.w	d1,d0
fop__lp:
	* < d0.w=total length
	* < d1.w=futen sorce length
	bsr	skip_spc2
	cmpi.b	#'.',(a0)
	bne	@f
	addq.w	#1,a0
	tst.b	renp_flg-work(a6)	*連符内に符点指定は出来ない
	bne	m_err44
	lsr.w	#1,d1
	add.w	d1,d0
for_ftlp:
	bcs	m_err27
	cmpi.w	#65534,d0
	bhi	m_err27
	bra	fop__lp
@@:				*PC98式タイ
	bsr	skip_spc2
	cmpi.b	#'^',(a0)
	bne	exit_fto
	addq.w	#1,a0		*skip '^'
	bsr	chk_0_9
	bmi	@f
	move.w	d0,-(sp)
	bsr	get_length	*> d1.l
	move.l	d1,d0
	add.w	(sp)+,d0
	bra	for_ftlp
@@:				*絶対音長指定
	cmpi.b	#'*',(a0)
	bne	@f
	addq.w	#1,a0		*skip '*'
	bsr	chk_0_9
	bmi	m_err34		*no length value
	move.w	d0,-(sp)
	bsr	get_num
	move.l	d0,d1
	add.w	(sp)+,d0
	bra	for_ftlp
@@:				*'^'のみの場合
	move.w	cv_l_com(a1),d1
	add.w	d1,d0
	bra	for_ftlp
exit_fto:
	andi.l	#$ffff,d0
	rts

chk_chogo:			*調号チェック
	* > d1=+1～-1
	moveq.l	#0,d1
chk_chogo_lp:
	bsr	skip_spc2
	cmpi.b	#'#',(a0)
	beq	case_sharp
	cmpi.b	#'+',(a0)
	beq	case_sharp
	cmpi.b	#'-',(a0)
	beq	case_flat
	rts			*none
case_sharp:
	addq.b	#1,d1		*+1
	addq.w	#1,a0
	bra	chk_chogo_lp
case_flat:
	subq.b	#1,d1		*-1
	addq.w	#1,a0
	bra	chk_chogo_lp

get_key:			*キーコードを得る
	* < d1.w=(a～g:0-6)
	* > d0.l=note number
	* - all except d0-d1
	move.l	d2,-(sp)
	moveq.l	#0,d0
	move.b	kc_value(pc,d1.w),d0	*d0=0～11(kc)
	bsr	skip_spc2
	cmpi.b	#'!',(a0)
	bne	@f
	addq.w	#1,a0
	bra	gk0
@@:
	add.b	cv_k_sig(a1,d1.w),d0	*調号考慮
gk0:
	move.b	cv_oct(a1),d1
	add.b	d1,d1		*2
	add.b	d1,d1		*4
	move.b	d1,d2		*d2=d1*4
	add.b	d1,d1		*8
	add.b	d2,d1		*12
	add.b	d1,d0		*d0=オクターブを考慮したキー値(0～127)
	bsr	chk_chogo	*♭・♯のチェック
	add.b	d1,d0
	add.b	cv_ktrans(a1),d0	*キートランスポーズをチェック
	move.l	(sp)+,d2
	rts

kc_value:	*A  B  C  D  E  F  G
	dc.b	09,11,00,02,04,05,07
	.even

len0:				*音長０の特殊ケース
	bsr	skip_spc2
	moveq.l	#$ad,d0
	cmpi.b	#'&',(a0)	*tieかどうか
	bne	@f
	moveq.l	#$cd,d0
	addq.w	#1,a0		*skip'&'
@@:
	bsr	wrt_data
	bsr	wrt_data_d2
	move.b	#5,onkai_flg-work(a6)	*mark onkai_flg
	bra	mml_lp

mml_z:				*velocity sequence
	cmpi.b	#1,cv_device(a1)
	beq	m_err30		*ADPCMには関係ないコマンドです
	bsr	skip_spc2
	bclr.b	#0,cv_cnv_flg(a1)
	cmpi.b	#',',(a0)
	beq	@f
	bsr	chk_0_9
	bmi	mml_lp		*数値が一個もない時はスイッチオフ
@@:
	bset.b	#0,cv_cnv_flg(a1)
	moveq.l	#0,d3
	move.b	cv_velo2(a1),d3
	moveq.l	#velo_max-1,d2
	lea	cv_velo(a1),a2
mml_zlp0:			*相対初期値の取り出し
	tst.b	(a2)+
	bmi	@f
	move.b	-1(a2),d3
@@:
	dbra	d2,mml_zlp0
	lea	cv_velo(a1),a2
	moveq.l	#velo_max-1,d2
mml_z_lp:
	bsr	skip_spc2
	cmpi.b	#',',(a0)
	bne	@f
	st.b	d4
	moveq.l	#0,d0
	bra	mmlz2
@@:
	bsr	chk_0_9
	bmi	exit_z_lp
	cmpi.b	#'+',(a0)
	seq	d4
	beq	mmlz1
	cmpi.b	#'-',(a0)
	seq	d4
mmlz1:
	bsr	get_num
mmlz2:
	move.l	d0,d1
	bsr	skip_sep2	*skip ','
	tst.b	d4
	beq	@f
				*相対を考慮
	move.l	d3,d1
	add.l	d0,d1
	cmpi.l	#127,d1
	bls	@f
	moveq.l	#0,d1
	tst.l	d0
	bmi	@f
	moveq.l	#127,d1
@@:
	cmpi.l	#127,d1		*127より大きいとエラー
	bhi	m_err53
	move.b	d1,(a2)+
	move.l	d1,d3
	dbra	d2,mml_z_lp
	bra	do_mml_z
exit_z_lp:
	st.b	(a2)		*end code
	cmpi.b	#velo_max-2,d2
	bne	do_mml_z
	move.b	cv_velo(a1),d1	*数値が一個の時は@uと同等
	bclr.b	#0,cv_cnv_flg(a1)	*スイッチオフ
	bsr	mml_atu_
	bra	mml_lp
do_mml_z:
	bsr	chk_0_9
	bpl	m_err58		*パラメータ多すぎ
	clr.b	cv_velo_n(a1)	*init pointer
	bra	mml_lp

velocity_seq:			*ベロシティシーケンス(macroコマンド)
	btst.b	#0,cv_cnv_flg(a1)
	beq	not_velo_seq
	movem.l	d0-d1,-(sp)
	moveq.l	#0,d0
	move.b	cv_velo_n(a1),d0
	cmpi.b	#velo_max-1,d0	*maxで戻る
	bhi	new_velo_n
	addq.b	#1,cv_velo_n(a1)
	move.b	cv_velo(a1,d0.w),d1
	bpl	wrt_velo
new_velo_n:
	move.b	#1,cv_velo_n(a1)
	move.b	cv_velo(a1),d1
wrt_velo:
	moveq.l	#$b9,d0		*wrt @u cmd
	bsr	wrt_data
	bsr	wrt_data_d1
	movem.l	(sp)+,d0-d1
not_velo_seq:
	rts

mml_port:			*ポルタメント
	bsr	velocity_seq
	clr.l	st_buf-work(a6)		*initialize the work
	clr.b	gt_buf-work(a6)		*initialize the work
	move.l	#$ffff_ff00,rinji_buf-work(a6)
	cmpi.b	#1,cv_device(a1)
	beq	m_err30		*ADPCMには関係ないコマンドです
	tst.b	renp_flg-work(a6)
	beq	get_1stk
	addq.l	#1,renp_cnt-work(a6)	*個数=個数+1
get_1stk:
	bsr	skip_spc2	*オクターブスイッチの考慮その1
	move.b	(a0)+,d0
	bsr	oct_chk
	bmi	get_1stk
	bsr	get_port_key
	move.l	d0,d3		*save 1st kc
	move.l	#32767,d0
	bsr	get_port_l0	*音長があるならそれをｹﾞｯﾄ
port_lp01:			*オクターブスイッチの考慮その2
	bsr	skip_sep2
	move.b	(a0)+,d0
	bsr	oct_chk
	bmi	port_lp01
	bsr	get_port_key
	move.l	d0,d4		*save 2nd kc
	move.l	#32767,d0
	bsr	get_port_l0
ushiro_port:			*最後のオクターブスイッチの考慮
	bsr	skip_spc2
	move.b	(a0)+,d0
	bsr	oct_chk
	bmi	ushiro_port
	subq.w	#1,a0
	bsr	skip_spc2
	cmpi.b	#')',(a0)+
	bne	m_err52		*portament error ')'が無い
	bsr	skip_spc2
	cmpi.b	#'*',(a0)
	bne	@f
	addq.w	#1,a0		*skip	'*'
@@:
	bsr	chk_0_9
	bpl	get_port_l
	moveq.l	#0,d0		*音長省略のケース
	move.w	st_buf+2(pc),d0
	bra	port_delay?
get_port_l:
	tst.b	renp_flg-work(a6)
	bne	m_err44		*連符内に音長指定は不可能
	bsr	get_num
port_delay?:
	move.w	d0,d0_work-work(a6)	*save step
	bsr	skip_spc2
	cmpi.b	#',',(a0)
	bne	do_wrt_port	*デフォルトのディレイを使用する
	bsr	skip_sep2
	cmpi.b	#'*',(a0)
	bne	@f
	addq.w	#1,a0		*skip	'*'
@@:
	bsr	chk_0_9
	bmi	m_err52		*ディレイがない
	bsr	get_num
	cmpi.l	#32767,d0
	bhi	m_err27
	move.w	d0,cv_port_dly(a1)	*save delay
do_wrt_port:			*バッファへの書き込み
	* < d3.l=src kc
	* < d4.l=dest kc
	* < d0_work.w=step time
	* < cv_port_dly(a1).w=delay count
	move.w	d0_work(pc),d0
	beq	m_err27		*0は駄目
	cmpi.w	#32767,d0
	bhi	m_err27		*illegal l
	sub.w	cv_port_dly(a1),d0
	bls	m_err59
				*まずステップカウンタの計算をする
	move.l	d4,d1
	sub.l	d3,d1
	move.l	d1,-(sp)
	tst.b	cv_device(a1)
	bpl	calc_stc_fm	*FMのケース
	muls	#683,d1		*MIDIのケース
	bsr	chk_k_ovf
	bra	calc_stc
calc_stc_fm:
	asl.w	#6,d1		*64倍
calc_stc:
	divs	d0,d1		*d1=range/L  d1.w=step counter
	move.l	d1,d2
	clr.w	d2
	swap	d2		*d2.w=余り
	tst.w	d2
	bpl	_256bai
	neg.w	d2		*d2.w=絶対値
_256bai:
	lsl.l	#8,d2		*d2=d2*256
	divu	d0,d2		*d2=d2/L
	swap	d2
	tst.w	d2
	beq	modosu_revise
	add.l	#$0001_0000,d2
modosu_revise:
	swap	d2		*d2.b=revise parameter

	bsr	revive_rinji	*臨時ベロシティ復元処理など
	moveq.l	#$e0,d0		*portament cmd set
	bsr	wrt_data
	bsr	wrt_data_d3	*wrt 1st kc
*	move.b	d4,d0
*	bsr	wrt_data	*wrt 2nd kc(必要無くなっちゃった...)
	move.w	d0_work(pc),d0
	bsr	wrt_d0w		*set step
	bsr	skip_spc2
	cmpi.b	#'&',(a0)	*tie?
	beq	port_tie
	move.w	d0_work(pc),d0
	cmpi.l	#1,d0
	beq	port_tie_
	bsr	calc_gt		*calc gate time
	move.l	gt_buf(pc),d3
	bpl	@f
	move.l	d3,d0
	bra	set_port_gt
@@:
	tst.b	renp_flg-work(a6)	*連符内便宜上の処理
	beq	@f
	moveq.l	#0,d0
@@:
	bra	set_port_gt
port_tie:
	addq.w	#1,a0
port_tie_:
	moveq.l	#-1,d0		*tie gate time
set_port_gt:
	bsr	wrt_d0w		*wrt gate time
	move.w	cv_port_dly(a1),d0	*wrt delay
	bsr	wrt_d0w
	move.w	d1,d0
	bsr	wrt_d0w		*wrt step counter
	bsr	wrt_data_d2	*wrt revise parameter
	moveq.l	#1,d0
	tst.l	(sp)+
	bpl	@f
	neg.b	d0		*ベンド方向
@@:
	bsr	wrt_data
	move.b	#4,onkai_flg-work(a6)	*mark onkai_flg
	bra	mml_lp

get_port_key:
	* > d0.b=kc
	subq.w	#1,a0
	bsr	skip_sep2
	move.b	(a0)+,d0
	bsr	mk_capital
	sub.b	#'A',d0
	cmpi.b	#6,d0
	bhi	m_err38		*illegal key code
	moveq.l	#0,d1
	move.b	d0,d1
	bsr	get_key		*キーコードを得る
	cmpi.b	#127,d0
	bhi	m_err38
exit_gpk:
	rts

oct_chk:
	* > mi=again
	bsr	mk_capital
	cmpi.b	#'O',d0
	bne	@f
	bsr	get_num
	addq.l	#1,d0		*d0=0～10
	cmpi.l	#10,d0
	bhi	m_err26		*illegal octave
	move.b	d0,cv_oct(a1)
	bra	oct_chk_again
@@:
	cmp.b	#OUP,d0
	bne	@f
	bsr	do_oup
	bra	oct_chk_again
@@:
	cmp.b	#ODWN,d0
	bne	@f
	bsr	do_odwn
	bra	oct_chk_again
@@:
	tst.b	d0
	rts
oct_chk_again:
	moveq.l	#-1,d0
	rts

do_oup:				*オクターブアップ
	cmpi.b	#10,cv_oct(a1)
	beq	m_err26		*illegal octave
	addq.b	#1,cv_oct(a1)
	rts

do_odwn:			*オクターブダウン
	tst.b	cv_oct(a1)
	beq	m_err26		*illegal octave
	subq.b	#1,cv_oct(a1)
	rts

kill_kon:			*強制キーオフコマンド
	moveq.l	#$bf,d0
	bsr	wrt_data
	bra	mml_lp

mml_waon:			*和音コマンド
	bsr	velocity_seq
	cmpi.b	#1,cv_device(a1)
	beq	m_err30		*ADPCMには関係ないコマンドです
	tst.b	renp_flg-work(a6)
	beq	init_waon_wk
	addq.l	#1,renp_cnt-work(a6)	*個数=個数+1
init_waon_wk:
	moveq.l	#0,d4		*note cnt
	lea	waon_buf(pc),a2
	move.l	#-1,(a2)
	move.l	(a2),4(a2)		*ワーク初期化
	clr.l	st_buf-work(a6)		*initialize the work
	clr.b	gt_buf-work(a6)		*initialize the work
	move.l	#$ffff_ff00,rinji_buf-work(a6)
	move.b	cv_oct(a1),oct_wk-work(a6)	*一時保存
get_waon_k:
	bsr	skip_spc2
	move.b	(a0)+,d0
	cmpi.b	#"'",d0
	beq	exit_gwk	*終端コード発見
	bsr	oct_chk
	bmi	get_waon_k
	bsr	get_port_key
	move.l	d0,d1		*save kc
ato_waon_k:			*キーの後ろのオクターブスイッチの考慮
	move.l	#65534,d0
	bsr	get_waon_l0	*数値があるならそれを音長に
waon_oct2:
	bsr	skip_spc2
	move.b	(a0)+,d0
	bsr	oct_chk
	bmi	waon_oct2
	addq.b	#1,d4
	cmpi.b	#max_note_on,d4
	bhi	m_err50
	move.b	d1,(a2)+	*得たキーコードをワークへ書く
	cmpi.b	#"'",d0
	beq	exit_gwk	*終端コード発見
	subq.w	#1,a0
	bra	get_waon_k	*ループする
exit_gwk:
	tst.b	d4
	beq	m_err49		*音符が一個もない
	bsr	skip_spc2
	cmpi.b	#'*',(a0)
	bne	@f
	addq.w	#1,a0		*skip	'*'
@@:
	bsr	chk_0_9
	bpl	get_waon_l
	moveq.l	#0,d0		*音長省略のケース
	move.w	st_buf+2(pc),d0
	bra	waon_delay?
get_waon_l:
	tst.b	renp_flg-work(a6)
	bne	m_err44		*連符内に音長指定は不可
	bsr	get_num
waon_delay?:
	move.w	d0,d0_work-work(a6)	*save step
	beq	m_err49		*0は駄目
	bsr	skip_spc2
	cmpi.b	#',',(a0)
	bne	do_wrt_waon	*デフォルトのディレイを使用する
	bsr	skip_sep2
	cmpi.b	#'*',(a0)
	bne	@f
	addq.w	#1,a0		*skip	'*'
@@:
	bsr	chk_0_9
	bmi	m_err49		*ディレイがない
	bsr	get_num
	cmpi.l	#255,d0
	bhi	m_err27
	move.b	d0,cv_waon_dly(a1)	*save delay
do_wrt_waon:			*バッファへの書き込み
	* < d4.l=number of notes
	* < d0_work.w=step time
	* < cv_waon_dly(a1).b=delay count
	moveq.l	#0,d2
	move.w	d0_work(pc),d2
	beq	m_err27
	cmpi.w	#65534,d2
	bhi	m_err27		*illegal l
	moveq.l	#0,d0
	move.b	cv_waon_dly(a1),d0
	subq.w	#1,d4
	mulu	d4,d0
	cmp.w	d2,d0
	bcc	m_err59		*delay too long
	bsr	revive_rinji	*臨時ベロシティ復元処理など
	moveq.l	#$e2,d0		*waon cmd set
	bsr	wrt_data
	move.w	d2,d0
	bsr	wrt_d0w		*set step
	bsr	skip_spc2
	cmpi.b	#'&',(a0)	*tie?
	beq	waon_tie
	move.l	d2,d0
	cmpi.l	#1,d0		*絶対音長１のときは強制的にタイ
	beq	waon_tie
	bsr	calc_gt		*calc gate time
	move.l	gt_buf(pc),d1
	bpl	@f
	move.l	d1,d0
	bra	set_waon_gt
@@:
	tst.b	renp_flg-work(a6)	*連符内便宜上の処理
	beq	@f
	moveq.l	#0,d0
@@:
	bra	set_waon_gt
waon_tie:
	moveq.l	#-1,d0		*tie gate time
set_waon_gt:
	bsr	wrt_d0w			*wrt gate time
	move.b	cv_waon_dly(a1),d0	*wrt delay
	bsr	wrt_data
	lea	waon_buf(pc),a4
	moveq.l	#max_note_on-1,d1
wrt_waon_lp:
	move.b	(a4)+,d0
	bsr	wrt_data
	dbra	d1,wrt_waon_lp
	move.b	oct_wk(pc),cv_oct(a1)
	move.b	#3,onkai_flg-work(a6)	*mark onkai_flg
	bra	mml_lp

get_ag_l0:
get_port_l0:
get_waon_l0:			*音符の後ろに音長のあるケース
	* < d0.l=step time max
	* > d0.w=step time
	movem.l	d1-d4/a2,-(sp)
	move.l	d0,d2
	bsr	skip_spc2
	cmpi.b	#wstep,(a0)
	bne	ongaku_l
					*絶対音長指定
	addq.w	#1,a0			*skip '*'
	bsr	chk_oncho
	bmi	m_err34			*数値がない
	tst.b	renp_flg-work(a6)	*連符内に音長指定は出来ない
	bne	m_err44
	bsr	get_num			*絶対音長指定
	move.l	d0,d1
	bra	@f
ongaku_l:
	bsr	chk_oncho
	bmi	get_def_l		*音長省略のケース(デフォルトの取得)
	tst.b	renp_flg-work(a6)	*連符内に音長指定は出来ない
	bne	m_err44
	bsr	get_length		*音楽音長
@@:
	st.b	st_buf-work(a6)		*mark
get_gate?:
	bsr	futen_ope		* < d1.w=step source
	cmp.l	d2,d0			*最大値チェック
	bhi	m_err27
	move.w	d0,st_buf+2-work(a6)
	clr.b	gt_buf-work(a6)		*ゲートタイム無効化
get_gate?_:
	bsr	skip_spc2
	cmpi.b	#',',(a0)
	bne	sv_wl
	addq.w	#1,a0			*skip ','
	bsr	chk_oncho		*ゲートタイムがあればゲット
	bmi	gt_direct?
	bsr	get_length
gt_futen?:
	bsr	futen_ope		*符点を考慮
	move.w	d0,gt_buf+2-work(a6)
	st.b	gt_buf-work(a6)		*flag on
mmlag_velo?:
	bsr	skip_spc2		*ベロシティがあればゲット
	cmpi.b	#',',(a0)
	bne	sv_wl
	addq.w	#1,a0			*skip ','
	bsr	chk_0_9
	bmi	sv_wl
	btst.b	#0,cv_cnv_flg(a1)		*ベロシティシーケンス中？
	beq	@f
	subq.l	#2,(a3)			*バック
	subq.l	#2,(a1)
@@:					*臨時ベロシティーコマンド生成
	cmpi.b	#'-',(a0)		*相対チェック
	beq	@f
	cmpi.b	#'+',(a0)
	bne	exc_velo_dn
				*up
	bsr	get_num
	tst.l	d0
	beq	m_err53
	moveq.l	#$da,d1
	bra	wev0
@@:				*down
	bsr	get_num
	neg.l	d0
	beq	m_err53
	moveq.l	#$db,d1
	bra	wev0
exc_velo_dn:
	bsr	get_num
	moveq.l	#$d9,d1		*直値
wev0:
	exg.l	d0,d1
	cmpi.l	#127,d1
	bhi	m_err53
	lea	rinji_buf(pc),a2
	move.b	d0,(a2)+
	move.b	d1,(a2)+
	st.b	(a2)
	bset.b	#1,cv_cnv_flg(a1)
	bra	@f
sv_wl:
	tst.b	rinji_flg-work(a6)
	bne	@f
	bclr.b	#1,cv_cnv_flg(a1)
	beq	@f
	lea	rinji_buf(pc),a2
	move.w	#$84ff,(a2)+	*臨時ベロシティ後処理
@@:
	addq.b	#1,rinji_flg-work(a6)
	movem.l	(sp)+,d1-d4/a2
	rts
gt_direct?:
	cmpi.b	#wstep,(a0)	*直接音長指定?
	bne	mmlag_velo?
	addq.w	#1,a0		*skip '*'
	bsr	chk_oncho
	bmi	m_err34		*数値がない
	bsr	get_num
	move.l	d0,d1
	beq	m_err27		*gate time=0はダメ
	bra	gt_futen?
get_def_l:
	bsr	skip_spc2
	cmpi.b	#'.',(a0)
	bne	@f
	st.b	st_buf-work(a6)	*付点がある場合は音長指定があったのと同等扱い
	bra	gdl0
@@:
	tst.b	st_buf-work(a6)
	bmi	get_gate?_
gdl0:
	move.w	cv_l_com(a1),d1	*get default length
	bra	get_gate?

revive_rinji:
	movem.l	d0/a2/a4,-(sp)
	lea	rinji_buf(pc),a4
@@:
	move.b	(a4)+,d0
	cmpi.b	#-1,d0
	beq	@f
	bsr	wrt_data
	bra	@b
@@:
	movem.l	(sp)+,d0/a2/a4
	rts

do_e_velo:			*臨時ベロシティ解除コードの挿入 #1
	bclr.b	#1,cv_cnv_flg(a1)
	beq	@f
	moveq.l	#$84,d0
	bra	wrt_data
@@:
	rts

mml_dbq:			*ＭＩＤＩのスペシャル・タイ・モードの設定
	tst.b	cv_device(a1)
	bpl	m_err16		*FM/ADPCMには関係ないコマンドです
	bsr	chk_0_9
	bpl	@f
	moveq.l	#0,d0
	bra	mst0
@@:
	bsr	get_num
mst0:
	move.l	d0,d1
	moveq.l	#$c5,d0
	bsr	wrt_data
	bsr	wrt_data_d1
	bra	mml_lp

mml_switch:			*特殊コマンドのオンオフスイッチ
	cmpi.b	#1,cv_device(a1)
	beq	m_err30		*adpcmに関係ない
	bsr	chk_0_9
	bpl	get_sw
	moveq.l	#0,d0
	bra	chk_sw_n
get_sw:
	bsr	get_num
chk_sw_n:
	bclr.b	#0,cv_cnv_flg(a1)	*ﾍﾞﾛｼﾃｨｼｰｹﾝｽ･ｱｸﾃｨｳﾞ･ｽｲｯﾁ初期化
	btst.l	#4,d0
	beq	@f
	bset.b	#0,cv_cnv_flg(a1)	*ﾍﾞﾛｼﾃｨｼｰｹﾝｽ･ｱｸﾃｨｳﾞ･ｽｲｯﾁ設定
@@:
	move.l	d0,d1
	moveq.l	#$c7,d0
	bsr	wrt_data
	bsr	wrt_data_d1
	bra	mml_lp

do_wrt_ms:			*ﾋﾟｯﾁﾓｼﾞｭﾚｰｼｮﾝスイッチ
	* < d2=switch
	moveq.l	#$bb,d0
	bsr	wrt_data
	bra	wrt_data_d2
do_wrt_as:			*ARCCスイッチ
	* < d3=switch
	moveq.l	#$bc,d0
	bsr	wrt_data
	bra	wrt_data_d3
do_wrt_bs:			*ｵｰﾄﾍﾞﾝﾄﾞスイッチ
	* < d4=switch
	moveq.l	#$bd,d0
	bsr	wrt_data
	bra	wrt_data_d4

do_wrt_aftcs:			*ｱﾌﾀｰﾀｯﾁｼｰｹﾝｽ
	* < d1=switch
	moveq.l	#$be,d0
	bsr	wrt_data
	bra	wrt_data_d1

mml_modu:			*ピッチモジュレーション
	cmpi.b	#1,cv_device(a1)
	beq	m_err30		*ADPCMには関係ないコマンドです
	bsr	skip_spc2
	cmpi.b	#',',(a0)
	beq	mmodu0
	bsr	chk_0_9
	bpl	mmodu0
goto_msof:
	moveq.l	#0,d2
	pea	mml_lp(pc)	*push return address
	bra	do_wrt_ms	*数値がない時はスイッチオフと解釈
mmodu0:
	lea	modu_work(pc),a2
	moveq.l	#0,d2
	moveq.l	#0,d3
	moveq.l	#-1,d4
mml_modu_lp01:
	moveq.l	#0,d1
	bsr	chk_0_9
	bpl	@f
	cmpi.b	#',',(a0)
	bne	mmodu3
	addq.w	#1,a0
	bra	mmodu3
@@:
	cmpi.b	#'+',(a0)
	beq	rltv_mod
	cmpi.b	#'-',(a0)
	bne	@f
rltv_mod:
	bsr	get_num
	move.l	d3,d1
	add.l	d0,d1
	move.l	d1,d0
	bra	chk_msz
@@:
	bsr	get_num
chk_msz:
	move.l	d0,d1
	cmpi.l	#32767,d0
	bgt	m_err55		*32767より大きいとエラー
	cmpi.l	#-32768,d0
	blt	m_err55		*-32768より小さいとエラー
	bsr	skip_sep2	*skip ','
	bsr	chk_0_9
	bpl	@f
	cmpi.b	#',',(a0)	*まだパラメータがあるか
	beq	@f
	tst.b	d2		*たった１個のパラメータで終わりならば
	beq	mml_modu2
@@:
	* < d1.b=modulation value
	tst.b	d2
	beq	@f		*初めのパラメータの場合は連続チェック省略
	cmp.l	d1,d3		*同じものが連続しているか
	bne	@f
mmodu3:
	bclr.l	d2,d4
@@:
	move.w	d1,(a2)+
	move.l	d1,d3		*save d1 into d3
	addq.b	#1,d2
	cmpi.b	#modu_max,d2
	bcs	mml_modu_lp01

	bsr	chk_0_9
	bpl	m_err58		*パラメータ多すぎ
				*@m0,0,0,0,0,0,0,0の時は…
	lea	modu_work(pc),a4
	tst.w	(a4)
	bne	@f
	cmpi.b	#%0000_0001,d4
	beq	goto_msof
@@:
	moveq.l	#$ee,d0		*cmd
	bsr	wrt_data
	bsr	wrt_data_d4	*omt
	moveq.l	#modu_max-1,d2
dwmd_lp01:
	move.w	(a4)+,d0
	bsr	wrt_d0w
	dbra	d2,dwmd_lp01
	bra	mml_lp

mml_modu2:			*1個のパラメータのケース
	* < d1.l=modulation depth value
	move.l	d1,d0
	beq	goto_msof
	bpl	@f
	cmpi.l	#-32768,d0
	bge	wrt_md2
@@:
	cmpi.l	#32767,d0
	bgt	m_err55
wrt_md2:
	moveq.l	#$e6,d0
	bsr	wrt_data
	bra	wrt_d1w

mml_ath:				*モジュレーションディレイ
	cmpi.b	#1,cv_device(a1)
	beq	m_err30		*ADPCMには関係ないコマンドです
	move.w	#-1,d1
	move.w	#-1,d2
	bsr	skip_spc2
	cmpi.b	#',',(a0)
	beq	mml_ath2		*最初のパラメータが省略
	bsr	chk_0_9
	bmi	m_err17		*数字がない
	bsr	get_num
	move.l	d0,d1
	bmi	m_err17		*規定外
	cmpi.l	#65534,d1
	bhi	m_err17		*規定外
mml_ath2:
	bsr	skip_sep2	*skip ','
	bsr	chk_0_9
	bmi	mml_ath3		*数字がないので省略と見なす
	bsr	get_num
	move.l	d0,d2
	bmi	m_err17		*規定外
	cmpi.l	#65534,d2
	bhi	m_err17		*規定外
mml_ath3:
	moveq.l	#$e8,d0
	bsr	wrt_data
wrt_d1w_d2w:
	move.l	d1,d0
	bsr	wrt_d0w
	move.l	d2,d0
	bsr	wrt_d0w
	bra	mml_lp

mml_ats:				*モジュレーションスピード
	move.l	#spd_max,d4
	cmpi.b	#1,cv_device(a1)
	beq	m_err30		*ADPCMには関係ないコマンドです
	moveq.l	#0,d1
	bsr	chk_0_9
	bmi	@f
	bsr	get_num
	move.l	d0,d1
	beq	m_err18		*規定外
	cmp.l	d4,d1
	bhi	m_err18		*規定外
@@:
	moveq.l	#0,d2
	bsr	skip_sep2	*skip ','
	bsr	chk_0_9
	bmi	@f
	bsr	get_num
	move.l	d0,d2
	beq	m_err18		*規定外
	cmp.l	d4,d2
	bhi	m_err18		*規定外
@@:
	move.l	d1,d0
	or.l	d2,d0
	beq	m_err18		*両方省略はダメ
	moveq.l	#$d6,d0
	bsr	wrt_data
	bra	wrt_d1w_d2w	*d1.w d2.wの書き込み

mml_arcc:			*アサイナブルリアルタイムコントロールチェンジ
	cmpi.b	#1,cv_device(a1)
	beq	m_err30		*ADPCMには関係ないコマンドです
	bsr	skip_spc2
	cmpi.b	#',',(a0)
	beq	marcc0
	bsr	chk_0_9
	bpl	marcc0
goto_asof:
	moveq.l	#0,d3
	pea	mml_lp(pc)	*push return address
	bra	do_wrt_as	*数値がない時はスイッチオフと解釈
marcc0:
	lea	modu_work(pc),a2
	moveq.l	#0,d2
	moveq.l	#0,d3
	moveq.l	#-1,d4
mml_arcc_lp01:
	moveq.l	#0,d1
	bsr	chk_0_9
	bpl	@f
	cmpi.b	#',',(a0)
	bne	marcc3
	addq.w	#1,a0
	bra	marcc3
@@:
	cmpi.b	#'+',(a0)
	beq	rltv_arcc
	cmpi.b	#'-',(a0)
	bne	@f
rltv_arcc:
	bsr	get_num
	move.l	d3,d1
	add.l	d0,d1
	move.l	d1,d0
	bra	chk_asz
@@:
	bsr	get_num
chk_asz:
	move.l	d0,d1
	cmpi.l	#127,d0
	bgt	m_err15		*127より大きいとエラー
	cmpi.l	#-127,d0
	blt	m_err15		*-128より小さいとエラー
	bsr	skip_sep2	*skip ','
	bsr	chk_0_9
	bpl	@f		*複数のパラメータ
	cmpi.b	#',',(a0)
	beq	@f
	tst.b	d2		*たった１個のパラメータで終わりならば
	beq	fmamod
@@:
	* < d1.b=arcc value
	tst.b	d2
	beq	@f		*最初のパラメータは連続チェックしない
	cmp.l	d1,d3		*同じものが連続しているか
	bne	@f
marcc3:
	bclr.l	d2,d4
@@:
	move.b	d1,(a2)+
	move.l	d1,d3		*save d1 into d3
	addq.b	#1,d2
	cmpi.b	#modu_max,d2
	bcs	mml_arcc_lp01

	bsr	chk_0_9
	bpl	m_err58		*パラメータ多すぎ
				*@a0,0,0,0,0,0,0,0の時は…
	lea	modu_work(pc),a4
	tst.b	(a4)
	bne	@f
	cmpi.b	#%0000_0001,d4
	beq	goto_asof
@@:
	moveq.l	#$ef,d0		*cmd
	bsr	wrt_data
	bsr	wrt_data_d4	*omt
	moveq.l	#modu_max-1,d2
dwam_lp01:
	move.b	(a4)+,d0
	bsr	wrt_data
	dbra	d2,dwam_lp01
	bra	mml_lp

fmamod:				*AMD
	tst.b	d1
	beq	goto_asof
	moveq.l	#$ae,d0
	bsr	wrt_data
	bsr	wrt_data_d1
	bra	mml_lp

mml_arcc_cnf:			*ARCCコンフィギュレーション
	moveq.l	#-1,d1
	moveq.l	#127,d4
	bsr	chk_0_9
	bmi	@f
	bsr	get_num
	move.l	d0,d1
	cmp.l	d4,d1
	bhi	m_err14		*規定外
@@:
	moveq.l	#-1,d2
	bsr	skip_sep2	*skip ','
	bsr	chk_0_9
	bmi	@f
	bsr	get_num
	move.l	d0,d2
	cmp.l	d4,d2
	bls	@f
	moveq.l	#128,d2		*設定しないケース
@@:
	moveq.l	#-1,d3
	bsr	skip_sep2	*skip ','
	bsr	chk_0_9
	bmi	@f
	bsr	get_num
	move.l	d0,d3
	cmp.l	d4,d3
	bhi	m_err14		*規定外
@@:
	move.l	d1,d0
	and.l	d2,d0
	and.l	d3,d0
	bmi	m_err14		*両方省略はダメ
	moveq.l	#$9a,d0
	bsr	wrt_data
	bra	wrt_d1d2d3

fo_set:				*FADE OUT SET
	bsr	chk_0_9
	bpl	@f
	moveq.l	#0,d1
	bra	do_wrt_fost	*数字がない時はオフ
@@:
	bsr	get_num
	move.l	d0,d1
	bpl	@f
	neg.l	d0
@@:
	cmpi.l	#fo_max,d0	*絶対値チェック
	bhi	m_err78
do_wrt_fost:
	moveq.l	#$a6,d0
	bsr	wrt_data
	bsr	wrt_data_d1
	bra	mml_lp

mml_noise:
	bsr	chk_0_9
	bmi	mml_n_off
	bsr	get_num
	move.l	d0,d1
	moveq.l	#$a5,d0		*noise cmd
	bsr	wrt_data
	tas.b	d1
	bsr	wrt_data_d1
	bra	mml_lp
mml_n_off:
	moveq.l	#$82,d0		*noise mode off
	bsr	wrt_data
	bra	mml_lp

vol_dwn:			*相対ボリュームダウン
*	cmpi.b	#1,cv_device(a1)
*	beq	m_err30		*ADPCMには関係ないコマンドです
	bsr	chk_0_9
	bmi	rltv_vd_dflt
	bsr	get_num
	move.l	d0,d1
	bmi	m_err13
	move.b	d1,cv_rltv_vol(a1)
	bra	@f
rltv_vd_dflt:
	moveq.l	#0,d1
	move.b	cv_rltv_vol(a1),d1
@@:
	moveq.l	#'_',d0
	bsr	rvm
	moveq.l	#$ab,d0
	tst.b	rltv_uv_mode-work(a6)
	beq	@f
	add.b	#$20,d0
@@:
	bsr	wrt_data
	bsr	calc_master
	move.l	d1,d0
	tst.b	cv_device(a1)
	bmi	@f
	sub.b	d1,cv_velo2(a1)
	bmi	rvd0
	bcc	@f
rvd0:
	moveq.l	#0,d1
	move.b	d1,cv_velo2(a1)
@@:
	bsr	wrt_data
	bra	mml_lp

vol_up:				*相対ボリュームアップ
*	cmpi.b	#1,cv_device(a1)
*	beq	m_err30		*ADPCMには関係ないコマンドです
	bsr	chk_0_9
	bmi	rltv_vu_dflt
	bsr	get_num
	move.l	d0,d1
	bmi	m_err13
	move.b	d1,cv_rltv_vol(a1)
	bra	@f
rltv_vu_dflt:
	moveq.l	#0,d1
	move.b	cv_rltv_vol(a1),d1
@@:
	moveq.l	#'~',d0
	bsr	rvm
	moveq.l	#$aa,d0
	tst.b	rltv_uv_mode-work(a6)
	beq	@f
	add.b	#$20,d0
@@:
	bsr	wrt_data
	bsr	calc_master
	move.l	d1,d0
	tst.b	cv_device(a1)
	bmi	@f
	add.b	d1,cv_velo2(a1)
	bmi	rvu0
	bcc	@f
rvu0:
	moveq.l	#127,d1
	move.b	d1,cv_velo2(a1)
@@:
	bsr	wrt_data
	bra	mml_lp
rvm:
	* < d0.b='~','_'
	moveq.l	#1,d2
vdlp:
	bsr	skip_spc2
	cmp.b	(a0)+,d0
	bne	@f
	bsr	chk_0_9
	bpl	@f
	addq.w	#1,d2
	bra	vdlp
@@:
	subq.w	#1,a0
	mulu	d2,d1
	rts

mml_dumper:
	bsr	chk_0_9
	bpl	mmldmp1
	moveq.l	#0,d1
	bra	do_wrt_dmp
mmldmp1:
	bsr	get_num
	move.l	d0,d1
	beq	do_wrt_dmp
	moveq.l	#127,d1		*0か127かのどちらかに直す
do_wrt_dmp:
	moveq.l	#$a7,d0
	bsr	wrt_data
	bsr	wrt_data_d1
	bra	mml_lp

mml_atr:
	bsr	chk_0_9
	bpl	@f
	moveq.l	#0,d1
	bra	do_wrt_nof
@@:
	bsr	get_num
	tst.l	d0
	sne	d1
do_wrt_nof:
	moveq.l	#$ac,d0
	bsr	wrt_data
	bsr	wrt_data_d1
	bra	mml_lp

mml_bnd_rng:			*ベンドレンジチェンジ
	tst.b	cv_device(a1)
	bpl	m_err16		*内蔵音源には関係ないコマンドです
	bsr	chk_0_9
	bmi	m_err81		*値がない
	bsr	get_num
	move.l	d0,d1
	cmpi.l	#127,d1
	bhi	m_err81
	moveq.l	#$a8,d0
	bsr	wrt_data
	bsr	wrt_data_d1
	bra	mml_lp

mml_i:				*音色バンク切り換え
	tst.b	cv_device(a1)
	bpl	mml_pgm		*FM/ADPCMでは音色切り換えと同じ
	moveq.l	#0,d1
	moveq.l	#0,d2
	bsr	chk_0_9
	bmi	@f
	bsr	get_num
	move.l	d0,d1
	cmpi.l	#127,d1
	bhi	mk_bank_i	*127以上は2バイトへ
@@:
	bsr	skip_sep2
	bsr	chk_0_9
	bmi	do_wrt_i	*第２パラメータがない
	bsr	get_num
	move.l	d0,d2
	cmpi.l	#127,d2
	bhi	m_err9		*too big
	bra	do_wrt_i
mk_bank_i:			*2バイト目を作る
	move.l	d1,d2
	moveq.l	#127,d0
	and.b	d0,d2
	lsr.w	#7,d1
	and.b	d0,d1
do_wrt_i:
	moveq.l	#$d3,d0
	bsr	wrt_data
	bsr	wrt_data_d1	*H
	bsr	wrt_data_d2	*L
	bra	mml_lp

mml_j:				*強制再演奏
	bsr	chk_0_9
	bmi	m_err46
	bsr	get_num
	move.l	d0,d1
	subq.l	#1,d1
	cmpi.l	#tr_max-1,d1
	bhi	m_err46
	moveq.l	#$ce,d0
	bsr	wrt_data
	bsr	wrt_data_d1
	bra	mml_lp

mml_direct:			*; コマンド
	tst.b	renp_flg-work(a6)	*連符内には使用出来ない
	bne	m_err44
	bsr	chk_0_9
	bmi	m_err77		*値が一個もない時はｴﾗｰ
@@:
	bsr	get_num
	cmpi.l	#255,d0
	bhi	m_err77
	bsr	wrt_data
	bsr	skip_sep2
	bsr	chk_0_9
	bmi	mml_lp		*END
	bra	@b

mml_poke:			*? コマンド
	bsr	do_e_velo	*臨時ベロシティ後処理
	bsr	chk_0_9
	bmi	m_err79		*値が一個もない時はｴﾗｰ
	bsr	get_num
	move.l	d0,d1
	cmpi.l	#wk_size-1,d1
	bhi	m_err79

	bsr	skip_sep2	*skip ','

	bsr	chk_0_9
	bmi	m_err79		*値が一個もない時はｴﾗｰ
	cmpi.b	#'-',(a0)
	beq	@f
	cmpi.b	#'+',(a0)
	bne	poke_imed
				*up
	bsr	get_num
	move.l	d0,d2
	beq	m_err79
	moveq.l	#$d7,d0
	bra	poke0
@@:				*down
	bsr	get_num
	neg.l	d0
	beq	m_err79
	move.l	d0,d2
	moveq.l	#$d8,d0
	bra	poke0
poke_imed:
	bsr	get_num
	move.l	d0,d2
	moveq.l	#$d5,d0
poke0:
	cmp.l	#255,d2
	bhi	m_err79
	bsr	wrt_data
	bra	wrt_d1d2

mml_w:				*同期コマンド
	bsr	chk_0_9
	bmi	waiting_cmd
	bsr	get_num
	move.l	d0,d1
	subq.l	#1,d1
	cmpi.l	#tr_max,d1
	bhi	m_err63
	moveq.l	#$af,d0
	bsr	wrt_data
	bsr	wrt_data_d1
	bra	mml_lp
waiting_cmd:
	moveq.l	#$83,d0
	bsr	wrt_data
	bra	mml_lp

mml_conv_end:

calc_gt:			*ゲートタイムの計算
	* < d0.w=step time
	* > d0.w=real step time
	* - all
	move.l	d1,-(sp)
	moveq.l	#0,d1		*init d1
	move.w	cv_q_com(a1),d1
	bpl	case_normal_q
	neg.w	d1
	cmp.w	d0,d1
	bcc	exit_cg		*ゲートタイムが負になる時は計算しない
	btst.b	#2,cv_cnv_flg(a1)	*!2.05
	bne	gt_eq_@q		*!2.05
	sub.w	d1,d0
	beq	@f
	bra	exit_cg
gt_eq_@q:				*!2.05
	move.w	d1,d0			*!2.05
	bra	exit_cg			*!2.05
case_normal_q:
	mulu	d0,d1
	lsr.l	#3,d1		*/8
	move.w	d1,d0
	bne	exit_cg
@@:
	moveq.l	#1,d0		*0の場合は最小値１にしておく
exit_cg:
	move.l	(sp)+,d1
	rts

chk_0_9:			*数字かそうでないか
	* < (a0)=data
	* > minus=not suji
	* > eq=suji
	* - all
	movem.l	d0,-(sp)	*わざとmovem
	bsr	skip_spc2
	move.b	(a0),d0
	cmpi.b	#'0',d0
	bcs	chk09_0
	cmpi.b	#'9',d0
	bls	yes_sj
chk09_0:
	cmpi.b	#'%',d0
	beq	yes_sj
	cmpi.b	#'$',d0
	beq	yes_sj
	cmpi.b	#'+',d0
	beq	yes_sj
	cmpi.b	#'-',d0
	beq	yes_sj
*not_sj:
	moveq.l	#-1,d0
	movem.l	(sp)+,d0
	rts
yes_sj:
	moveq.l	#0,d0
	movem.l	(sp)+,d0
	rts

chk_oncho:			*音長があるかないか
	* < (a0)=data
	* > minus=not oncho
	* > eq=suji
	* X d0
	bsr	skip_spc2
	move.b	(a0),d0
	cmpi.b	#'0',d0
	bcs	chk_o0
	cmpi.b	#'9',d0
	bls	yes_oncho
chk_o0:
	cmpi.b	#'$',d0		*１６進数指定
	beq	yes_oncho
*not_oncho:
	moveq.l	#-1,d0
	rts
yes_oncho:
	moveq.l	#0,d0
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
*	cmpi.b	#'_',d1
*	beq	_num_lp01
	sub.b	#$30,d1
	bmi	_num_exit
	cmp.b	#9,d1
	bhi	_num_exit

	clr.b	d4		*clr.b	hajimete

	add.l	d0,d0
	move.l	d0,d3
	lsl.l	#2,d0
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
*	cmpi.b	#'_',d1
*	beq	__num_lp01
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
*	cmpi.b	#'_',d1
*	beq	b__num_lp01
	cmpi.b	#'0',d1
	beq	exit_mkcptl_bn
	cmpi.b	#'1',d1
	bne	_num_exit
exit_mkcptl_bn:
	sub.b	#$30,d1
	clr.b	d4		*数字をゲットしたことをマーク
	add.l	d0,d0		*lsl.l	#1,d0
	or.b	d1,d0
	bra	b__num_lp01
