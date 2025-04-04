*-------------------------------------------------------
*	       ZMUSIC.X支援プログラムサンプル
*
*			   ＺＰ.Ｒ
*
*		PROGRAMMED  BY  Z.NISHIKAWA
*
*-------------------------------------------------------
	.include	iocscall.mac
	.include	doscall.mac
	.include	LzzConst.mac
	.include	label.mac

max_p:		equ	64	*juke boxで演奏出来る最大曲数
ch_max:		equ	25	*fm8+adpcm1+midi16=25
pl_max:		equ	32	*一度に演奏可能なトラック数
tr_max:		equ	80	*確保出来るトラック数の最大値
fn_size:	equ	91	*１ファイルネームバッファの大きさ
setup1:		equ	fn_size*0
setup2:		equ	fn_size*1
setup3:		equ	fn_size*2
setup4:		equ	fn_size*3
juke_int:	equ	30	*割り込み周期＝(1/60)*30=約0.50秒
debug_int:	equ	6	*割り込み周期＝(1/60)*06=約0.10秒
RTS:		equ	$4e75		*RTSの命令コード
BSR:		equ	$6100		*BSR.wの命令コード
fm_addr_port:	equ	$e90001
fm_data_port:	equ	$e90003

opmwait		macro			*24MHzに改造したXVIへ対応/X68030へ対応させる時
	local	chk_opm_wait
chk_opm_wait:
	tst.b	fm_data_port	*busy check
	bmi	chk_opm_wait
	endm

opmset	macro	reg,data	*ＦＭ音源のレジスタ書き込み
	opmwait
	move.b	reg,fm_addr_port
	opmwait
	move.b	data,fm_data_port
	endm

Z_MUSIC	macro	func		*ドライバへのファンクションコール
	moveq.l	func,d1
	trap	#3
	endm

bitsns	macro	n,dreg
	move.b	$800+n.w,dreg
	endm

sftsns	macro	dreg
	move.w	$810.w,dreg
	endm

top:
	bra	non_keep
				*常駐部分
start:				*割り込みエントリーその１(JUKE BOX)
	movem.l	d0-d4/a0-a2/a5-a6,-(sp)
	btst.b	#3,$e84080
	bne	quit_int		*DMAが活動中は何も処理をしない

	lea	work(pc),a6

	tst.b	_int_flag-work(a6)
	bne	quit_int
	st.b	_int_flag-work(a6)
	move.w	$0028(a7),d0
	ori.w	#$2000,d0
	move.w	d0,sr			*スタックを割り込み発生前に戻す

	tst.b	hajimete-work(a6)
	bmi	chk_nxt_

	lea	key_tbl_jk(pc),a0
	lea	$800.w,a1

	bsr	key_inp			*[shift]+[opt1]
	bne	jk_im_next
	clr.b	sft_opt1-work(a6)

	bsr	key_inp			*[shift]+[opt2]
	bne	jk_fo_next
	clr.b	sft_opt2-work(a6)

	bsr	key_inp			*[shift]+[ctrl]
	bne	jk_stop_cont
	clr.b	sft_ctrl-work(a6)

	bsr	key_inp			*[shift]+[xf4]
	bne	jk_im_play_again
	clr.b	jk_im_play_k-work(a6)

	bsr	key_inp			*[shift]+[xf5]
	bne	jk_fo_play_again
	clr.b	jk_fo_play_k-work(a6)

	bsr	key_inp			*[ctrl]+[opt1]
	bne	jk_im_back
	clr.b	ctrl_opt1-work(a6)

	bsr	key_inp			*[ctrl]+[opt2]
	bne	jk_fo_back
	clr.b	ctrl_opt2-work(a6)
chk_nxt_:
	tst.b	next_flg-work(a6)	*フラグが寝てるから次の曲へ移らない
	beq	bye_int

	tst.b	hajimete-work(a6)
	bmi	hajimete_ope

	clr.b	next_flg-work(a6)
	not.b	next_flg2-work(a6)
	bmi	goto_fadeout	*next_flg2=$ff : フェードアウトを行う
				*next_flg2=$00 : 次の曲を演奏する
set_param:
	andi.w	#$f8ff,sr	*割り込み許可

	moveq.l	#0,d4
	move.b	music_no(pc),d4
	move.w	d4,-(sp)
	add.w	d4,d4
	add.w	d4,d4		*4倍
	lea	data_addr_tbl(pc),a5
	add.w	d4,a5
	addq.b	#1,music_no-work(a6)
	tst.l	4(a5)
	bne	_jkbx0
	clr.b	music_no-work(a6)	*始めに戻す
_jkbx0:
	move.l	(a5),a5
	move.l	a5,d4		*save a5 into d4
	move.l	(a5),d0
	beq	ope_zmd		*ZMDデータのみだから即演奏

_jkbx_lp:
	move.l	d0,a5

	move.l	4(a5),d3
	bne	ope_mdd		*case:MDD
	tst.l	8(a5)		*address mode?
	beq	zpd_adr_mode
	lea.l	8+8(a5),a1	*a1=8(zpd_top_adr)最初のテーブル情報にポインタを設定
	Z_MUSIC	#$46		*ZPDテーブルの登録
	bra	more_?
zpd_adr_mode:
	move.l	12(a5),a1
	addq.w	#8,a1		*a1=8(zpd_top_adr)最初のテーブル情報にポインタを設定
	Z_MUSIC	#$46		*ZPDテーブルの登録
	bra	more_?
ope_mdd:
	* < d3.l=MDD size
	lea	MIDI(pc),a2
	move.l	a5,d2		*save a5 into d2
	tst.l	8(a5)		*address mode?
	beq	mdd_adr_mode
	lea.l	8(a5),a5	*MDD address
	bsr	self_output	*MDD 登録
	move.l	d2,a5		*get back a5 from d2
	bra	more_?
mdd_adr_mode:
	move.l	12(a5),a5
	bsr	self_output	*MDD 登録
	move.l	d2,a5		*get back a5 from d2
more_?:
	move.l	(a5),d0
	bne	_jkbx_lp
ope_zmd:
	move.l	d4,a5		*get back a5
	lea.l	8+7(a5),a1	*music data address
				*7(zmd_top_adr)=version number
	moveq.l	#0,d2		*高速応答モードで演奏
	Z_MUSIC	#$11		*play

	move.w	(sp)+,d4
	lea	loop_tbl(pc),a0
	move.b	(a0,d4.w),d2		*d3.b=次の曲のループ回数
	lea	set_next_flg(pc),a1	*ジャンプ先
	Z_MUSIC	#$3b			*飛先やループ回数を登録
bye_int:
	move.w	#$2700,sr		*自分自身への二重割り込みの禁止
	clr.b	_int_flag-work(a6)
quit_int:
	movem.l	(sp)+,d0-d4/a0-a2/a5-a6
	rte

jk_im_next:
	tst.b	sft_opt1-work(a6)
	bne	chk_nxt_
	st.b	sft_opt1-work(a6)
					*next_flg=$ff	*強制的に次の曲へ移る
					*next_flg2=$ff	*すぐに…
	move.w	#$ffff,next_flg-work(a6)
	bra	chk_nxt_
jk_fo_next:
	tst.b	sft_opt2-work(a6)
	bne	chk_nxt_
	st.b	sft_opt2-work(a6)
					*next_flg=$ff	*強制的に次の曲へ移る
					*next_flg2=$00	*フェードアウトしてから…
	move.w	#$ff00,next_flg-work(a6)
	bra	chk_nxt_
jk_stop_cont:				*一時停止？
	tst.b	sft_ctrl-work(a6)
	bne	chk_nxt_
	st.b	sft_ctrl-work(a6)
	not.b	stop_cont-work(a6)
	bne	jk_stop
	moveq.l	#0,d2
	moveq.l	#0,d3
	moveq.l	#0,d4
	Z_MUSIC	#$0b		*m_cont_all
	bra	chk_nxt_
jk_stop:
	moveq.l	#0,d2
	moveq.l	#0,d3
	moveq.l	#0,d4
	Z_MUSIC	#$0a		*m_stop_all
	bra	chk_nxt_
jk_im_play_again:
	tst.b	jk_im_play_k-work(a6)
	bne	chk_nxt_
	st.b	jk_im_play_k-work(a6)
	Z_MUSIC	#$0f		*m_play2
	bra	chk_nxt_
jk_fo_play_again:
	tst.b	jk_fo_play_k-work(a6)
	bne	chk_nxt_
	st.b	jk_fo_play_k-work(a6)
	move.w	#$ff00,next_flg-work(a6)
	subq.b	#1,music_no-work(a6)
	bcc	chk_nxt_
	bra	jk_no_rev
jk_im_back:
	tst.b	ctrl_opt1-work(a6)
	bne	chk_nxt_
	st.b	ctrl_opt1-work(a6)
					*next_flg=$ff	*強制的に次の曲へ移る
					*next_flg2=$ff	*すぐに…
	move.w	#$ffff,next_flg-work(a6)
	bra	@f
jk_fo_back:
	tst.b	ctrl_opt2-work(a6)
	bne	chk_nxt_
	st.b	ctrl_opt2-work(a6)
					*next_flg=$ff	*強制的に次の曲へ移る
					*next_flg2=$00	*フェードアウトしてから…
	move.w	#$ff00,next_flg-work(a6)
@@:
	subq.b	#2,music_no-work(a6)
	bcc	chk_nxt_
jk_no_rev:
	move.b	juke_max(pc),d0
	addq.b	#1,d0
	add.b	d0,music_no-work(a6)
	bra	chk_nxt_

hajimete_ope:			*初めての時はここへ
	clr.b	hajimete-work(a6)
	clr.b	next_flg-work(a6)
	bra	set_param

goto_fadeout:			*フェードアウトをする
	moveq.l	#16,d2		*fade out speed
	Z_MUSIC	#$1a
				*ループ回数チェックが成立した時にfunc$3bの機能が
				*初期化されるためもう一度有効にするためのダミー設定をする。
				*今度はフェードアウトで演奏が停止してしまうので
				*特にループ回数は設定しない
	lea	set_next_flg(pc),a1	*ジャンプ先
	moveq.l	#-1,d2		*dummy
	Z_MUSIC	#$3b		*飛先やループ回数を登録
	bra	bye_int

set_next_flg:			*ループ条件が満たされるとドライバからここへ来る
	lea	next_flg(pc),a1
*	tst.b	d0
	beq	mark_nf		*ちゃんと指定回数分ループしたとき
				*next_flg=$ff
				*next_flg2=$ff フェードアウトしないで次へ行くようにマーク
	move.w	#$ffff,(a1)
	rts
mark_nf:
				*next_flg=$ff
				*next_flg2=$00
	move.w	#$ff00,(a1)
	rts			*ドライバへ戻る

self_output:
	* < a2=file name
	* < d3=size
	* < a5=data address
	* - all
	move.l	d5,-(sp)
	move.w	#%0_000_01,-(sp)	*zmusicへ出力しちゃう
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
	move.l	(sp)+,d5
	rts

*	.include	dbg.s

int_entry2:			*割り込みエントリーその２(DEBUG MODE)
	movem.l	d0-d4/a0-a1/a6,-(sp)
	lea	work(pc),a6

	tst.b	_int_flag-work(a6)
	bne	quit_int2
	st.b	_int_flag-work(a6)
	move.w	$0020(a7),d0
	ori.w	#$2000,d0
	move.w	d0,sr			*スタックを割り込み発生前に戻す

	lea	key_tbl_db(pc),a0
	lea	$800.w,a1

	bsr	key_inp
	bne	_PLAY		*演奏開始(xf4)
	clr.b	play_k-work(a6)

	bsr	key_inp
	bne	_PAUSE		*一時停止(opt1)
	clr.b	stop_k-work(a6)

	bsr	key_inp
	bne	_SRCH		*一時停止解除(opt2)
	clr.b	cont_k-work(a6)

	bsr	key_inp
	bne	_FF		*早送り(xf5)

	bsr	key_inp
	bne	_SLOW		*低速演奏

	bsr	key_inp		*FADEOUT(xf1)
	bne	_FADEOUT
	clr.b	fadeout_k-work(a6)

	bsr	key_inp		*FADE IN(xf2)
	bne	_FADE_IN
	clr.b	fadein_k-work(a6)
chk_FF:
	tst.b	_FF_flg-work(a6)	*早送り終了か?
	beq	chk_SLOW
	moveq.l	#-1,d2
	moveq.l	#-1,d3
	Z_MUSIC	#$47			*通常音量に戻す。
	clr.b	_FF_flg-work(a6)
	bra	@f
chk_SLOW:
	tst.b	_SLOW_flg-work(a6)	*低速演奏か?
	beq	bye_ie2
@@:
	moveq.l	#-1,d2
	Z_MUSIC	#$05
	move.w	a0,d2
	Z_MUSIC	#$3f			*通常テンポに戻す
	clr.b	_SLOW_flg-work(a6)
bye_ie2:
	move.w	#$2700,sr		*自分自身への二重割り込みの禁止
	clr.b	_int_flag-work(a6)
quit_int2:
	movem.l	(sp)+,d0-d4/a0-a1/a6
	rte

key_inp:
	* > nz key on
	* > ze key off
	move.b	(a0)+,d0	*key group
	move.b	d0,d1
	andi.w	#$0f,d0
	lsr.b	#4,d1		*key status bit
	btst.b	d1,(a1,d0.w)
	beq	@f
	move.b	(a0)+,d0	*key group
	move.b	d0,d1
	andi.w	#$0f,d0
	lsr.b	#4,d1		*key status bit
	btst.b	d1,(a1,d0.w)
	rts
@@:
	addq.w	#1,a0
	moveq.l	#0,d0
	rts

_PAUSE:				*一時停止
	tst.b	stop_k-work(a6)
	bmi	bye_ie2
	st.b	stop_k-work(a6)
	tst.b	stop_mode-work(a6)
	bne	do_cont
	st.b	stop_mode-work(a6)
	moveq.l	#0,d2
	moveq.l	#0,d3
	moveq.l	#0,d4
	Z_MUSIC	#$0a		*m_stop
	bra	bye_ie2

_SRCH:				*一時停止解除
	tst.b	cont_k-work(a6)
	bmi	bye_ie2
	st.b	cont_k-work(a6)
	tst.b	stop_mode-work(a6)
	beq	bye_ie2		*演奏は停止していない
do_cont:
	moveq.l	#0,d2
	moveq.l	#0,d3
	moveq.l	#0,d4
	move.b	d2,stop_mode-work(a6)
	Z_MUSIC	#$0b		*m_cont
	bra	bye_ie2

_PLAY:				*演奏開始
	tst.b	play_k-work(a6)
	bmi	bye_ie2
	st.b	play_k-work(a6)
	clr.b	stop_mode-work(a6)
	Z_MUSIC	#$0f		*m_play2
	bra	bye_ie2

_FADE_IN:
	tst.b	fadein_k-work(a6)
	bmi	bye_ie2
	st.b	fadein_k-work(a6)
	moveq.l	#-16,d2
	Z_MUSIC	#$1a
	bra	bye_ie2

_FADEOUT:
	tst.b	fadeout_k-work(a6)
	bmi	bye_ie2
	st.b	fadeout_k-work(a6)
	moveq.l	#16,d2
	Z_MUSIC	#$1a
	bra	bye_ie2

_FF:				*早送り
	tst.b	_SLOW_flg-work(a6)	*低速演奏か?
	bne	bye_ie2
	move.l	real_ch_tbl(pc),a0
	tst.b	(a0)		*check base ch
	bne	bs_mdi
	move.l	#$0000_00ff,d2	*FM
	moveq.l	#111,d3
	Z_MUSIC	#$47
	move.l	#$fe00_0100,d2	*ADPCM
	moveq.l	#30,d3
	Z_MUSIC	#$47
	move.l	#$01ff_fe00,d2	*MIDI
	moveq.l	#70,d3
	Z_MUSIC	#$47
	bra	_ff0
bs_mdi:
	move.l	#$00ff_0000,d2	*FM
	moveq.l	#111,d3
	Z_MUSIC	#$47
	move.l	#$ff00_0000,d2	*ADPCM
	moveq.l	#30,d3
	Z_MUSIC	#$47
	move.l	#$0000_ffff,d2	*MIDI
	moveq.l	#70,d3
	Z_MUSIC	#$47
_ff0:
	move.w	#$399,d2	*timer Bのときは自己書き換え
_ff1:
	bsr.s	set_timer_a
	st.b	_FF_flg-work(a6)
	bra	bye_ie2

_SLOW:				*低速演奏
	tst.b	_FF_flg-work(a6)	*早送り終了か?
	bne	bye_ie2
_sl0:
	move.w	#$00,d2		*timer Bのときは自己書き換え
_sl1:
	bsr.s	set_timer_a
	st.b	_SLOW_flg-work(a6)
	bra	bye_ie2

set_timer:			*タイマーBの初期化(init_timerとほぼ同じ)
	move.w	sr,-(sp)
	ori.w	#$0700,sr
	moveq.l	#$12,d1
	bsr	opmset
	move.w	(sp)+,sr
	rts

set_timer_a:			*タイマーAの初期化(init_timer_bほぼ同じ)
	move.w	sr,-(sp)
	ori.w	#$0700,sr
	moveq.l	#$11,d1
	bsr	opmset		*set value L
	lsr.w	#2,d2
	moveq.l	#$10,d1
	bsr	opmset		*set value H
	move.w	(sp)+,sr
	rts

opmset:
	opmset	d1,d2		*set value H
	rts

work:
key_tbl_db:	dc.b	$0e,$0b,$0e,$2e,$0e,$3e,$0e,$1b,$0e,$7a,$0e,$5a,$0e,$6a
key_tbl_jk:	dc.b	$0e,$2e,$0e,$3e,$0e,$1e,$0e,$0b,$0e,$1b,$1e,$2e,$1e,$3e
juke_max:	dc.b	0
play_k:		dc.b	0
stop_k:		dc.b	0
stop_mode:	dc.b	0
cont_k:		dc.b	0
fadein_k:	dc.b	0
fadeout_k:	dc.b	0
hajimete:	dc.b	$ff	*初めてかどうか($ff=yes)
stop_cont:	dc.b	0
sft_ctrl:	dc.b	0
sft_opt1:	dc.b	0
sft_opt2:	dc.b	0
jk_im_play_k:	dc.b	0
jk_fo_play_k:	dc.b	0
ctrl_opt1:	dc.b	0
ctrl_opt2:	dc.b	0
		.even
next_flg:	dc.b	$ff	*!!!	順序変更禁止
next_flg2:	dc.b	$00	*!!!	順序変更禁止
music_no:	dc.b	0	*次回に演奏すべきデータナンバー
loop_tbl:	dcb.b	max_p,0
OPM:		dc.b	'OPM',0
MIDI:		dc.b	'MIDI',0
_FF_flg:	dc.b	0
_SLOW_flg:	dc.b	0
_int_flag:	dc.b	0
	.even
real_ch_tbl:	ds.l	1
results:	ds.l	1
timer_value:	ds.w	1
data_addr_tbl:	*zmd data address
		dcb.l	max_p+1,0
end:

non_keep:
	lea	work(pc),a6
	move.l	a0,a0work-work(a6)	*メモリ管理ポインタ
	move.l	a3,env_bak-work(a6)

	lea	pname(pc),a1
	lea	$c4(a0),a0
lop1:
	move.b	(a0)+,(a1)+	*a0+$c4はexecされたfile名
	bne	lop1		*file名をpnameへ転送

	lea	prog_name(pc),a1	*release時のためにフルパスのファイルネームを作成
	movea.l	a0work(pc),a0
	lea	$80(a0),a0	*$a0+$80はこのプログラムが存在するパス名
lop2:
	move.b	(a0)+,(a1)+
	bne	lop2
	subq.w	#1,a1
	lea	pname(pc),a0
lop3:
	move.b	(a0)+,(a1)+
	bne	lop3
	move.b	#' ',-1(a1)	*' /R'
	move.b	#'/',(a1)+
	move.b	#'r',(a1)+
	clr.b	(a1)

	move.l	a0work(pc),a0		*メモリ管理ポインタ
	lea	end_of_prog(pc),a1	*program end address+1
	lea	$10(a0),a0		*メモリブロックの変更
	suba.l	a0,a1
	pea	(a1)
	pea	(a0)
	DOS	_SETBLOCK
	addq.w	#8,sp
	tst.l	d0
	bmi	out_mem

	suba.l	a1,a1		*スーパーバイザへ
	IOCS	_B_SUPER
	move.l	d0,ssp-work(a6)
	lea	my_sp(pc),sp	*スタックの設定

	tst.b	(a2)+
	beq	title_hlp

	bsr	skip_spc2	*スペースのスキップ
	move.b	(a2)+,d0
	cmpi.b	#'-',d0		*スイッチ指定がある
	beq	g?
	cmpi.b	#'/',d0		*スイッチ指定がある。
	bne	fn_p
g?:
	bsr	skip_spc2
	move.b	(a2)+,d0
	bsr	mk_capital
	cmpi.b	#'G',d0
	bne	go_chk_sw

	st.b	non_disp-work(a6)

	bsr	skip_spc2	*スペースのスキップ
	move.b	(a2)+,d0
	cmpi.b	#'-',d0		*スイッチ指定がある
	beq	chk_sw
	cmpi.b	#'/',d0		*スイッチ指定がある。
	beq	chk_sw
	subq.w	#1,a2		*スイッチ無しは演奏開始と解釈
	bra	m_play_
fn_p:
	moveq.l	#0,d1
	IOCS	_B_CLR_ST
	bsr	print_title	*タイトルの表示
	subq.w	#1,a2		*スイッチ無しは演奏開始と解釈
	bra	m_play_

go_chk_sw:			*'/g'でなかった
	move.w	d0,-(sp)
	moveq.l	#0,d1
	IOCS	_B_CLR_ST
	bsr	print_title	*タイトルの表示
	move.w	(sp)+,d0
	bra	cs0
chk_sw:
	bsr	skip_spc2	*スペースのスキップ
	move.b	(a2)+,d0
	bsr	mk_capital	*小文字/大文字変換
cs0:
	cmpi.b	#'A',d0		*MIDIデータ取り込み／書き出し
	beq	midi_ope
	cmpi.b	#'C',d0		*演奏再開
	beq	m_cont
	cmpi.b	#'D',d0		*デバッグツールの常駐
	beq	debug_mode
	cmpi.b	#'E',d0		*enable ch
	beq	solo_play
	cmpi.b	#'F',d0		*Fadeout
	beq	fadeout
	cmpi.b	#'I',d0		*ZMUSICと楽器の初期化
	beq	init_zmsc
	cmpi.b	#'J',d0		*ジュークボックス
	beq	juke_box
	cmpi.b	#'M',d0		*Mute ch
	beq	mask_ch
	cmpi.b	#'O',d0		*set output level
	beq	output_level
	cmpi.b	#'P',d0		*演奏開始
	beq	m_play
	cmpi.b	#'Q',d0		*合計値計算
	beq	m_total
	cmpi.b	#'R',d0		*ジュークボックス終了(解除)
	beq	release
	cmpi.b	#'S',d0		*演奏停止
	beq	m_stop
	cmpi.b	#'T',d0		*バイナリMIDIデータを楽器へ転送
	beq	midi_send_b
	cmpi.b	#'W',d0		*同期演奏
	beq	synchro_play
	cmpi.b	#'X',d0		*MIDIデータを楽器へ転送
	beq	midi_send
	bra	print_hlp

title_hlp:
	bsr	print_title	*タイトルの表示
	bra	print_hlp	*ヘルプの表示

exit:				*/Wの時パッチが当たる
	NOP
	bsr	print_err_code

	move.l	ssp(pc),a1	*ユーザーモードへ
	IOCS	_B_SUPER

	DOS	_EXIT		*終了

keeppr:				*(JUKE BOX)
	lea	start(pc),a1	*設定を行う
	moveq.l	#juke_int,d1
	IOCS	_VDISPST
	tst.l	d0
	bne	other_prog	*既に他のプログラムが使用中

	bsr	key_bind_jk

	Z_MUSIC	#$50
	bset.b	#0,6(a0)	*juke_box included

	lea	prog_name(pc),a1
	Z_MUSIC	#$40
	move.l	d0,results-prog_name(a1)

	move.l	ssp(pc),a1	*ユーザーモードへ
	IOCS	_B_SUPER

	tst.b	non_disp-work(a6)
	bne	aa0
	move.w	#2,-(sp)
	pea	juke_mes(pc)
	DOS	_FPUTS
	addq.w	#6,sp
aa0:
	move.l	lzz_adr(pc),d0
	beq	kp00
	move.l	d0,-(sp)
	DOS	_MFREE
	addq.w	#4,sp
kp00:
	clr.w	-(sp)
	move.l	#end-top,-(sp)
	DOS	_KEEPPR		*常駐終了

debug_mode:
	bsr	kep_chk		*常駐check
	bpl	jb_already	*既に常駐してます
	bsr	chk_drv		*ドライバ常駐チェック
	bmi	no_drv		*ドライバが組み込まれてない

	lea	int_entry2(pc),a1	*設定を行う
	moveq.l	#debug_int,d1
	IOCS	_VDISPST
	tst.l	d0
	bne	other_prog	*既に他のプログラムが使用中

	bsr	key_bind_db

	Z_MUSIC	#$50
	bset.b	#1,6(a0)	*Debug mode included

	lea	prog_name(pc),a1
	Z_MUSIC	#$40
	move.l	d0,results-prog_name(a1)

	Z_MUSIC	#$3d		*get timer type
	tst.l	d0
	beq	@f
	move.w	#$f8,_ff0+2-work(a6)	*case:timer B
	move.w	#BSR+((set_timer-_ff1-2).and.$ff),_ff1-work(a6)
	move.w	#$5d,_sl0+2-work(a6)
	move.w	#BSR+((set_timer-_sl1-2).and.$ff),_sl1-work(a6)
@@:
	move.l	ssp(pc),a1	*ユーザーモードへ
	IOCS	_B_SUPER

	tst.b	non_disp-work(a6)
	bne	aa1
	move.w	#2,-(sp)
	pea	debug_mes(pc)
	DOS	_FPUTS
	addq.w	#6,sp
aa1:
	clr.w	-(sp)
	move.l	#end-top,-(sp)
	DOS	_KEEPPR		*常駐終了

release:			*JUKE BOX終了(常駐解除)
	bsr	kep_chk		*常駐check
	bmi	not_kep		*常駐していない
	bsr	chk_drv		*ドライバ常駐チェック
	bmi	no_drv		*ドライバが組み込まれてない

	suba.l	a1,a1		*割り込み終了
	IOCS	_VDISPST

	Z_MUSIC	#$00

	suba.l	a1,a1		*a1=0
	Z_MUSIC	#$3b		*飛先やループ回数の登録の無効化

	move.l	a2work(pc),d3
	sub.l	a0work(pc),d3	*calc offset

	Z_MUSIC	#$50
	bclr.b	#0,6(a0)	*juke_box released
	bclr.b	#1,6(a0)	*Debug mode released

	lea	results(pc),a0
	move.l	(a0,d3.l),d2
	Z_MUSIC	#$40		*サポートプログラムキャンセル(<a1.l=0)

	lea	data_addr_tbl(pc),a1
	adda.l	d3,a1		*juke boxではない
	tst.l	(a1)
	beq	not_mus
	move.l	(a1),a1
	pea	(a1)
	DOS	_MFREE
	addq.w	#4,sp
not_mus:
	move.l	a2work(pc),a2
	clr.b	$c4(a2)
	pea	$10(a2)
	DOS	_MFREE
	addq.w	#4,sp

	bra	jb_released	*常駐解除のメッセージ表示

kep_chk:
	* > eq=exists
	* > ne=none
	movem.l	d1-d7/a0-a6,-(sp)
	move.l	a0work(pc),a0
klop0:
	tst.l	(a0)
	beq.s	klop1
	move.l	(a0),a0
	bra.s	klop0
klop1:
	move.l	12(a0),d0	*次のメモリ管理ポインタ
	beq.s	err_chk
	movea.l	d0,a2
	cmpa.l	a0work(pc),a2
	beq.s	klop_nxt
	bsr.s	str_chk
	beq.s	end_chk
klop_nxt:
	move.l	a2,a0
	bra.s	klop1		*どんどんさかのぼる

end_chk:
	move.l	a2,a2work-work(a6)
	movem.l	(sp)+,d1-d7/a0-a6
	moveq.l	#0,d0
	rts
err_chk:
	movem.l	(sp)+,d1-d7/a0-a6
	moveq.l	#-1,d0
	rts

str_chk:
	* > ne=none
	* > eq=exists
	lea	pname(pc),a0	*このプロセスの名前
	moveq.l	#0,d1
	lea	$c4(a2),a1
slop1:				*同じプロセス名が存在するか
	move.b	0(a0,d1.l),d2
	andi.b	#$df,d2		*小文字から大文字へ変換
	move.b	0(a1,d1.l),d3
	andi.b	#$df,d3		*小文字から大文字へ変換
	cmp.b	d3,d2
	bne.s	noteq		*違う
	addq.l	#1,d1
	tst.b	0(a0,d1.l)
	beq.s	equal		*同じものが存在
	bra.s	slop1
noteq:
	moveq.l	#1,d0
	rts			*d0<>0で同じものが存在しなかったと知らせる
equal:
	moveq.l	#0,d0		*d0= 0で同じものが存在と知らせる
	rts

juke_box:			*ジュークボックス処理
	bsr	kep_chk		*常駐check
	bpl	jb_already	*既に常駐してます
	bsr	chk_drv		*ドライバ常駐チェック
	bmi	no_drv		*ドライバが組み込まれてない
	bsr	read_lzz	*LZZ.Rがあれば読み込んでおく
	bsr	skip_spc2
	lea	setup_fn(pc),a0
	move.l	a0,sfp-setup_fn(a0)
	moveq.l	#0,d0
	move.b	d0,setup1(a0)	*ワーク初期化
	move.b	d0,setup2(a0)
	move.b	d0,setup3(a0)
	move.b	d0,setup4(a0)
	lea	zpdnm_buf(pc),a0
	bsr	clr_nm_buf
	lea	mddnm_buf(pc),a0
	bsr	clr_nm_buf
*	lea	filename(pc),a0
*	bsr	setsulp
	lea	JUK_kaku(pc),a1
	bsr	set_fn
jkbx_lp:			*セットアップファイルがあるなら最大４つまで指定可能
	cmpi.b	#',',d0
	bne	exec_setup_jb
	bsr	set_stup	*get setup filename
	add.l	#fn_size,sfp-work(a6)
	bra	jkbx_lp
exec_setup_jb:			*セットアップファイルの実行
	lea	setup_fn(pc),a2
exec_stjblp:
	tst.b	(a2)
	beq	exec_jkbx
	pea	(a2)
	pea	(a2)
	bsr	fopen
	tst.l	d5
	bmi	file_not_found
	bsr	read		*>d3=data size,a5=data address
	move.l	(sp)+,a2
	bsr	chk_mdd?	*>a2.l=dest. filename
	bsr	self_output	*自己出力
	pea	(a5)
	DOS	_MFREE
	addq.w	#4,sp
	move.l	(sp)+,a2
	lea	fn_size(a2),a2
	bra	exec_stjblp
exec_jkbx:			*juke fileの解釈
	lea	filename(pc),a2
	bsr	fopen
	tst.l	d5
	bmi	file_not_found
	bsr	read		*(ret:d3=data size,a5=data address)
	move.l	d3,d4
	move.l	a5,a4
	move.l	a5,list_adr-work(a6)	*あとでmfreeする時に使用

	pea	$40000		*256kB
	DOS	_MALLOC
	addq.w	#4,sp
	move.l	d0,data_addr_tbl-work(a6)
	bmi	out_mem		*メモリ不足error
	move.l	d0,a5		*データアドレス
	lea	loop_tbl(pc),a1		*曲の繰り返し回数
	lea	data_addr_tbl(pc),a3	*data table
	moveq.l	#0,d6		*how many data
	moveq.l	#0,d7		*total size
	move.l	d7,last_zpd_addr-work(a6)
jkbx_lp01:			*演奏データの読み込み
	tst.l	d4
	beq	exit_jkbx_lp
	bmi	exit_jkbx_lp	*念のため
	move.b	(a4),d0
	cmpi.b	#$1a,d0
	beq	exit_jkbx_lp	*ファイル終端発見
	cmpi.b	#' ',d0
	bhi	get_lpc
	subq.l	#1,d4
	addq.w	#1,a4
	bra	jkbx_lp01
get_lpc:			*ループカウンタゲット
	bsr	chk_num
	bmi	set_l2
	bsr	asc_to_n
	bra	set_l_a1
set_l2:
	moveq.l	#1,d1
set_l_a1:
	move.b	d1,(a1)+	*ループ回数セット
	bsr	set_fn2
	lea	filename(pc),a2
	move.l	a5,(a3)+	*set zmd address
	move.l	a5,hozon_a5-work(a6)
	clr.l	(a5)+		*end mark
	clr.l	(a5)+		*dummy
	bsr	read_data	*曲データ読み込み > d3.l=size > a5.l=addr
	cmpi.l	#'ZDF0',(a5)
	beq	case_ZDF_jk
	cmpi.l	#$105a6d75,(a5)	*ＩＤチェック
	bne	unid_error
	cmpi.w	#'Si',4(a5)
	bne	unid_error
	cmpi.b	#'C',6(a5)
	bne	unid_error
czj0:
	move.l	a5,zmd_addr-work(a6)
	addq.l	#1,d3
	bclr.l	#0,d3
	add.l	d3,a5
rd_zpd_lp:
	pea	(a5)
	move.l	zmd_addr(pc),a5
	move.w	#$00ff,zpd_scan-work(a6)
					*clr.b	zpd_scan-work(a6)
					*st.b	mdd_scan-work(a6)
	bsr	get_sakiyomi_name	*get zpd filename(& get mdd filename)
	move.l	(sp)+,a5

	tst.b	zpd_scan-work(a6)
	beq	read_mdd??
read_zpd:				*ZPDデータの読み込み
	lea	filename(pc),a0
	lea	ZPD_kaku(pc),a2
	bsr	kakuchoshi
	lea	filename(pc),a0
	lea	zpdnm_buf(pc),a2
	moveq.l	#3,d5
	bsr	chk_same_nm		*同じものをすでに読んでいないか(>d0.l=result code)
	bmi	do_r_zpd		*読んでいない
	move.l	d0,a0
	move.b	(a0)+,d1
	lsl.l	#8,d1
	move.b	(a0)+,d1
	lsl.l	#8,d1
	move.b	(a0)+,d1
	lsl.l	#8,d1
	move.b	(a0)+,d1
	cmp.l	last_zpd_addr(pc),d1
	beq	rd_zpd_lp		*同じものが２回続く場合は省略
	move.l	d1,last_zpd_addr-work(a6)
	move.l	hozon_a5(pc),a0
	move.l	a5,hozon_a5-work(a6)
	move.l	a5,(a0)			*set link param.
	add.l	#16,d7
	move.l	d7,-(sp)
	move.l	data_addr_tbl(pc),-(sp)
	DOS	_SETBLOCK
	addq.w	#8,sp
	tst.l	d0
	bmi	out_mem		*OUT OF MEMORY
	clr.l	(a5)+			*END MARK
	clr.l	(a5)+			*ZPD MARK
	clr.l	(a5)+			*mark address mode
	move.l	d1,(a5)+		*set data address
	bra	rd_zpd_lp
do_r_zpd:
	move.l	d0,-(sp)
	move.l	hozon_a5(pc),a0
	move.l	a5,hozon_a5-work(a6)
	move.l	a5,(a0)		*set link param.
	clr.l	(a5)+		*end code
	clr.l	(a5)+		*zpd mark
	lea	filename(pc),a2
	bsr	read_data	*ZPDデータ読み込み
	cmpi.l	#$105a6d41,(a5)	*ＩＤチェック
	bne	unid_error
	cmpi.l	#'dpCm',4(a5)
	bne	unid_error

	move.l	a5,last_zpd_addr-work(a6)

	move.l	(sp)+,a0	*ファイルネームバッファ／アドレスバッファへ登録
	move.l	a5,d0
	bsr	wrt_d02a0
	add.l	d3,a5
	bra	rd_zpd_lp
read_mdd??:				*MDDデータの読み込み

rd_mdd_lp:
	pea	(a5)
	move.l	zmd_addr(pc),a5
	move.w	#$ff00,zpd_scan-work(a6)
					*st.b	zpd_scan-work(a6)
					*clr.b	mdd_scan-work(a6)
	bsr	get_sakiyomi_name	*get zpd filename(& get mdd filename)
	move.l	(sp)+,a5

	tst.b	mdd_scan-work(a6)
	beq	prepare_next
read_mdd:
	lea	mddfilename(pc),a0
	lea	MDD_kaku(pc),a2
	bsr	kakuchoshi
	lea	mddfilename(pc),a0
	lea	mddnm_buf(pc),a2
	moveq.l	#7,d5
	bsr	chk_same_nm		*同じものをすでに読んでいないか(>d0.l=result code)
	bmi	do_r_mdd		*読んでいない
	move.l	d0,a0
	move.b	(a0)+,d1
	lsl.l	#8,d1
	move.b	(a0)+,d1
	lsl.l	#8,d1
	move.b	(a0)+,d1
	lsl.l	#8,d1
	move.b	(a0)+,d1
	move.l	hozon_a5(pc),a2
	move.l	a5,hozon_a5-work(a6)
	move.l	a5,(a2)			*set link param.
	add.l	#16,d7
	move.l	d7,-(sp)
	move.l	data_addr_tbl(pc),-(sp)
	DOS	_SETBLOCK
	addq.w	#8,sp
	tst.l	d0
	bmi	out_mem		*OUT OF MEMORY
	clr.l	(a5)+			*end mark
	move.b	(a0)+,(a5)+		*save size(MDD mark)
	move.b	(a0)+,(a5)+
	move.b	(a0)+,(a5)+
	move.b	(a0)+,(a5)+
	clr.l	(a5)+			*mark address mode
	move.l	d1,(a5)+		*set data address
	bra	rd_mdd_lp
do_r_mdd:
	move.l	d0,-(sp)
	move.l	hozon_a5(pc),a0
	move.l	a5,hozon_a5-work(a6)
	move.l	a5,(a0)		*set link param.
	clr.l	(a5)+		*end code
	addq.w	#4,a5		*dummy(あとでサイズが入る)
	lea	mddfilename(pc),a2
	bsr	read_data	*>d3.l=mem.block size,d0.l=mdd true size
	cmpi.l	#'ZDF0',(a5)
	bne	chk_mdd_jk
	bsr	case_ZDF_jk2	*>d0.l=mdd true size
chk_mdd_jk:
	cmpi.w	#$0d0a,(a5)	*header checking
	bne	unid_error

	move.l	d0,-4(a5)	*save size(MDD mark)

	move.l	(sp)+,a0
	move.l	a5,d0
	bsr	wrt_d02a0	*save address
	move.l	-4(a5),d0
	bsr	wrt_d02a0	*save size
	add.l	d3,a5
	bra	rd_mdd_lp
prepare_next:
	move.b	d6,juke_max-work(a6)
	addq.b	#1,d6
	cmpi.b	#max_p,d6
	bhi	too_many	*曲データ多すぎ

	bra	jkbx_lp01

exit_jkbx_lp:
	move.l	list_adr(pc),-(sp)
	DOS	_MFREE
	addq.w	#4,sp

	Z_MUSIC	#$00		*m_init

	bra	keeppr		*常駐作業へ

read_data:
	* < a2.l=filename
	* < a5.l=address 
	* > d3.l=size(d0.l=true size)
	bsr	fopen
	tst.l	d5
	bmi	file_not_found
	* < d5.l=file handle
	move.w	#2,-(sp)	*ファィルの長さを調べる
	clr.l	-(sp)
	move.w	d5,-(sp)
	DOS	_SEEK
 	addq.w	#8,sp		*d0.l=file length
	addq.l	#1,d0
	bclr.l	#0,d0		*.even
	move.l	d0,d3		*d3=length
	beq	fsize0		*file size=0
	bmi	fsize0		*file size=minus
	add.l	d3,d7

	addq.l	#8,d7		*listワークのために８バイト余分に
	move.l	d7,-(sp)
	move.l	data_addr_tbl(pc),-(sp)
	DOS	_SETBLOCK
	addq.w	#8,sp
	tst.l	d0
	bmi	out_mem		*OUT OF MEMORY

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
	move.l	d0,-(sp)
	bmi	read_err	*読み込み失敗

	move.w	d5,-(sp)	*close
	DOS	_CLOSE
	addq.l	#2,sp
	move.l	(sp)+,d0
	rts

wrt_d02a0:
	rol.l	#8,d0
	move.b	d0,(a0)+
	rol.l	#8,d0
	move.b	d0,(a0)+
	rol.l	#8,d0
	move.b	d0,(a0)+
	rol.l	#8,d0
	move.b	d0,(a0)+
	rts

clr_nm_buf:
	movem.l	d0-d1,-(sp)
	moveq.l	#0,d0
	move.w	#94*max_p,d1	*94*max_p+1-1だから
cnb_lp:
	move.b	d0,(a0)+
	dbra	d1,cnb_lp
	movem.l	(sp)+,d0-d1
	rts

chk_same_nm:
	* < a0.l=find source
	* < a2.l=find destination
	* < d5.l=number of skip bytes
	* > d0.l=result
	* destination format name strings(.b)...,(0,addr(.l))
	movem.l	d1-d2/a0/a2,-(sp)
wc_lp01:
	bsr	do_get_cmd_num
	bpl	exit_wc
wc_lp02:
	tst.b	(a2)+		*次のコマンド名へ
	bne	wc_lp02
	add.l	d5,a2		*skip addr
	tst.b	(a2)
	bne	wc_lp01
wc_lp03:			*バッファに登録
	move.b	(a0)+,d0
	bsr	mk_capital
	move.b	d0,(a2)+
	bne	wc_lp03
	subq.w	#1,a2
	move.l	a2,d0
	moveq.l	#-1,d1		*set minus
exit_wc:
	movem.l	(sp)+,d1-d2/a0/a2
	rts

do_get_cmd_num:			*実際に文字列を捜す
	* < a0=source str addr
	* > pl=get it!
	* > mi=can't found
	move.l	a0,d1
	move.l	a2,d2
dgscn_lp:
	move.b	(a0)+,d0
	bsr	mk_capital	*小文字→大文字
	cmp.b	(a2)+,d0
	bne	not_same_dgscn
	tst.b	(a2)		*終了?
	bne	dgscn_lp
dgcn0:
	move.l	a2,d0
	rts
not_same_dgscn:
	move.l	d1,a0
	move.l	d2,a2
	moveq.l	#-1,d0		*error!
	rts

case_ZDF_jk:			*juke box用ZDF展開
	* < a5.l=ZDF address
	* > d7.l=calculated total size
	* - all except result parameters
	sub.l	d3,d7
	movem.l	d0-d3/d5/a0-a4,-(sp)

	move.l	lzz_adr(pc),d0
	beq	cant_use_lzz
	move.l	d0,a0

	lea	bufadr(pc),a4

	pea	(a4)			*情報テーブル
	pea	(a5)			*ZDF addr
	jsr	_ref_data(a0)
	addq.w	#8,sp
	tst.l	d0
	bmi	lzz_err

	move.w	ZNumOfData(a4),d5
zjklp0:
	add.l	ZSize(a4),d7		*展開後のサイズ
	add.l	#8+1,d7		*list構造のwork size, +1は.even処理用
	bclr.l	#0,d7		*.even
	lea	ZNext(a4),a4
	subq.w	#1,d5
	bne	zjklp0
	subq.l	#8,d7		*ZMDの部分はコール以前に考慮済み

	move.l	a5,a1		*DMA転送のための下準備
	add.l	d3,a1
	subq.w	#1,a1		*source
	move.l	d3,d2		*size
	add.l	d3,d7
	move.l	data_addr_tbl(pc),a2
	add.l	d7,a2
	subq.w	#1,a2		*destination

	move.l	d7,-(sp)	*あらかじめメモリは確保しておく
	move.l	data_addr_tbl(pc),-(sp)
	DOS	_SETBLOCK
	addq.w	#8,sp
	tst.l	d0
	bmi	out_mem		*OUT OF MEMORY

	moveq.l	#%0000_1010,d1
	bsr	trans_dma

	lea	bufadr(pc),a4

	sub.l	d3,a2
	addq.w	#1,a2		*a2=zdf先頭アドレス

	pea	(a4)		*情報テーブル
	pea	(a2)		*ZDF addr
	jsr	_ref_data(a0)
	addq.w	#8,sp
	tst.l	d0
	bmi	lzz_err

	move.w	ZNumOfData(a4),d3
zjklp1:
	move.l	ZSize(a4),-(sp)		*展開後のサイズ分メモリ確保
	DOS	_MALLOC
	addq.w	#4,sp
	tst.l	d0
	bmi	out_mem			*OUT OF MEMORY
	move.l	d0,a1

	pea	(a1)			*push extract
	move.l	ZTopAdr(a4),-(sp)
	jsr	_ext_data(a0)
	addq.w	#8,sp
	tst.l	d0
	bmi	lzz_err

	move.l	ZSize(a4),d2
	move.l	a5,a2
	moveq.l	#5,d1		*dma mode
	bsr	trans_dma
	move.l	d2,d0
	addq.l	#1,d0
	bclr.l	#0,d0		*.even
	add.l	d0,a5

	move.w	ZKind(a4),d0	*get data type
	bne	zjk00
				*ZMD data
	cmpi.w	#1,d3		*ZMD１個だけならば
	beq	zjkfr
	movem.l	d5/a2/a5,-(sp)
	move.l	a2,a5
	move.l	d3,d5
	move.l	a4,a2
	bsr	erase_zpd_mdd	*adpcm_block_data,midi_dumpをつぶす
	movem.l	(sp)+,d5/a2/a5
	addq.w	#8,a5
	bra	zjkfr
zjk00:
	subq.w	#8,a2
	cmpi.w	#$20,d0		*ZPD?
	bne	zjk01
				*case:ZPD
	move.l	hozon_a5(pc),a3
	move.l	a5,hozon_a5-work(a6)
	move.l	a2,(a3)		*set link param.
	clr.l	(a2)+		*end code
	clr.l	(a2)+		*zpd mark
	bra	zjkfr
zjk01:				*case:MDD
	move.l	hozon_a5(pc),a3
	move.l	a5,hozon_a5-work(a6)
	move.l	a2,(a3)		*set link param.
	clr.l	(a2)+		*end code
	move.l	d2,(a2)+	*set size
zjkfr:
	pea	(a1)
	DOS	_MFREE
	addq.w	#4,sp

	lea	ZNext(a4),a4
	subq.w	#1,d3
	bne	zjklp1

	movem.l	(sp)+,d0-d3/d5/a0-a4

	sub.l	d3,d7
	move.l	d7,-(sp)	*あらかじめメモリは確保しておく
	move.l	data_addr_tbl(pc),-(sp)
	DOS	_SETBLOCK
	addq.w	#8,sp
	tst.l	d0
	bmi	out_mem		*OUT OF MEMORY

	cmpi.w	#1,bufadr+ZNumOfData-work(a6)
	bne	prepare_next
xx:					*ZMDが１個だけの時は
	move.l	hozon_a5(pc),a5
	addq.w	#8,a5
	move.l	bufadr+ZSize(pc),d3
	bra	czj0

case_ZDF_jk2:			*juke box用ZDF展開(MDD展開専用)
	* < a5.l=ZDF address
	* > d7.l=calculated total size
	* > d0.l=mdd true size
	* - all except result parameters
	sub.l	d3,d7
	movem.l	d0-d3/d5/a0-a2/a4,-(sp)

	move.l	lzz_adr(pc),d0
	beq	cant_use_lzz
	move.l	d0,a0

	lea	bufadr(pc),a4

	pea	(a4)			*情報テーブル
	pea	(a5)			*ZDF addr
	jsr	_ref_data(a0)
	addq.w	#8,sp
	tst.l	d0
	bmi	lzz_err

	cmpi.w	#1,ZNumOfData(a4)
	bne	unid_error_zdf		*MDDデータではあり得ない構造です。
	cmpi.w	#$41,ZKind(a4)
	bne	unid_error_zdf		*MDDデータではない。

	add.l	ZSize(a4),d7	*展開後のサイズ
	addq.l	#1,d7
	bclr.l	#0,d7		*.even

	move.l	a5,a1		*DMA転送のための下準備
	add.l	d3,a1
	subq.w	#1,a1		*source
	move.l	d3,d2		*size
	add.l	d3,d7
	move.l	data_addr_tbl(pc),a2
	add.l	d7,a2
	subq.w	#1,a2		*destination

	move.l	d7,-(sp)	*あらかじめメモリは確保しておく
	move.l	data_addr_tbl(pc),-(sp)
	DOS	_SETBLOCK
	addq.w	#8,sp
	tst.l	d0
	bmi	out_mem		*OUT OF MEMORY

	moveq.l	#%0000_1010,d1
	bsr	trans_dma

	lea	bufadr(pc),a4

	sub.l	d3,a2
	addq.w	#1,a2		*a2=zdf先頭アドレス

	pea	(a4)		*情報テーブル
	pea	(a2)		*ZDF addr
	jsr	_ref_data(a0)
	addq.w	#8,sp
	tst.l	d0
	bmi	lzz_err

	move.l	ZSize(a4),-(sp)		*展開後のサイズ分メモリ確保
	DOS	_MALLOC
	addq.w	#4,sp
	tst.l	d0
	bmi	out_mem			*OUT OF MEMORY
	move.l	d0,a1

	pea	(a1)			*push extract
	move.l	ZTopAdr(a4),-(sp)
	jsr	_ext_data(a0)
	addq.w	#8,sp
	tst.l	d0
	bmi	lzz_err

	move.l	ZSize(a4),d2
	move.l	a5,a2
	moveq.l	#5,d1		*dma mode
	bsr	trans_dma

	pea	(a1)
	DOS	_MFREE
	addq.w	#4,sp

	movem.l	(sp)+,d0-d3/d5/a0-a2/a4

	sub.l	d3,d7
	move.l	d7,-(sp)	*あらかじめメモリは確保しておく
	move.l	data_addr_tbl(pc),-(sp)
	DOS	_SETBLOCK
	addq.w	#8,sp
	tst.l	d0
	bmi	out_mem		*OUT OF MEMORY

	move.l	bufadr+ZSize(pc),d0
	rts

read_lzz:
	movem.l	d0-d1/d5/a2,-(sp)
	clr.l	lzz_adr-work(a6)

	bsr	fopen2
	tst.l	d5
	bmi	rl00
	move.w	d5,-(sp)		*close
	DOS	_CLOSE
	addq.l	#2,sp

	pea	$ffff.w
	DOS	_MALLOC
	andi.l	#$00ffffff,d0
	move.l	d0,d1
	move.l	d0,(sp)
	DOS	_MALLOC			* lzz 読み込みのためのメモリ確保
	addq.l	#4,sp
	tst.l	d0
	bmi	out_mem

	movea.l	d0,a2			* a2=LZZの存在アドレス
	add.l	d0,d1			* リミットアドレス
	move.l	d1,-(sp)
	move.l	d0,-(sp)
	pea	open_fn(pc)
	move.b	#1,(sp)
	move.w	#3,-(sp)
	DOS	_EXEC			* ロード
	lea	14(sp),sp
	tst.l	d0
	bmi	rl00

	cmpi.l	#'LzzR',_LzzCheck(a2)	* lzzかチェック
	bne	rl00

	move.l	_LzzSize(a2),-(sp)
	pea	(a2)
	DOS	_SETBLOCK		* メモリブロックを必要な大きさに縮小
	addq.l	#8,sp

	move.l	a2,lzz_adr-work(a6)
rl00:
	movem.l	(sp)+,d0-d1/d5/a2
	rts

trans_dma:			*ＤＭＡ転送($ff00バイト以上も考慮)
	* < d1=mode
	* < d2=size
	* < a1=source addr.
	* < a2=destination addr.
	* - all
	tst.l	d2
	beq	exit_dt
	movem.l	d0-d3/a0-a2,-(sp)
	lea	d3a1(pc),a0
	move.w	d1,d3
	andi.w	#%11,d3
	add.w	d3,d3
	move.w	d3a1_op(pc,d3.w),(a0)+
	move.w	d1,d3
	andi.w	#%1100,d3
	lsr.w	#1,d3
	move.w	d3a2_op(pc,d3.w),(a0)+
	move.l	#$ff00,d3
trans_dma_lp:
	cmp.l	d3,d2
	bcs	go_single_dma

	movem.l	d2/a1-a2,-(sp)
	move.l	d3,d2
	IOCS	_DMAMOVE
	movem.l	(sp)+,d2/a1-a2
d3a1:	ds.w	1
d3a2:	ds.w	1
	sub.l	d3,d2
	bne	trans_dma_lp
bye_dma:
	movem.l	(sp)+,d0-d3/a0-a2
exit_dt:
	rts
d3a1_op:
	nop
	adda.l	d3,a1
	suba.l	d3,a1
	dc.w	-1
d3a2_op:
	nop
	adda.l	d3,a2
	suba.l	d3,a2
	dc.w	-1
go_single_dma:
	IOCS	_DMAMOVE
	bra	bye_dma

get_sakiyomi_name:		*先読みするファイルネームを探す
	* < zpd_scan(=0:scan,=$ff:no scan)
	* < mdd_scan(=0:scan,=$ff:no scan)
	* < a5.l=zmd address
	movem.l	d0-d1/a2/a5,-(sp)
	addq.w	#8,a5		*skip header
pcd_lp01:
	move.b	(a5)+,d1
	bmi	exit_ccd	*共通コマンドエンド
	cmpi.b	#$42,d1
	beq	cnv_mclk	*全音符カウンタセット
	cmpi.b	#$15,d1
	beq	cnv_scm		*ベースチャンネル切り換え
	cmpi.b	#$04,d1
	beq	cnv_vset	*ボイスセット
	cmpi.b	#$05,d1
	beq	cnv_tempo	*テンポ
	cmpi.b	#$18,d1
	beq	cnv_mdtrns	*MIDI生データ転送
	cmpi.b	#$1b,d1
	beq	cnv_vset2	*ボイスセット２
	cmpi.b	#$40,d1
	beq	juke_error	*adpcm data read
	cmpi.b	#$4a,d1
	beq	cnv_wvf		*wave form setting
	cmpi.b	#$60,d1
	beq	juke_error	*#com0:read opmd.cnf
	cmpi.b	#$61,d1
	beq	skip_until0	*#com2:print message
	cmpi.b	#$62,d1
	beq	cnv_midi	*#com22:trans midi data dump
	cmpi.b	#$63,d1
	beq	cnv_abdt	*#com35:read adpcm block data
	cmpi.b	#$7e,d1
	beq	pcd_lp01	*dummy code
	cmpi.b	#$7f,d1
	beq	skip_until0	*#com36:comment
	bra	unid_error	*unknown command
exit_ccd:
	movem.l	(sp)+,d0-d1/a2/a5
	rts

skip	macro	n
	if	n<=8	then
	addq.w	#n-1,a5
	else
	lea	n-1(a5),a5
	endif
	bra	pcd_lp01
	endm

cnv_vset:	*音色設定
cnv_vset2:	*ボイスセット２
	skip	57

cnv_tempo:	*テンポ
	skip	3

cnv_mclk:	*全音符カウンタセット
	skip	6

cnv_scm:	*ベースチャンネル切り換え
	skip	2

cnv_mdtrns:	*MIDI生データ転送
	move.b	(a5)+,d0
	lsl.w	#8,d0
	move.b	(a5)+,d0	*get number of data
	adda.w	d0,a5
	bra	pcd_lp01

cnv_midi:	*#com22:trans midi data dump
	tst.b	mdd_scan-work(a6)
	bne	skip_until0
	st.b	mdd_scan-work(a6)
	lea	mddfilename(pc),a2
	bra	adbt0		*あとは同じ

cnv_wvf:
	moveq.l	#0,d0
	move.b	(a5)+,d0
	lsl.w	#8,d0
	move.b	(a5)+,d0	*get number of data
	add.l	d0,d0
	lea	4(a5,d0.l),a5	*skip wave_n,loop_type,loop_addr.
	bra	pcd_lp01

cnv_abdt:	*#com35:read adpcm block data
	tst.b	zpd_scan-work(a6)
	bne	skip_until0
	st.b	zpd_scan-work(a6)
	lea	filename(pc),a2
adbt0:
	moveq.l	#$7e,d0
adbt_lp:
	move.b	d0,-1(a5)
	move.b	(a5)+,(a2)+
	bne	adbt_lp
	move.b	d0,-1(a5)
	bra	pcd_lp01
skip_until0:
	tst.b	(a5)+
	bne	skip_until0
	bra	pcd_lp01

m_play_:			*'-P'なしの場合の演奏開始
	* < a2.l=command line address
	bsr	chk_drv		*ドライバ常駐チェック
	bmi	no_drv		*ドライバが組み込まれてない
m_pllp00_:
	move.b	(a2)+,d0
	beq	just_m_p	*ファイル名無しの場合は…
	cmpi.b	#' ',d0
	bls	m_pllp00_
	subq.w	#1,a2
	bra	init_mpwk

synchro_play:
	move.w	#RTS,exit-work(a6)
	moveq.l	#-1,d2
	Z_MUSIC	#$54		*interception mode
	bsr	m_play
	moveq.l	#-1,d2
@@:
	Z_MUSIC	#$55

	bitsns	0,d1
	btst.l	#1,d1
	bne	@f		*取り消し

	cmpi.b	#$fa,d0		*start message
	bne	@b
	moveq.l	#0,d2
	Z_MUSIC	#$54		*release interception & play
	bra	exit+2
@@:
	moveq.l	#1,d2
	Z_MUSIC	#$54		*release interception
	bra	exit+2

m_play:				*演奏開始
	* < a2.l=command line address
	bsr	chk_drv		*ドライバ常駐チェック
	bmi	no_drv		*ドライバが組み込まれてない
m_pllp00:
	move.b	(a2)+,d0
	beq	just_m_p	*ファイル名無しの場合は…
	cmpi.b	#' ',d0
	bls	m_pllp00
	subq.w	#1,a2

	bsr	chk_num2	*数字?
	bpl	trk_play
init_mpwk:
	lea	setup_fn(pc),a0
	move.l	a0,sfp-work(a6)
	moveq.l	#0,d0
	move.b	d0,retry-work(a6)
	move.b	d0,setup1(a0)		*ワーク初期化
	move.b	d0,setup2(a0)
	move.b	d0,setup3(a0)
	move.b	d0,setup4(a0)
	bsr	get_org_fn
	lea	ZMD_kaku(pc),a1
	bsr	set_fn
m_pl_lp:			*セットアップファイルがあるなら最大４つまで指定可能
	cmpi.b	#',',d0
	bne	exec_setup
	bsr	set_stup
	add.l	#fn_size,sfp-work(a6)
	bra	m_pl_lp
exec_setup:			*セットアップファイルの実行
	lea	setup_fn(pc),a2
exec_stlp:
	tst.b	(a2)
	beq	exec_play
	pea	(a2)
	pea	(a2)
	bsr	fopen
	tst.l	d5
	bmi	file_not_found
	bsr	read		*>d3=data size,a5=data address
	move.l	(sp)+,a2
	bsr	chk_mdd?	*>a2.l=dest. filename
	bsr	self_output	*自己出力
	move.l	(sp)+,a2
	lea	fn_size(a2),a2
	bra	exec_stlp
exec_play:
	lea	filename(pc),a2
	bsr	fopen
	tst.l	d5
	bpl	do_exc_pl
	bsr	skip_peri2
excpl0:				*ZMDでだめならZMS
	move.b	(a2)+,d0
	beq	file_not_found2
	cmpi.b	#'.',d0
	bne	excpl0
	moveq.l	#0,d0
	move.b	retry(pc),d0
	cmpi.b	#3,d0
	bhi	file_not_found2
	add.w	d0,d0
	move.w	kktbl(pc,d0.w),d0
	lea	kktbl(pc,d0.w),a5
	move.b	(a5)+,(a2)+
	move.b	(a5)+,(a2)+
	move.b	(a5)+,(a2)+
	addq.b	#1,retry-work(a6)
	bra	exec_play
kktbl:
	dc.w	ZMD_kaku-kktbl
	dc.w	ZMS_kaku-kktbl
	dc.w	OPM_kaku-kktbl
	dc.w	ZDF_kaku-kktbl

skip_peri0:
	cmpi.b	#'.',(a0)
	bne	exit_sp0
	addq.w	#1,a0
	bra	skip_peri0
exit_sp0:
	rts

skip_peri2:
	cmpi.b	#'.',(a2)
	bne	exit_sp2
	addq.w	#1,a2
	bra	skip_peri2
exit_sp2:
	rts

skip_peri4:
	cmpi.b	#'.',(a4)
	bne	exit_sp4
	addq.w	#1,a4
	subq.l	#1,d4
	bra	skip_peri4
exit_sp4:
	rts

do_exc_pl:
	bsr	read		*>d3:data size,a5=data address
	cmpi.l	#'ZDF0',(a5)	*ZDF?
	beq	case_ZDF
	cmpi.b	#$10,(a5)	*MDD OR ZMS
	bne	go_on_p
	cmpi.b	#'A',3(a5)
	beq	go_on_p
do_exc_pl0:
	lea	8(a5),a2
next_cmnt:			*初めのコメントを画面に出力
	cmpi.b	#$7f,(a2)+
	bne	go_on_p_zmd

	tst.b	non_disp-work(a6)
	bne	aa2
	move.w	#2,-(sp)
	pea	(a2)
	DOS	_FPUTS
	addq.w	#6,sp

	move.w	#2,-(sp)
	pea	CRLF(pc)
	DOS	_FPUTS
	addq.w	#6,sp
aa2:
srch_zr:
	tst.b	(a2)+
	bne	srch_zr
	bra	next_cmnt
go_on_p:			*演奏開始
	lea	filename(pc),a2
	bsr	chk_mdd?
	bsr	self_output	*< d3.l=size, a5=data address, a2=out name
	bra	exit
go_on_p_zmd:			*ZMD演奏開始
	lea	7(a5),a1	*version number
	move.l	d3,d2		*size
	Z_MUSIC	#$11
	tst.l	d0
	beq	exit
	cmpi.b	#5,d0		*track buffer is not enough
	beq	t_err_5
	cmpi.b	#66,d0		*バージョン不一致
	beq	t_err_66
	cmpi.b	#68,d0		*MIDIボードがありません
	beq	t_err_68
	bra	exit
case_ZDF:
	* < d3=size
	* < a5.l=data address
	bsr	read_lzz
	move.l	lzz_adr(pc),a3
	lea	bufadr(pc),a2
	pea	(a2)
	pea	(a5)
	jsr	_ref_data(a3)
	addq.w	#8,sp
	tst.l	d0
	bmi	lzz_err

	moveq.l	#0,d3
	move.l	d3,a5
	move.w	ZNumOfData(a2),d5
lzze_lp:
	move.l	ZSize(a2),-(sp)
	DOS	_MALLOC
	addq.w	#4,sp
	tst.l	d0
	bmi	out_mem		*OUT OF MEMORY
	move.l	d0,a1

	pea	(a1)		*push extract
	move.l	ZTopAdr(a2),-(sp)
	jsr	_ext_data(a3)
	addq.w	#8,sp
	tst.l	d0
	bmi	lzz_err

	move.w	ZKind(a2),d0	*get data type
	bne	lzze0		*ZMD以外なら
	move.l	ZSize(a2),d3	*get zmd size
	move.l	a1,a5		*get zmd addr.
	cmpi.w	#1,d5		*ZMDだけならば
	beq	do_exc_pl0	*メインへ帰還
	bsr	erase_zpd_mdd	*adpcm_block_data,midi_dumpをつぶす
	bra	lzze2
lzze0:				*ZMDデータ以外のものの実行
	movem.l	d3/a2/a5,-(sp)
	move.l	a1,a5
	move.l	ZSize(a2),d3
	lea	OPM(pc),a2
	cmpi.b	#$04,d0		*case:ZMS
	beq	lzze1
	cmpi.w	#$20,d0		*case:ZPD
	beq	lzze1
	cmpi.w	#$41,d0		*case:MDD
	bne	unid_error_zdf	*わけわかんないファイルです。
	lea	MIDI(pc),a2
lzze1:
	bsr	self_output
	pea	(a1)
	DOS	_MFREE
	addq.w	#4,sp
	movem.l	(sp)+,d3/a2/a5
lzze2:
	lea	ZNext(a2),a2
	subq.w	#1,d5
	bne	lzze_lp
	tst.l	d3
	bne	do_exc_pl0
	bra	exit

erase_zpd_mdd:				*ZPD登録/MDD登録コマンドの書きつぶし
	* < d5.w=num of data
	* < a5.l=zmd address
	* < a2.l=bufadr
	movem.l	d0-d7/a0-a6,-(sp)
	lea	ZNext(a2),a2
	subq.w	#1,d5
ezm_lp:
	move.w	ZKind(a2),d0
	cmpi.w	#$20,d0
	bne	ezm1
	clr.b	zpd_scan-work(a6)
	st.b	mdd_scan-work(a6)
	bra	ezm2
ezm1:
	cmpi.w	#$41,d0
	bne	unid_error_zdf		*わけわかんないファイルです。
	st.b	zpd_scan-work(a6)
	clr.b	mdd_scan-work(a6)
ezm2:
	bsr	get_sakiyomi_name	*ダミーでかきつぶす
	lea	ZNext(a2),a2
	subq.w	#1,d5
	bne	ezm_lp
	movem.l	(sp)+,d0-d7/a0-a6
	rts

just_m_p:			*単にm_play()を実行する。
	moveq.l	#0,d2
	moveq.l	#0,d3
	moveq.l	#0,d4
	Z_MUSIC	#$08
	bra	exit

trk_play:
	move.l	a2,a4
	moveq.l	#$7f,d4		*dummy
	bsr	bit_pat_set
	exg	d4,d5
	Z_MUSIC	#$08
	exg	d4,d5
	bra	exit

init_zmsc:
	bsr	chk_drv		*ドライバ常駐チェック
	bmi	no_drv		*ドライバが組み込まれてない
	Z_MUSIC	#$00
	Z_MUSIC	#$0d
	bra	exit

chk_mdd?:			*.mdd?かどうかチェックする
	* < a2.l=filename address
	bsr	skip_peri2
cm_lp0
	move.b	(a2)+,d0
	beq	not_mdd
	cmpi.b	#'.',d0
	bne	cm_lp0
	move.b	(a2)+,d0
	bsr	mk_capital
	cmpi.b	#'M',d0
	bne	not_mdd
	move.b	(a2)+,d0
	bsr	mk_capital
	cmpi.b	#'D',d0
	bne	not_mdd
	move.b	(a2)+,d0
	bsr	mk_capital
	cmpi.b	#'D',d0
	bne	not_mdd
	lea	MIDI(pc),a2
	moveq.l	#-1,d0
	rts
not_mdd:
	lea	OPM(pc),a2
	moveq.l	#0,d0
	rts

m_total:			*合計値計算
	* < a2.l=command line address
	bsr	chk_drv		*ドライバ常駐チェック
	bmi	no_drv		*ドライバが組み込まれてない
m_ttlp00:
	move.b	(a2)+,d0
	beq	just_m_t	*ファイル名無しの場合は…
	cmpi.b	#' ',d0
	bls	m_ttlp00
	subq.w	#1,a2

	bsr	get_org_fn
	lea	ZMD_kaku(pc),a1
	bsr	set_fn
	clr.b	retry-work(a6)
exc_clc_opn:
	lea	filename(pc),a2
	bsr	fopen
	tst.l	d5
	bpl	do_ex_clc
	bsr	skip_peri2
excclc0:				*ZMDでだめならZMS
	move.b	(a2)+,d0
	beq	file_not_found2
	cmpi.b	#'.',d0
	bne	excclc0
	moveq.l	#0,d0
	move.b	retry(pc),d0
	cmpi.b	#3,d0
	bhi	file_not_found2
	add.w	d0,d0
	lea	kktbl(pc),a5
	move.w	(a5,d0.w),d0
	lea	(a5,d0.w),a5
	move.b	(a5)+,(a2)+
	move.b	(a5)+,(a2)+
	move.b	(a5)+,(a2)+
	addq.b	#1,retry-work(a6)
	bra	exc_clc_opn
do_ex_clc:
	Z_MUSIC	#$00
	bsr	read		*>d3:data size,a5=data address
	cmpi.l	#'ZDF0',(a5)	*ZDF?
	beq	case_ZDF_clc
	cmpi.b	#$10,(a5)	*MDD OR ZMS
	bne	go_on_clc
	cmpi.b	#'A',3(a5)
	beq	not_play_data
do_exc_clc0:
	lea	8(a5),a2
	bra	go_on_clc_zmd
go_on_clc:			*演奏開始
	bsr	clr_ext_file_read1
	lea	filename(pc),a2
	bsr	chk_mdd?
	bmi	not_play_data	*演奏データではない
goc0:
	moveq.l	#-1,d2
	Z_MUSIC	#$54		*intercept mode
	bsr	self_output
	bra	do_clc
go_on_clc_:			*ZMSのZDF演奏開始
	bsr	clr_ext_file_read1
	lea	filename(pc),a2
	bsr	chk_mdd?
	bra	goc0
go_on_clc_zmd:			*ZMD演奏開始
	bsr	clr_ext_file_read2
	lea	7(a5),a1	*version number
	moveq.l	#-1,d2
	Z_MUSIC	#$54		*intercept mode
	moveq.l	#0,d2
	Z_MUSIC	#$11
	tst.l	d0
	beq	do_clc
*	cmpi.b	#5,d0		*track buffer is not enough
*	beq	t_err_5
	cmpi.b	#66,d0		*バージョン不一致
	beq	t_err_66
	cmpi.b	#68,d0		*MIDIボードがありません
	beq	t_err_68
	bra	exit
do_clc:
	moveq.l	#1,d2
	Z_MUSIC	#$19
	move.l	d0,-(sp)
	Z_MUSIC	#$54		*release intercept mode
	move.l	(sp)+,d0
	beq	do_Q_print
	cmp.l	#-1,d0
	beq	err_remain	*なんらかのエラーが発生したので計算は行なわれなかった
	bra	exit
just_m_t:			*単にm_total()を実行する。
	moveq.l	#1,d2
	Z_MUSIC	#$19
	tst.l	d0
	beq	do_Q_print
	cmpi.l	#-1,d0
	beq	err_remain2	*なんらかのエラーが発生したので計算は行なわれなかった
	bra	exit
do_Q_print:
	pea	Q_mes(pc)
	DOS	_PRINT
	addq.w	#4,sp

	Z_MUSIC	#$3c		*a4=seq_wk_tbl base addr.
	move.l	a0,seq_wk_tbl-work(a6)
	Z_MUSIC	#$3a		*a0=play_trk_tbl,d0=real_ch_tbl
	moveq.l	#pl_max-1,d1
	moveq.l	#4,d2
clc_ttl_lp03:
	lea	suji(pc),a2
	moveq.l	#0,d0
	move.b	(a0)+,d0
	bmi	clc_ttl_all_end
	move.l	d0,d4
	lsl.w	#wk_size2,d4
	movea.l	seq_wk_tbl(pc),a5
	adda.w	d4,a5
	addq.b	#1,d0		*1-80
	bsr	num_to_str
	addq.w	#8,a2
	tst.b	1(a2)
	bne	ct0
	move.b	(a2)+,(a2)+	*１桁のケース
	clr.b	(a2)		*end code
	subq.w	#2,a2
	move.b	#' ',(a2)
ct0:
	pea	(a2)
	DOS	_PRINT
*	addq.w	#4,sp
	pea	sep_(pc)
	DOS	_PRINT
*	addq.w	#4,sp
	addq.w	#8,sp			*!!!

	move.l	p_total_olp(a5),d0	*ループ外
	lea	suji(pc),a1
	move.l	a1,a2
	bsr	get_hex32
	pea	(a2)
	DOS	_PRINT
*	addq.w	#4,sp

	pea	SPC2+1(PC)
	DOS	_PRINT
*	addq.w	#4,sp
	addq.w	#8,sp			*!!!

	move.l	p_total(a5),d0		*ループ内
	bsr	get_hex32
	pea	(a2)
	DOS	_PRINT
	addq.w	#4,sp

	subq.b	#1,d2
	bne	ttl_tab
	pea	CRLF(PC)
	DOS	_PRINT
	addq.w	#4,sp
	moveq.l	#4,d2
	bra	clc_ttl0
ttl_tab:
	pea	SPC2(PC)
	DOS	_PRINT
	addq.w	#4,sp
clc_ttl0:
	dbra	d1,clc_ttl_lp03
clc_ttl_all_end:
	cmpi.b	#4,d2
	beq	exit_clc
	pea	CRLF(PC)	*最後の改行
	DOS	_PRINT
	addq.w	#4,sp
exit_clc:
*	Z_MUSIC	#$00		*m_init
	bra	exit

case_ZDF_clc:
	* < d3=size
	* < a5.l=data address
	bsr	read_lzz
	move.l	lzz_adr(pc),a3
	lea	bufadr(pc),a2
	pea	(a2)
	pea	(a5)
	jsr	_ref_data(a3)
	addq.w	#8,sp
	tst.l	d0
	bmi	lzz_err

	move.l	ZSize(a2),-(sp)
	DOS	_MALLOC
	addq.w	#4,sp
	tst.l	d0
	bmi	out_mem		*OUT OF MEMORY
	move.l	d0,a1

	pea	(a1)		*push extract
	move.l	ZTopAdr(a2),-(sp)
	jsr	_ext_data(a3)
	addq.w	#8,sp
	tst.l	d0
	bmi	lzz_err

	move.l	ZSize(a2),d3	*get zmd size
	move.l	a1,a5		*get zmd addr.
	move.w	ZKind(a2),d0	*get data type
	beq	do_exc_clc0	*メインへ帰還
	cmpi.w	#$04,d0		*ZMS?
	beq	go_on_clc_
	bra	not_play_data	*ZMD/ZMS以外なら

clr_ext_file_read1:		*外部ファイルを読むような共通コマンドの撲滅
	* < a5.l=zms address
	* < d3.l=zms size
	movem.l	d0/d4/a0-a1/a4,-(sp)
	move.l	d3,d4
	move.l	a5,a4
cefr_lp:
	bsr	skip_spc
	lea	shp_com_tbl(pc),a1
cefr_lp00:
	move.l	a4,a0
cefr_lp01:
	move.b	(a0)+,d0
	bsr	mk_capital
	cmp.b	(a1)+,d0
	bne	next_koho
	tst.b	(a1)
	bne	cefr_lp01
cefr_lp02:				*スペースで埋める
	cmpi.b	#' ',(a4)
	bcs	no_more_koho
	subq.l	#1,d4
	bmi	cefr_end
	move.b	#' ',(a4)+
	bra	cefr_lp02
next_koho:
	tst.b	(a1)+
	bne	next_koho
	tst.b	(a1)
	bmi	no_more_koho
	bra	cefr_lp00
no_more_koho:				*もう候補がないので改行
	subq.l	#1,d4
	bmi	cefr_end
	cmpi.b	#$0a,(a4)+
	beq	cefr_lp
	bra	no_more_koho
cefr_end
	movem.l	(sp)+,d0/d4/a0-a1/a4
	rts

shp_com_tbl:
	dc.b	'.ADPCM_LIST',0
	dc.b	'.O',0
	dc.b	'.MIDI_DUMP',0
	dc.b	'.ADPCM_BLOCK_DATA',0
	dc.b	'#ADPCM_LIST',0
	dc.b	'#O',0
	dc.b	'#MIDI_DUMP',0
	dc.b	'#ADPCM_BLOCK_DATA',0
	dc.b	-1
	.even

clr_ext_file_read2:		*外部ファイルを読むような共通コマンドの撲滅
	* < a5.l=zmd address
	movem.l	d0-d1/a2/a5,-(sp)
	addq.w	#8,a5		*skip header
_pcd_lp01:
	move.b	(a5)+,d1
	bmi	_exit_ccd	*共通コマンドエンド
	cmpi.b	#$42,d1
	beq	_cnv_mclk	*全音符カウンタセット
	cmpi.b	#$15,d1
	beq	_cnv_scm	*ベースチャンネル切り換え
	cmpi.b	#$04,d1
	beq	_cnv_vset	*ボイスセット
	cmpi.b	#$05,d1
	beq	_cnv_tempo	*テンポ
	cmpi.b	#$18,d1
	beq	_cnv_mdtrns	*MIDI生データ転送
	cmpi.b	#$1b,d1
	beq	_cnv_vset2	*ボイスセット２
	cmpi.b	#$40,d1
	beq	_cnv_adpcm_read	*adpcm data read
	cmpi.b	#$4a,d1
	beq	_cnv_wvf	*wave form setting
	cmpi.b	#$60,d1
	beq	_cnv_opmdcnf	*#com0:read opmd.cnf
	cmpi.b	#$61,d1
	beq	_skip_until0	*#com2:print message
	cmpi.b	#$62,d1
	beq	_cnv_midi	*#com22:trans midi data dump
	cmpi.b	#$63,d1
	beq	_cnv_abdt	*#com35:read adpcm block data
	cmpi.b	#$7e,d1
	beq	_pcd_lp01	*dummy code
	cmpi.b	#$7f,d1
	beq	_skip_until0	*#com36:comment
	bra	unid_error	*unknown command
_exit_ccd:
	movem.l	(sp)+,d0-d1/a2/a5
	rts

_skip	macro	n
	if	n<=8	then
	addq.w	#n-1,a5
	else
	lea	n-1(a5),a5
	endif
	bra	_pcd_lp01
	endm

_cnv_vset:	*音色設定
_cnv_vset2:	*ボイスセット２
	_skip	57

_cnv_tempo:	*テンポ
	_skip	3

_cnv_mclk:	*全音符カウンタセット
	_skip	6

_cnv_scm:	*ベースチャンネル切り換え
	_skip	2

_cnv_mdtrns:	*MIDI生データ転送
	move.b	(a5)+,d0
	lsl.w	#8,d0
	move.b	(a5)+,d0	*get number of data
	adda.w	d0,a5
	bra	_pcd_lp01

_cnv_wvf:
	moveq.l	#0,d0
	move.b	(a5)+,d0
	lsl.w	#8,d0
	move.b	(a5)+,d0	*get number of data
	add.l	d0,d0
	lea	4(a5,d0.l),a5	*skip wave_n,loop_type,loop_addr.
	bra	_pcd_lp01

_cnv_adpcm_read:
	moveq.l	#$7e,d0
	moveq.l	#19-1,d1
	move.b	d0,-1(a5)
car_lp:
	move.b	d0,(a5)+
	dbra	d1,car_lp
	tst.b	(a5)
	bne	_adbt_lp
	move.b	d0,(a5)+
	move.b	d0,(a5)+
	move.b	d0,(a5)+
	move.b	d0,(a5)+
	bra	_pcd_lp01

_cnv_opmdcnf:
_cnv_midi:	*#com22:trans midi data dump
_cnv_abdt:	*#com35:read adpcm block data
	moveq.l	#$7e,d0
_adbt_lp:
	move.b	d0,-1(a5)
	tst.b	(a5)+
	bne	_adbt_lp
	move.b	d0,-1(a5)
	bra	_pcd_lp01
_skip_until0:
	tst.b	(a5)+
	bne	_skip_until0
	bra	_pcd_lp01

mask_ch:			*チャンネル・マスク
	* < a2.l=command line address
	bsr	chk_drv		*ドライバ常駐チェック
	bmi	no_drv		*ドライバが組み込まれてない
msk_lp00:
	move.b	(a2)+,d0
	beq	reset_mask
	cmpi.b	#' ',d0
	bls	msk_lp00
	subq.w	#1,a2

	move.l	a2,a4
	moveq.l	#$7f,d4		*dummy
	bsr	chk_num		*数字?
	bpl	ms0
	bsr	macro_ch_set
	beq	reset_mask
	bra	ms1
ms0:
	bsr	bit_pat_set
ms1:
	Z_MUSIC	#$44
	bra	exit

reset_mask:			*mask取り消し
	moveq.l	#0,d2
	Z_MUSIC	#$44
	bra	exit

solo_play:			*Solo play
	* < a2.l=command line address
	bsr	chk_drv		*ドライバ常駐チェック
	bmi	no_drv		*ドライバが組み込まれてない
slp_lp00:
	move.b	(a2)+,d0
	beq	reset_mask
	cmpi.b	#' ',d0
	bls	slp_lp00
	subq.w	#1,a2

	move.l	a2,a4
	moveq.l	#$7f,d4		*dummy
	bsr	chk_num		*数字?
	bpl	sl0
	bsr	macro_ch_set
	beq	reset_mask
	bra	sl1
sl0:
	bsr	bit_pat_set
sl1:
	not.l	d2
	Z_MUSIC	#$44
	bra	exit

macro_ch_set:			*マクロチャンネル指定
	* > eq no parameters
	* > mi parameters exist
	moveq.l	#0,d2
	moveq.l	#0,d3
mcc_lp01:
	move.b	(a4),d0
	bsr	mk_capital
	cmpi.b	#'A',d0
	beq	mcc22
	cmpi.b	#'F',d0
	bne	mcc1
	addq.w	#1,a4
	bsr	skip_spc
	bsr	chk_num
	bmi	mcc0_
	bsr	asc_to_n
	tst.l	d1
	beq	illegal_p
	cmpi.l	#8,d1
	bhi	illegal_p
	add.b	#15,d1
	bra	mcc23
mcc0_:
	ori.l	#$00ff_0000,d2
	bra	next_mcc
mcc1:
	cmpi.b	#'M',d0
	bne	mcc2
	addq.w	#1,a4
	bsr	skip_spc
	bsr	chk_num
	bmi	mcc1_
	bsr	asc_to_n
	tst.l	d1
	beq	illegal_p
	cmpi.l	#16,d1
	bhi	illegal_p
	subq.b	#1,d1
	bra	mcc23
mcc1_:
	ori.l	#$0000_ffff,d2
	bra	next_mcc
mcc2:
	cmpi.b	#'P',d0
	bne	mcc3
mcc22:
	addq.w	#1,a4
	bsr	skip_spc
	bsr	chk_num
	bmi	mcc2_
	bsr	asc_to_n
	tst.l	d1
	beq	illegal_p
	cmpi.l	#8,d1
	bhi	illegal_p
	add.b	#23,d1
mcc23:
	bset.l	d1,d2
	bra	next_mcc
mcc2_:
	ori.l	#$ff00_0000,d2
next_mcc:
	moveq.l	#-1,d3
	bsr	skip_sep
	bra	mcc_lp01
mcc3:
	bsr	rev_ch
	tst.l	d3
	rts

rev_ch:				*m_ch() (Bn)考慮
	* X d0-d1,a3
	movem.l	d0-d1/a3,-(sp)
	move.l	real_ch_tbl(pc),a3
	tst.b	(a3)
	bne	rvc0		*case:(b1)
				*case:(b0)
	moveq.l	#0,d0
	moveq.l	#0,d1
	swap	d2
	move.w	d2,d0
	andi.w	#$01ff,d0	*d0=fm,adpcm1
	move.w	d2,d1		*save
	clr.w	d2
	lsr.l	#7,d2
	or.l	d0,d2
	andi.w	#$fe00,d1
	swap	d1
	or.l	d1,d2
rvc0:
	movem.l	(sp)+,d0-d1/a3
	rts

output_level:			*出力設定
	* < a2.l=command line address
	bsr	chk_drv		*ドライバ常駐チェック
	bmi	no_drv		*ドライバが組み込まれてない
sopl_lp00:
	move.b	(a2)+,d0
	beq	reset_sopl
	cmpi.b	#' ',d0
	bls	sopl_lp00
	subq.w	#1,a2

	bsr	chk_num2	*数字?
	bmi	reset_sopl

	move.l	a2,a4
	moveq.l	#$7f,d4
	bsr	asc_to_n
	move.w	d1,-(sp)
	bsr	chk_num
	bpl	gl0
	moveq.l	#-1,d2
	bra	go_sopl
gl0:
	bsr	bit_pat_set
go_sopl:
	move.w	(sp)+,d3
	Z_MUSIC	#$47
	bra	exit
reset_sopl:
	moveq.l	#$80,d3
	moveq.l	#-1,d2
	Z_MUSIC	#$47
	bra	exit

kakuchoshi:			*拡張子を設定
	* < a0=filename address
	* < a2=拡張子アドレス
	* X a0
	bsr	skip_peri0
	moveq.l	#fn_size-1,d0
kkchs_lp:
	move.b	(a0)+,d0
	beq	do_kkchs
	cmpi.b	#'.',d0
	beq	find_period
	dbra	d0,kkchs_lp
do_kkchs:
	subq.l	#1,a0
	move.b	#'.',(a0)+
	move.b	(a2)+,(a0)+
	move.b	(a2)+,(a0)+
	move.b	(a2)+,(a0)+
	clr.b	(a0)
	rts
find_period:
	cmpi.b	#' ',(a0)
	bls	do_kkchs	*'.'はあっても拡張子がないケース
	rts

get_org_fn:
	movem.l	a0/a2,-(sp)
	lea	org_fn(pc),a0
gef0:
	move.b	(a2)+,(a0)+
	bne	gef0
	movem.l	(sp)+,a0/a2
	rts

set_fn:				*ファイルネームのセット
	* < a1.l=拡張子source
	* < a2.l=source filename
	* > d0.b=last chr
	bsr	skip_peri2
	movem.l	d1/a0,-(sp)
	lea	filename(pc),a0
	clr.b	d1
setfnlp:
	move.b	(a2)+,d0
	cmpi.b	#'.',d0
	bne	chk_sp_
	st.b	d1
chk_sp_:
	cmpi.b	#' ',d0
	bls	exit_setfn
	cmpi.b	#',',d0
	beq	exit_setfn
	move.b	d0,(a0)+
	bra	setfnlp
exit_setfn:
	tst.b	d1		*拡張子省略かどうか
	bne	set_edc_	*省略しなかった
	move.b	#'.',(a0)+	*拡張子をセット
	move.b	(a1)+,(a0)+
	move.b	(a1)+,(a0)+
	move.b	(a1)+,(a0)+
set_edc_:
	clr.b	(a0)
	movem.l	(sp)+,d1/a0
	rts

set_fn2:			*ファイルネームのセット(case:juke box)
	* < a4.l=source
	* > d0.b=last chr
	bsr	skip_peri4
	movem.l	d1/a0,-(sp)
	lea	filename(pc),a0
	clr.b	d1
setfn2lp:
	subq.l	#1,d4
	move.b	(a4)+,d0
	cmpi.b	#'.',d0
	bne	chk_sp_2
	st.b	d1
chk_sp_2:
	cmpi.b	#' ',d0
	bls	exit_setfn2
	cmpi.b	#',',d0
	beq	exit_setfn2
	move.b	d0,(a0)+
	bra	setfn2lp
exit_setfn2:
	tst.b	d1		*拡張子省略かどうか
	bne	set_edc_2	*省略しなかった
	move.b	#'.',(a0)+	*拡張子をセット
	move.b	#'Z',(a0)+
	move.b	#'M',(a0)+
	move.b	#'D',(a0)+
set_edc_2:
	clr.b	(a0)
	movem.l	(sp)+,d1/a0
	rts

set_stup:
	* < a2.l=command line address
	* > d0.b=last chr
	* X a0
	move.l	sfp(pc),a0
setsulp:
	move.b	(a2)+,d0
	cmpi.b	#' ',d0
	bls	exit_setsu
	cmpi.b	#',',d0
	beq	exit_setsu
	move.b	d0,(a0)+
	bra	setsulp
exit_setsu:
	clr.b	(a0)
	rts

m_stop:				*演奏停止
	bsr	chk_drv		*ドライバ常駐チェック
	bmi	no_drv		*ドライバが組み込まれてない

	bsr	chk_num2	*数字?
	bpl	trk_stop

	moveq.l	#0,d2		*m_stop_all
	moveq.l	#0,d3		*m_stop_all
	moveq.l	#0,d4		*m_stop_all
	Z_MUSIC	#$0a		*m_stop
	bra	exit

trk_stop:
	move.l	a2,a4
	moveq.l	#$7f,d4		*dummy
	bsr	bit_pat_set
	exg	d4,d5
	Z_MUSIC	#$0a
	exg	d4,d5
	bra	exit

m_cont:				*演奏停止
	bsr	chk_drv		*ドライバ常駐チェック
	bmi	no_drv		*ドライバが組み込まれてない

	bsr	chk_num2	*数字?
	bpl	trk_cont

	moveq.l	#0,d2		*m_cont_all
	moveq.l	#0,d3		*m_cont_all
	moveq.l	#0,d4		*m_cont_all
	Z_MUSIC	#$0b		*m_cont
	bra	exit

trk_cont:
	move.l	a2,a4
	moveq.l	#$7f,d4		*dummy
	bsr	bit_pat_set
	exg	d4,d5
	Z_MUSIC	#$0b
	exg	d4,d5
	bra	exit

fadeout:			*fadeout
	bsr	chk_drv		*ドライバ常駐チェック
	bmi	no_drv		*ドライバが組み込まれてない

	bsr	skip_spc
	cmpi.b	#'-',(a2)
	beq	get_f_spd
	bsr	chk_num2	*数字?
	bpl	get_f_spd
d_fo:
	moveq.l	#16,d2		*m_stop_all
	Z_MUSIC	#$1a		*m_stop
	bra	exit

get_f_spd:
	move.l	a2,a4
	moveq.l	#$7f,d4
	bsr	asc_to_n
	move.l	d1,d2
	Z_MUSIC	#$1a
	bra	exit

midi_ope:			*MIDIデータの取り込み
	* < a2.l=command line address
	bsr	chk_drv		*ドライバ常駐チェック
	bmi	no_drv		*ドライバが組み込まれてない
m_oplp00:
	move.b	(a2)+,d0
	beq	rec_mode_on	*ファイル名無しの場合は…
	cmpi.b	#' ',d0
	bls	m_oplp00
	subq.w	#1,a2

	lea	MDD_kaku(pc),a1
	bsr	set_fn

	tst.b	non_disp-work(a6)
	bne	aa3
	move.w	#2,-(sp)
	pea	saving_mes(pc)
	DOS	_FPUTS
	addq.w	#6,sp
aa3:
	moveq.l	#0,d2
	Z_MUSIC	#$17		*> a0.l=data address,d0.l=data size
	cmpa.l	#0,a0
	beq	midi_err
	move.l	d0,d2
	beq	midi_err
	bmi	midi_err
				*ファイルの書き出し
	move.w	#32,-(sp)
	pea	filename(pc)
	DOS	_CREATE
	addq.w	#6,sp
	move.l	d0,d5		*d5.w=file handle
	bmi	write_err

	move.l	d2,-(sp)	*data size
	pea	(a0)		*data addr
	move.w	d5,-(sp)
	DOS	_WRITE
	lea	10(sp),sp
	tst.l	d0
	bmi	write_err

	move.w	d5,-(sp)
	DOS	_CLOSE
	addq.w	#2,sp
	bra	exit

rec_mode_on:			*単にm_rec()を実行する。
	Z_MUSIC	#$16

	tst.b	non_disp-work(a6)
	bne	exit
	move.w	#2,-(sp)
	pea	rec_st_mes(pc)
	DOS	_FPUTS
	addq.w	#6,sp

	bra	exit

midi_send:			*MIDIデータの楽器への転送
	* < a2.l=command line address
	bsr	chk_drv		*ドライバ常駐チェック
	bmi	no_drv		*ドライバが組み込まれてない
m_sdlp00:
	move.b	(a2)+,d0
	beq	print_hlp	*ファイル名無しの場合は…
	cmpi.b	#' ',d0
	bls	m_sdlp00
	subq.w	#1,a2

	lea	MDD_kaku(pc),a1
	bsr	set_fn

	tst.b	non_disp-work(a6)
	bne	aa4
	move.w	#2,-(sp)
	pea	trns_mes(pc)
	DOS	_FPUTS
	addq.w	#6,sp
aa4:
	lea	filename(pc),a2
	bsr	fopen
	tst.l	d5
	bmi	file_not_found
	bsr	read
	lea	MIDI(pc),a2
	bsr	self_output
	bra	exit

midi_send_b:			*バイナリMIDIデータの楽器への転送
	* < a2.l=command line address
	bsr	chk_drv		*ドライバ常駐チェック
	bmi	no_drv		*ドライバが組み込まれてない
m_sdlp00_:
	move.b	(a2)+,d0
	beq	print_hlp	*ファイル名無しの場合は…
	cmpi.b	#' ',d0
	bls	m_sdlp00_
	subq.w	#1,a2

	lea	BMD_kaku(pc),a1
	bsr	set_fn

	tst.b	non_disp-work(a6)
	bne	aa5
	move.w	#2,-(sp)
	pea	trns_mes(pc)
	DOS	_FPUTS
	addq.w	#6,sp
aa5:
	lea	filename(pc),a2
	bsr	fopen
	tst.l	d5
	bmi	file_not_found
	bsr	read		*>d3,a5
	move.l	d3,d2
	move.l	a5,a1
	Z_MUSIC	#$18
	tst.l	d0
	beq	exit
	cmpi.b	#68,d0		*MIDIボードがありません
	beq	t_err_68
	bra	exit

bit_pat_set:			*チャンネルナンバーをビット列へ
	moveq.l	#0,d2
	moveq.l	#0,d3
	moveq.l	#0,d5
bps_lp:
	bsr	chk_num
	bmi	exit_bps
	bsr	asc_to_n
	subq.b	#1,d1
	cmpi.b	#tr_max-1,d1
	bhi	illegal_p
	cmpi.b	#31,d1
	bls	bset_d2
	cmpi.b	#63,d1
	bhi	bset_d5
	bset.l	d1,d3
	bra	bps_lp
bset_d5:
	bset.l	d1,d5
	bra	bps_lp
bset_d2:
	bset.l	d1,d2
	bra	bps_lp
exit_bps:
	rts

skip_sep:			*セパレータをスキップする(スペースやタブも)
	move.l	d0,-(sp)
skip_sep_lp:
	subq.l	#1,d4
	bmi	exit_skip_sep
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
exit_skip_sep:
	addq.l	#1,d4
	move.l	(sp)+,d0
	rts

skip_spc:			*スペースをスキップする
	subq.l	#1,d4
	bmi	exit_skip_spc
	cmpi.b	#' ',(a4)+
	beq	skip_spc
	cmpi.b	#09,-1(a4)	*skip tab
	beq	skip_spc
	subq.w	#1,a4
exit_skip_spc:
	addq.l	#1,d4
	rts

skip_spc2:			*スペースを読み飛ばす(case:command line)
	move.b	(a2)+,d0
	beq	print_hlp	*ヘルプ表示(スタックのつじつまが合わなくなるが大丈夫)
	cmpi.b	#' ',d0
	bls	skip_spc2
	subq.w	#1,a2
	rts

skip_plus:			*PLUSをスキップする
	subq.l	#1,d4
	bmi	exit_skip_plus
	cmpi.b	#'+',(a4)+
	beq	skip_plus
	subq.w	#1,a4
exit_skip_plus:
	addq.l	#1,d4
	rts

chk_num:			*数字かどうかチェック
	* > eq=number
	* > mi=not num
	cmpi.b	#'%',(a4)
	beq	yes_num
	cmpi.b	#'$',(a4)
	beq	yes_num
	cmpi.b	#'0',(a4)
	bcs	not_num
	cmpi.b	#'9',(a4)
	bhi	not_num
yes_num:
	move.w	#%0000_0100,ccr	*eq
	rts
not_num:
	move.w	#%0000_1000,ccr	*mi
	rts

chk_num2:			*数字かどうかチェック(for command line)
	* > eq=number
	* > mi=not num
	cmpi.b	#'0',(a2)
	bcs	not_num
	cmpi.b	#'9',(a2)
	bhi	not_num
	move.w	#%0000_0100,ccr	*eq
	rts

asc_to_n:			*数字文字列を数値へ
	* < (a4)=number strings
	* < d4=length counter
	* > d1.l=value
	* > a4=next,d4=dec
	* x none
	bsr	skip_sep	*','などをskip
	tst.l	d4
	beq	num_ret
	movem.l	d2-d3,-(sp)
	cmpi.b	#'-',(a4)
	seq	d2   		*'-'ならマーク
	bne	get_num0
	subq.l	#1,d4		*skip '-'
	addq.w	#1,a4
get_num0:
	bsr	skip_plus
	bsr	skip_spc

	cmpi.b	#'$',(a4)
	beq	get_hexnum_
	cmpi.b	#'%',(a4)
	beq	get_binnum_

	moveq.l	#0,d1
	moveq.l	#0,d0
num_lp01:
	subq.l	#1,d4
	bmi	num_exitt
	move.b	(a4)+,d0
	cmpi.b	#'_',d0
	beq	num_lp01
	sub.b	#$30,d0
	bmi	num_exit
	cmp.b	#9,d0
	bhi	num_exit

	add.l	d1,d1
	move.l	d1,d3
	lsl.l	#2,d1
	add.l	d3,d1		*d1=d1*10
	add.l	d0,d1		*d1=d1+d0
	tst.l	d4
	beq	num_exit
	bra	num_lp01
num_exit:
	subq.w	#1,a4
num_exitt:
	addq.l	#1,d4
	tst.b	d2
	beq	@f
	neg.l	d1
@@:
	bsr	skip_sep
	movem.l	(sp)+,d2-d3
num_ret:
	rts
get_hexnum_:			*16進数
	moveq.l	#0,d0
	moveq.l	#0,d1
	subq.l	#1,d4
	bmi	num_exitt
	addq.w	#1,a4
	bsr	skip_spc
__num_lp01_:
	subq.l	#1,d4
	bmi	num_exitt
	move.b	(a4)+,d0
	cmpi.b	#'_',d0
	beq	__num_lp01_
	bsr	mk_capital
	sub.b	#$30,d0
	bmi	num_exit
	cmp.b	#9,d0
	bls	calc_hex_
	cmpi.b	#17,d0
	bcs	num_exit
	cmpi.b	#22,d0
	bhi	num_exit
	subq.b	#7,d0
calc_hex_:
	lsl.l	#4,d1
	or.b	d0,d1
	tst.l	d4
	beq	num_exit
	bra	__num_lp01_
get_binnum_:			*2進数
	moveq.l	#0,d0
	moveq.l	#0,d1
	subq.l	#1,d4
	bmi	num_exitt
	addq.w	#1,a4
	bsr	skip_spc
b__num_lp01_:
	subq.l	#1,d4
	bmi	num_exitt
	move.b	(a4)+,d0
	cmpi.b	#'_',d0
	beq	b__num_lp01_
	cmpi.b	#'0',d0
	beq	calc_b_num__
	cmpi.b	#'1',d0
	bne	num_exit
calc_b_num__:
	sub.b	#$30,d0
	add.l	d1,d1
	or.b	d0,d1
	tst.l	d4
	beq	num_exit
	bra	b__num_lp01_

mk_capital:			*小文字→大文字(英字以外の場合はそのままthrough out)
	* < d0.b=letter chr
	cmpi.b	#'a',d0
	bcs	exit_mkcptl
	cmpi.b	#'z',d0
	bhi	exit_mkcptl
	andi.w	#$df,d0		*わざと.w
exit_mkcptl:
	rts

fopen2:				*LZZ.Rの検索
	* > d5=file handle (error:d5<0)
	* - all 
	movem.l	d0-d2/a0-a3,-(sp)

	lea	lzz(pc),a0
	lea	open_fn(pc),a1
	move.b	(a0)+,(a1)+
	move.b	(a0)+,(a1)+
	move.b	(a0)+,(a1)+
	move.b	(a0)+,(a1)+
	move.b	(a0)+,(a1)+
	move.b	(a0)+,(a1)+
	subq.w	#6,a0

	clr.w	-(sp)
	pea     (a0)
	DOS	_OPEN
	addq.w	#6,sp
	move.l	d0,d5		*d5.w=file handle
	bpl	exit_fopen	*no problem

	pea	path(pc)
	bsr	search_env
	addq.w	#4,sp
	tst.l	d0
	bmi	exit_fopen
	move.l	d0,a1
	bra	fo0

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
fo0:
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
	beq	fsize0		*file size=0
	bmi	fsize0		*file size=minus

	move.l	d0,-(sp)
	DOS	_MALLOC
	addq.w	#4,sp
	tst.l	d0
	bmi	out_mem		*OUT OF MEMORY
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
	bmi	read_err	*読み込み失敗

	move.w	d5,-(sp)	*close
	DOS	_CLOSE
	addq.l	#2,sp
	rts

key_bind_jk:
	movem.l	d0-d2/d4/a0/a4,-(sp)

	pea	zp_key(pc)
	bsr	search_env
	addq.w	#4,sp
	tst.l	d0
	bmi	quit_zk_jk

	moveq.l	#$7f,d4
	move.l	d0,a4
	lea	key_tbl_jk(pc),a0
	moveq.l	#14-1,d2
@@:
	bsr	asc_to_n
	ror.b	#4,d1
	move.b	d1,(a0)+
	dbra	d2,@b
quit_zk_jk:
	movem.l	(sp)+,d0-d2/d4/a0/a4
	rts

key_bind_db:
	movem.l	d0-d2/d4/a0/a4,-(sp)

	pea	zp_key(pc)
	bsr	search_env
	addq.w	#4,sp
	tst.l	d0
	bmi	quit_zk_db

	moveq.l	#$7f,d4
	move.l	d0,a4
	lea	key_tbl_db(pc),a0
	moveq.l	#14-1,d2
@@:
	bsr	asc_to_n
	ror.b	#4,d1
	move.b	d1,(a0)+
	dbra	d2,@b
quit_zk_db:
	movem.l	(sp)+,d0-d2/d4/a0/a4
	rts

chk_drv:
	* > eq=kept
	* > mi=error
	move.l	$8c.w,a0
	subq.w	#8,a0
	cmpi.l	#'ZmuS',(a0)+
	bne	chk_drv_err
	cmpi.w	#'iC',(a0)+
	bne	chk_drv_err
	cmpi.b	#$2f,(a0)+	*version check
	bhi	t_err_66
	Z_MUSIC	#$3a		*a0=play_trk_tbl,d0=real_ch_tbl
	move.l	d0,real_ch_tbl-work(a6)
	Z_MUSIC	#$50
	move.l	a0,info_tbl-work(a6)
	moveq.l	#0,d0
	move.l	d0,-(a0)	*エラークリア
	move.l	d0,-(a0)	*エラークリア
	rts
chk_drv_err:
	moveq.l	#-1,d0
	rts

print_title:			*タイトル表示
	movem.l	d0-d7/a0-a6,-(sp)
	tst.b	non_disp-work(a6)
	bne	aa6
	move.w	#2,-(sp)
	pea	title_mes(pc)
	DOS	_FPUTS
	addq.w	#6,sp
aa6:
	movem.l	(sp)+,d0-d7/a0-a6
	rts

exit2:				*エラーで終了
	tst.b	non_disp-work(a6)
	bne	aa7
	move.w	#2,-(sp)
	pea	(a1)		*エラーメッセージ表示
	DOS	_FPUTS
	addq.w	#6,sp
aa7:
	move.l	ssp(pc),a1	*ユーザーモードへ
	IOCS	_B_SUPER

	move.w	#-1,-(sp)
	DOS	_EXIT2

other_prog:			*他のプログラムが垂直帰線期間割り込みを使用中
	lea	other_mes(pc),a1
	bra	exit2

print_hlp:			*簡易ヘルプ表示
	lea	hlp_mes(pc),a1
	bra	exit2

no_drv:				*ドライバが組み込まれていません
	lea	no_drv_mes(pc),a1
	bra	exit2

lzz_not_found:
	lea	lzz_nf_mes(pc),a1
	bra	exit2

cant_use_lzz:
	lea	cant_use_lzz_mes(pc),a1
	bra	exit2

lzz_err:
	lea	lzz_err_mes(pc),a1
	bra	exit2

illegal_p:
	lea	illegal_p_mes(pc),a1
	bra	exit2

fsize0:				*ファイルサイズがゼロ
	move.l	a2,a1		*filename
	bsr	print_fnm2
	lea	fsize0_mes(pc),a1
	bra	exit2

unid_error:			*未確認ファイル
	bsr	print_fnm
	lea	unid_mes(pc),a1
	bra	exit2

unid_error_zdf:			*未確認ファイル(ZDF)
	bsr	print_fnm
	lea	unidzdf_mes(pc),a1
	bra	exit2

juke_error:			*既定外コマンドの使用
	bsr	print_fnm
	lea	juke_er_mes(pc),a1
	bra	exit2

err_remain:			*エラーが残っている。
	bsr	print_fnm
err_remain2:			*エラーが残っている。
	lea	remain_er_mes(pc),a1
	bra	exit2

not_play_data:			*演奏データではない
	bsr	print_fnm
	lea	not_pld_mes(pc),a1
	bra	exit2

zpd_big:			*ADPCMバッファが小さい
	bsr	print_fnm
	lea	adpcm_small_mes(pc),a1
	bra	exit2

zmd_big:			*Trackバッファが小さい
	bsr	print_fnm
t_err_5:
	lea	trk_small_mes(pc),a1
	bra	exit2

out_mem:			*メモリ不足
	lea	out_mem_mes(pc),a1
	bra	exit2

read_err:			*読み込み失敗
	lea	read_err_mes(pc),a1
	bra	exit2

file_not_found2:
	lea	org_fn(pc),a2
file_not_found:			*ファイルが無い
	move.l	a2,a1		*filename
	bsr	print_fnm2
	lea	fnf_err_mes(pc),a1
	bra	exit2

too_many:			*データ多すぎ
	lea	too_many_mes(pc),a1
	bra	exit2

not_kep:			*JUKE BOXは常駐していない
	lea	not_kep_mes(pc),a1
	bra	exit2

jb_released:			*JUKE BOXを解除しました
	tst.b	non_disp-work(a6)
	bne	@f
	move.w	#2,-(sp)
	pea	release_mes(pc)
	DOS	_FPUTS
	addq.w	#6,sp
@@:
	bsr	print_err_code

	move.l	ssp(pc),a1	*ユーザーモードへ
	IOCS	_B_SUPER

	DOS	_EXIT

jb_already:			*JUKE BOXは既に常駐しています
	lea	already_mes(pc),a1
	bra	exit2

midi_err:			*MIDIデータ作成失敗
	lea	midi_err_mes(pc),a1
	bra	exit2

write_err:			*書き出し失敗
	DOS	_ALLCLOSE

	pea	filename(pc)
	DOS	_DELETE
	addq.w	#4,sp

	lea	write_err_mes(pc),a1
	bra	exit2

err_buf_size:	equ	8
print_err_code:
	tst.b	non_disp-work(a6)
	bne	exit_pec	*表示なし
	move.l	info_tbl(pc),a0
	tst.l	-(a0)
	bne	@f
	tst.l	-(a0)
	beq	exit_pec	*エラーなし
@@:
	moveq.l	#0,d0
	moveq.l	#err_buf_size-1,d1
pec_lp00:
	move.b	(a0)+,d0
	beq	next_pec
	bsr	num_to_str
	move.w	#2,-(sp)
	pea	error??_mes(pc)
	DOS	_FPUTS
	addq.w	#6,sp

	move.w	#2,-(sp)
	pea	suji+7(pc)
	DOS	_FPUTS
	addq.w	#6,sp

	move.w	#2,-(sp)
	pea	error??_mes2(pc)
	DOS	_FPUTS
	addq.w	#6,sp
next_pec:
	dbra	d1,pec_lp00
exit_pec:
	rts

print_fnm:
	lea	filename(pc),a1
print_fnm2:
	tst.b	non_disp-work(a6)
	bne	aa8
	move.w	#2,-(sp)
	pea	(a1)
	DOS	_FPUTS
	addq.w	#6,sp

	move.w	#2,-(sp)
	pea	chon(pc)
	DOS	_FPUTS
	addq.w	#6,sp
aa8:
	rts

t_err_66:		*バージョン不一致
	lea	version_mes(pc),a1
	bra	exit2
t_err_68:		*MIDIボードがありません
	lea	midi_bd_mes(pc),a1
	bra	exit2

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

num_to_str:	*レジスタの値を文字数列にする
	* < d0.l=value
	* > (suji)=ascii data
	* - all
	movem.l	d1-d4/a0-a1,-(sp)
	clr.b	d4
	lea	suji(pc),a0
	lea	exp_tbl(pc),a1
	moveq.l	#10-1,d1
ex_loop0:
	moveq.l	#0,d2
	move.l	(a1)+,d3
ex_loop1:
	sub.l	d3,d0
	bcs	xbcd_str
	addq.b	#1,d2
	bra	ex_loop1
xbcd_str:
	add.l	d3,d0
	move.b	d2,d3
	or.b	d4,d3
	bne	nml_ktset
	move.b	#$20,(a0)+
	bra	nml_lp_ope
nml_ktset:
	st	d4
	add.b	#'0',d2
	move.b	d2,(a0)+
nml_lp_ope:
	dbra	d1,ex_loop0
	tst.b	d4
	bne	set_suji_end
	move.b	#'0',-1(a0)
set_suji_end:
	clr.b	(a0)		*end flg
	movem.l	(sp)+,d1-d4/a0-a1
	rts

	.data
title_mes:
	dc.b	$1b,'[36mΖ',$1b,'[35mp.R '
	dc.b	$1b,'[37m',$F3,'V',$F3,'E',$F3,'R',$F3,'S',$F3,'I',$F3,'O',$F3,'N'
	dc.b	$f3,' ',$f3,'2',$f3,'.',$f3,'0',$f3,'2'	*,$f3,'A'
	dc.b	$1b,'[m (C) 1991,1992,1993,1994 '
	dc.b	$1b,'[36mZENJI SOFT',$1b,'[m',13,10,0
hlp_mes:
	dc.b	$1b,'[37m< USAGE > '
	dc.b	$1b,'[m ZP.R [COMMAND] [FILENAME1][,SETUP-FILE1…4]',13,10
	dc.b	$1b,'[37m< COMMAND SWITCHES >',13,10,$1b,'[m'
	dc.b	$1b,'[m'
	dc.b	'-A                               Prepare ZMUSIC.X to MIDI data recording.',13,10
	dc.b	'-A<filename>                     Save MIDI dump data.',13,10
	dc.b	'-C[track number(s),…]           Continue playing',13,10
	dc.b	'-D                               Debug tool',13,10
	dc.b	'-E[channel number(s),…]         To enable channel(s).',13,10
	dc.b	'-F[speed]                        Fadeout',13,10
	dc.b	'-I                               Initialize ZMUSIC.X and all instruments.',13,10
	dc.b	'-J<filename>[,setup1…4]         Juke box',13,10
	dc.b	'-M[channel number(s),…]         To disable channel(s).',13,10
	dc.b	'-O[level[,channel number(s),…]] To set Output-level.',13,10
	dc.b	'-P[filename[,setup1…4]]         Start playing (ZMD,ZMS,OPM,ZDF)',13,10
	dc.b	'-P[track number(s),…]           Start playing',13,10
	dc.b	'-Q[filename]                     Calculate the total step time.',13,10
	dc.b	'-R                               Release the juke-box or the debug tool.',13,10
	dc.b	'-S[track number(s),…]           Stop playing',13,10
	dc.b	'-W[filename[,setup1…4]]         Start playing as soon as MIDI IN receives $FA.',13,10
	dc.b	'-X<filename>                     Send MIDI dump data to the MIDI instrument.',13,10
	dc.b	0
other_mes:
	dc.b	'V-DISP interrupt has already used by other applications.',13,10,0
no_drv_mes:
	dc.b	$1b,'[47mZMUSIC.X is not included.',$1b,'[m',13,10,0
fsize0_mes:
	dc.b	$1b,'[47mIllegal file size.',$1b,'[m',13,10,0
illegal_p_mes:
	dc.b	$1b,'[47mIllegal parameter.',$1b,'[m',13,10,0
out_mem_mes:
	dc.b	$1b,'[47mOut of memory.',$1b,'[m',13,10,0
read_err_mes:
	dc.b	$1b,'[47mRead error.',$1b,'[m',13,10,0
fnf_err_mes:
	dc.b	$1b,'[47mFile not found.',$1b,'[m',13,10,0
too_many_mes:
	dc.b	$1b,'[47mToo many filenames are written in index-file.',$1b,'[m',13,10,0
unidzdf_mes:
unid_mes:
	dc.b	$1b,'[47mUnidentified file.',$1b,'[m',13,10,0
juke_er_mes:
	dc.b	$1b,'[47mIllegal command has existed.',$1b,'[m',13,10,0
adpcm_small_mes:
	dc.b	$1b,'[47mADPCM buffer is too small.',$1b,'[m',13,10,0
trk_small_mes:
	dc.b	$1b,'[47mTrack buffer is too small.',$1b,'[m',13,10,0
version_mes:
	dc.b	$1b,'[47mVersion number mismatch.',$1b,'[m',13,10,0
midi_bd_mes:
	dc.b	$1b,'[47mMIDI is Unable to use.',$1b,'[m',13,10,0
remain_er_mes:
	dc.b	$1b,'[47mAn error is still remained in ZMD/ZMS.',$1b,'[m',13,10,0
not_pld_mes:
	dc.b	$1b,'[47mThis file is not ZMD or ZMS.',$1b,'[m',13,10,0
error??_mes:
	dc.b	$1b,'[47mERROR',0
error??_mes2:
	dc.b	$1b,'[m',13,10,0

juke_mes:
	dc.b	$1b,'[46m♪Juke-box has just started.',$1b,'[m',13,10,0
debug_mes:
	dc.b	$1b,'[46m♪Debug tool has included.',$1b,'[m',13,10,0
not_kep_mes:
	dc.b	$1b,'[47mZP.R is not kept in your system.',$1b,'[m',13,10,0
release_mes:
	dc.b	'ZP.R has released from your system.',13,10,0
already_mes:
	dc.b	$1b,'[47mZP.R has already kept.',$1b,'[m',13,10,0
midi_err_mes:
	dc.b	$1b,'[47mFail in making of string MIDI data.',$1b,'[m',13,10,0
lzz_nf_mes:
	dc.b	$1b,"[47mLZZ.R couldn't be found.",$1b,'[m',13,10,0
cant_use_lzz_mes:		*LZZがなかったから
	dc.b	$1b,"[47mLZZ.R couldn't be used.",$1b,'[m',13,10,0
lzz_err_mes:
	dc.b	$1b,"[47mError in LZZ.R.",$1b,'[m',13,10,0
write_err_mes:
	dc.b	$1b,'[47mWrite error.',$1b,'[m',13,10,0
rec_st_mes:
	dc.b	$1b,'[46m♪Recording mode on ',$1b,'[m',13,10,0
saving_mes:
	dc.b	$1b,'[46m♪Now processing and writing.',$1b,'[m',13,10,0
trns_mes:
	dc.b	$1b,'[46m♪Now transmitting.',$1b,'[m',13,10,0
Q_mes:
*	dc.b	'				   TOTAL STEP TIMES',13,10
	dc.b	'   External Internal  '
	dc.b	'   External Internal  '
	dc.b	'   External Internal  '
	dc.b	'   External Internal  '
	dc.b	13,10,0
non_disp:	dc.b	0
chon:		dc.b	' … ',0
dev_name:	dc.b	'OPM     '
dev_name2:	dc.b	'MIDI    '
NUL:		dc.b	'NUL     '
getname:	dc.b	'zmusic',0,0
path:		dc.b	'path',0
lzz:		dc.b	'lzz.r',0
zp_key:		dc.b	'zp_keyctrl',0
zp_juk:		dc.b	'zp_jukectrl',0
sep_:	dc.b	':',0
SPC2:		dc.b	'  ',0
CRLF:		dc.b	13,10,0
ZMD_kaku:	dc.b	'ZMD',0
ZMS_kaku:	dc.b	'ZMS',0
OPM_kaku:	dc.b	'OPM',0
ZDF_kaku:	dc.b	'ZDF',0
BMD_kaku:	dc.b	'BMD',0
MDD_kaku:	dc.b	'MDD',0
ZPD_kaku:	dc.b	'ZPD',0
JUK_kaku:	dc.b	'JUK',0

	.even
exp_tbl:
	dc.l	1000000000
	dc.l	100000000
	dc.l	10000000
	dc.l	1000000
	dc.l	100000
	dc.l	10000
	dc.l	1000
	dc.l	100
	dc.l	10
	dc.l	1

	.bss
zpdnm_buf:	ds.b	(fn_size+3)*max_p+1
mddnm_buf:	ds.b	(fn_size+7)*max_p+1
open_fn:	ds.b	fn_size
zpdfilename:
filename:	ds.b	fn_size
mddfilename:	ds.b	fn_size
setup_fn:	ds.b	fn_size*4
prog_name:	ds.b	fn_size
org_fn:		ds.b	fn_size
suji:		ds.b	11
retry:		ds.b	1
	.even
zpd_scan:	ds.b	1
mdd_scan:	ds.b	1
	.even
info_tbl:	ds.l	1
env_bak:	ds.l	1
seq_wk_tbl:	ds.l	1
zmd_addr:	ds.l	1
hozon_a5:	ds.l	1
last_zpd_addr:	ds.l	1
lzz_adr:	ds.l	1
bufadr:		ds.b	5*10+2
pname:		ds.b	26
a0work:		ds.l	1
a2work:		ds.l	1
list_adr:	ds.l	1
*buffer_adr:	ds.l	1
ssp:		ds.l	1
sfp:		ds.l	1
		ds.l	2048		*スタックエリア
my_sp:
end_of_prog:
	.end
