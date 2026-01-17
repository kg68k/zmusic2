*-----------------------------------------------
*	      X68k ADPCM TOTAL TOOL
*
*		    ＺＶＴ．Ｘ
*
*	    PROGRAMMED BY Z.NISHIKAWA
*
*-----------------------------------------------

*参考文献一覧
*		X68000ﾊﾟﾜｰｱｯﾌﾟﾌﾟﾛｸﾞﾗﾐﾝｸﾞ	ASCII		ADPCM→PCM変換法
*		nnPCMDRV.SYSのDOCUMENT		Mr.GORRY	DMA/ADPCMの操作法
*		iocsの解析			T.ARIKUNI	同上
*		Oh!X 1989,11			SOFTBANK	同上
*		68000ﾌﾟﾛｸﾞﾗﾏｰｽﾞﾊﾝﾄﾞﾌﾞｯｸ		技評		演算処理関係
*		68000ﾌﾟﾛｸﾞﾗﾐﾝｸﾞ入門		ASCII		同上

	.include	iocscall.mac
	.include	doscall.mac
	.include	dma.mac
	.list
	.text
	.even

max:		equ	4096
max_b:		equ	12	*maxが２の何乗か
scr_wx:		equ	766	*x width
scr_wy:		equ	85	*y width ±
start_x:	equ	1
plus_:		equ	224
genten_a:	equ	scr_wy
genten_b:	equ	scr_wy+plus_
stack:		equ	$4000	*stackの大きさ
RTS:		equ	$4e75	*rtsの機械語コード
NOP:		equ	$4e71	*NOPの命令コード

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

display	macro	mes,atr,x,y,ln	*メッセージ表示のマクロ
	lea	mes(pc),a1
	moveq.l	#atr,d1
	or.b	d7,d1
	moveq.l	#x,d2
	moveq.l	#y,d3
	moveq.l	#ln-1,d4
	IOCS	_B_PUTMES
	endm

display2	macro	mes,atr,x,y,ln	*メッセージ表示のマクロ
	lea	mes(pc),a1
	moveq.l	#atr,d1
	moveq.l	#x,d2
	moveq.l	#y,d3
	moveq.l	#ln-1,d4
	IOCS	_B_PUTMES
	endm

display2_	macro	mes,atr,x,y,ln	*メッセージ表示のマクロ
	lea	mes,a1
	moveq.l	#atr,d1
	moveq.l	#x,d2
	moveq.l	#y,d3
	moveq.l	#ln-1,d4
	IOCS	_B_PUTMES
	endm

display3	macro	mes,atr,x,y	*メッセージ表示のマクロ
	moveq.l	#atr,d1
	or.b	d7,d1
	IOCS	_B_COLOR
	move.l	d0,-(sp)
	moveq.l	#x,d1
	moveq.l	#y,d2
	IOCS	_B_LOCATE
	lea	mes(pc),a1
	IOCS	_B_PRINT
	move.l	(sp)+,d1
	IOCS	_B_COLOR
	endm

	*プログラムスタート

	lea	$10(a0),a0	*メモリブロックの変更
	suba.l	a0,a1
	pea	(a1)
	pea	(a0)
	DOS	_SETBLOCK
	addq.w	#8,sp

	suba.l	a1,a1
	IOCS	_B_SUPER
	move.l	d0,ssp

	move.l	sp,org_sp
	lea	user_sp,sp

	tst.b	(a2)+
	beq	editor_start	*何もスイッチがないなら…

	lea	title(pc),a1	*タイトル表示
	bsr	ppp

	lea	pitch_cmd(pc),a5
	lea	volume_cmd(pc),a6
	clr.b	fn1-pitch_cmd(a5)		*init fn1 to fn3
	clr.b	fn2-pitch_cmd(a5)
	clr.b	fn3-pitch_cmd(a5)
	clr.w	(a5)				*init two of them.
	clr.l	(a6)				*init two of them.
	clr.b	wild_mode-pitch_cmd(a5)
chk_opt_lp:
	tst.b	(a2)
	beq	prepare
	bsr	get_name

	lea	input_buffer,a0
	cmpi.b	#'-',(a0)
	beq	option_chk
	cmpi.b	#'/',(a0)
	beq	option_chk
	bsr	sv_fn		*get file name
	bne	print_hlp
	bra	chk_opt_lp

option_chk:
	addq.w	#1,a0
	move.b	(a0)+,d0
	move.b	d0,d1
	bsr	mk_capital1
	cmpi.b	#'4',d0		*4-pattren save
	beq	get_param_4
	cmpi.b	#'?',d0
	beq	print_hlp
	cmpi.b	#'A',d1		*convert to adpcm
	beq	ptoa_com_set
	cmpi.b	#'C',d1		*convert to pcm
	beq	atop_com_set
	cmpi.b	#'G',d1
	beq	ok_ed_go
	cmpi.b	#'H',d1
	beq	hakei_hyoji
	cmpi.b	#'I',d1		*insert
	beq	ins_com_set
	cmpi.b	#'L',d1
	beq	read_go
	cmpi.b	#'M',d1		*mix
	beq	mix_com_set
	cmpi.b	#'P',d1		*pitch change
	beq	get_param_p
	cmpi.b	#'V',d1		*volume change
	beq	get_param_v
	cmpi.b	#'T',d1		*automatic truncate
	beq	get_param_t

	bra	chk_opt_lp

hakei_hyoji:
	not.b	hk_mode
	bra	chk_opt_lp

mix_com_set:
	move.l	a0,-(sp)
	lea	mix_cmd(pc),a0
	st	(a0)
	clr.b	mix_mode-mix_cmd(a0)
	clr.b	_4_cmd-mix_cmd(a0)
	clr.b	conv_ptoa_cmd-mix_cmd(a0)
	clr.b	conv_atop_cmd-mix_cmd(a0)
	move.l	(sp)+,a0
	bra	get_param_m

ins_com_set:
	move.l	a0,-(sp)
	lea	mix_cmd(pc),a0
	st	(a0)
	st	mix_mode-mix_cmd(a0)
	clr.b	_4_cmd-mix_cmd(a0)
	clr.b	conv_ptoa_cmd-mix_cmd(a0)
	clr.b	conv_atop_cmd-mix_cmd(a0)
	move.l	(sp)+,a0
	bra	get_param_m

atop_com_set:			*adpcm to pcm
	cmpi.b	#'8',(a0)
	seq.b	_8bit_pcm	*8 or 16 ?
	bne	@f
	addq.w	#1,a0		*skip 8
@@:
	move.l	a0,-(sp)
	lea	conv_atop_cmd(pc),a0
	st	(a0)
	clr.b	_4_cmd-conv_atop_cmd(a0)
	clr.b	conv_ptoa_cmd-conv_atop_cmd(a0)
	move.l	(sp)+,a0
	bra	chk_opt_lp

ptoa_com_set:
	move.l	a0,-(sp)
	lea	conv_ptoa_cmd(pc),a0
	st	(a0)
	clr.b	_4_cmd-conv_ptoa_cmd(a0)
	clr.b	conv_atop_cmd-conv_ptoa_cmd(a0)
	move.l	(sp)+,a0
	bra	chk_opt_lp

get_param_t:			*隠れコマンド
	cmpi.b	#' ',(a0)
	bls	chk_opt_lp
	exg	a0,a1
	bsr	get_num
	tst.l	d1
	bmi	fn_error
	cmpi.l	#$ffff,d1
	bhi	fn_error
	move.w	d1,truncate_cmd
	st.b	truncate_cmd+3
	exg	a0,a1
	bra	chk_opt_lp

get_param_p:
	exg	a0,a1
	bsr	get_num
	move.b	d1,d2
	bpl	chk_hani
	neg.b	d1
chk_hani:
	cmpi.b	#12,d1
	bhi	fn_error	*絶対値が12以上ならエラー
	tst.b	(a5)
	bne	sv_p_p2
	move.b	d2,(a5)
	bra	exg_p
sv_p_p2:
	tst.b	1(a5)
	bne	fn_error	*スイッチが多すぎ
	move.b	d2,1(a5)
exg_p:
	exg	a0,a1
	bra	chk_opt_lp
get_param_v:
	exg	a0,a1
	bsr	get_num
	tst.w	d1
	beq	fn_error	*0ならエラー
	bmi	fn_error
	cmpi.w	#400,d1
	bhi	fn_error	*400以上ならエラー
	tst.w	(a6)
	bne	sv_p_v2
	move.w	d1,(a6)
	bra	exg_v
sv_p_v2:
	tst.w	2(a6)
	bne	fn_error
	move.w	d1,2(a6)
exg_v:
	exg	a0,a1
	bra	chk_opt_lp

get_param_m:
	cmpi.b	#' ',(a0)
	bls	chk_opt_lp
	exg	a0,a1
	bsr	get_num
	tst.l	d1
	bmi	fn_error
	move.l	d1,offset
	exg	a0,a1
	bra	chk_opt_lp

get_param_4:
	move.b	(a0)+,d0
	move.b	(a0),d1
	bsr	mk_capital0
	cmpi.b	#'P',d0
	beq	set_4_p
	cmpi.b	#'V',d0
	beq	set_4_v
	bra	fn_error
set_4_p:
	move.b	#'P',_4_cmd
	bra	set_dir
set_4_v:
	move.b	#'V',_4_cmd
set_dir:
	st.b	dirofchg
	move.b	(a0)+,d0
	cmpi.b	#' ',d0
	bls	chk_opt_lp
	cmpi.b	#'+',d0
	beq	chk_opt_lp
	clr.b	dirofchg
	cmpi.b	#'-',d0
	beq	chk_opt_lp
	bra	fn_error

prepare:
	move.b	wild_mode(pc),d0
	beq	go_command	*ワイルドカードでない

	tst.b	mix_cmd
	bne	prp_mix_wild
				*ミックス以外でワイルドカードをチェック
	btst.l	#0,d0
	bne	do_wild_prp
	btst.l	#1,d0
	beq	do_wild_prp
	bra	fn_error
prp_mix_wild:			*ミックスでワイルドカードチェック
	btst.l	#0,d0
	beq	do_wild_prp
	btst.l	#1,d0
	beq	do_wild_prp
	bra	fn_error
do_wild_prp:
	clr.b	files_hjm
	move.w	mission_complete(pc),patch1
	move.w	#RTS,mission_complete

	bsr	make_true_fn
wild_com_lp:
	bsr	mfree_adpcm_a
	bsr	mfree_adpcm_b
	bsr	mfree_pcm_a
	bsr	mfree_pcm_b

	bsr	get_true_fn
	bmi	wild_end
	bsr	get_dest_fn

*	pea	fn1(pc)		*for debug
*	DOS	_PRINT
*	addq.w	#4,sp

	bsr	go_command
	bra	wild_com_lp
wild_end:
	move.w	patch1,mission_complete
	not.b	files_hjm
	bmi	mission_complete
	bra	fn_error

get_dest_fn:			*書き込み側のファイルネーム
	tst.b	mix_cmd
	bmi	case_mxc
	btst.b	#1,wild_mode(pc)
	beq	exit_gdf
	lea	fn1(pc),a0	*読み込み側の名前
	lea	wfn2(pc),a1	*ワイルドカード入り
	lea	fn2(pc),a2	*ワイルドカードを考慮した名前
	bra	do_gdf
case_mxc:
	btst.b	#2,wild_mode(pc)
	beq	exit_gdf
	lea	fn1(pc),a0
	lea	wfn3(pc),a1
	lea	fn3(pc),a2
do_gdf:
	cmpi.b	#':',1(a1)
	bne	do_gdf1
	move.b	(a1)+,(a2)+
	move.b	(a1)+,(a2)+
do_gdf1:
	move.l	a0,work1
gdf_lp:
	move.b	(a1)+,d0
	beq	exit_gdf
	cmpi.b	#'.',d0
	beq	mark_period
	cmpi.b	#'*',d0
	beq	copy_fn1
	cmpi.b	#'?',d0
	bne	gdf1
	move.b	(a0),d0
	cmpi.b	#'.',d0
	beq	add_aregs2
gdf1:
	move.b	d0,(a2)
add_aregs:
	addq.w	#1,a0
add_aregs2:
	addq.w	#1,a2
	bra	gdf_lp
exit_gdf:
	clr.b	(a2)
	rts
mark_period:
	bsr	srch_period
	bra	gdf1
copy_fn1:
	tst.b	(a0)
	beq	gdf_lp
copy_fn1_lp:
	move.b	(a0)+,d0
	move.b	d0,(a2)
	beq	gdf_lp
	cmpi.b	#'.',d0
	beq	gdf_lp
	addq.w	#1,a2
	bra	copy_fn1_lp

srch_period:
	move.l	work1,a0
srch_period_lp:
	move.b	(a0),d1
	tst.b	d1
	beq	exit_sp
	cmpi.b	#'.',d1
	beq	exit_sp
	addq.w	#1,a0
	bra	srch_period_lp
exit_sp:
	rts
make_true_fn:			*ちゃんとしたファイルネームを得る
	btst.b	#0,wild_mode(pc)
	beq	chk_wfn2
	move.l	fnbfsz(pc),-(sp)
	DOS	_MALLOC
	addq.w	#4,sp
	move.l	d0,mm_error
	bsr	print_em2
	bmi	bye_bye
	move.l	d0,fn_buffer1
	move.l	d0,a0
	move.l	d0,fn_point1
	lea	wfn1(pc),a2
	bsr	do_make_true_fn
	bmi	fn_error	*そんなファイルネームはない
chk_wfn2:
	tst.b	mix_cmd
	bpl	exit_mtf
	btst.b	#1,wild_mode(pc)
	beq	exit_mtf
	lea	fn2(pc),a0
	lea	wfn2(pc),a2
	bsr	do_make_true_fn
	bmi	fn_error	*そんなファイルネームはない
exit_mtf:
	rts

do_make_true_fn:
	* < a0=destination
	* < a2=wild card name

	move.l	a0,work1
	clr.b	files_hjm

	bsr	get_drive	*getドライブ名

	move.w	#%100000,-(sp)
	pea	(a2)		*wild card
	pea	filbuf(pc)
	DOS	_FILES
	lea	10(sp),sp
	tst.l	d0
	bmi	exit_dmtf
	st	files_hjm
	bra	_nm_trns
_nfiles:
	pea	filbuf(pc)
	DOS	_NFILES
	addq.w	#4,sp
	tst.l	d0
	bmi	exit_dmtf
_nm_trns:
	lea	filbuf+30(pc),a1
@@:				*save file_name
	move.b	(a1)+,(a0)+
	bne	@b
	lea.l	wfn2(pc),a1
	cmpa.l	a1,a2		*ミックスのときのfn2は初めに発見したもの
	beq	exit_dmtf
	move.l	a0,d0
	sub.l	work1,d0
	move.l	fnbfsz(pc),d1
	sub.l	#100,d1		*余裕を持って
	cmp.l	d1,d0
	bls	_nfiles

	add.l	#$1000,fnbfsz-wfn2(a1)
	move.l	fnbfsz,-(sp)	*new size
	move.l	fn_buffer1,-(sp)	*address
	DOS	_SETBLOCK
	addq.w	#8,sp
	move.l	d0,mm_error
	bsr	print_em2
	bmi	bye_bye
	bra	_nfiles
exit_dmtf:
	st	(a0)		*set end_mark
	not.b	files_hjm
	rts

get_drive
	clr.b	drive_name
	move.b	1(a2),d0
	cmpi.b	#':',d0
	bne	exit_gd
	move.b	(a2),drive_name
	move.b	d0,drive_name+1
exit_gd:
	rts

get_true_fn:
	btst.b	#0,wild_mode(pc)
	beq	exit_gtf
	move.l	fn_point1,a0
	tst.b	(a0)
	bmi	exit_gtf
	lea	fn1(pc),a1
	bsr	trns_fnm
	move.l	a0,fn_point1
exit_gtf:
	rts
trns_fnm:
	move.b	(a0)+,(a1)+
	bne	trns_fnm
	rts

read_go:
	pea	(a0)
	bsr	zvt_init
	move.l	(sp)+,a0

	lea	bank_a_fn,a2
@@:				*skip dummy
	move.b	(a0)+,d0
	cmpi.b	#' ',d0
	bls	@b
	cmpi.b	#',',d0
	beq	rg_b?
	subq.w	#1,a0
@@:				*get bank a name
	move.b	(a0)+,d0
	cmpi.b	#' ',d0
	bls	rg_b?
	cmpi.b	#',',d0
	beq	rg_b?
	move.b	d0,(a2)+
	bra	@b
rg_b?:
	clr.b	(a2)		*set end code
	lea	bank_b_fn,a2
@@:				*skip dummy
	move.b	(a0)+,d0
	beq	rg_do
	cmpi.b	#' ',d0
	bls	@b
	subq.w	#1,a0
@@:				*get bank b name
	move.b	(a0)+,d0
	cmpi.b	#' ',d0
	bls	rg_do
	move.b	d0,(a2)+
	bra	@b
rg_do:
	clr.b	(a2)

	lea	bank_a_fn,a1
	tst.b	(a1)
	beq	@f
	bsr	open_check
	bsr	print_em
	bmi	all_end_	*case error
	clr.b	data_a
	bsr	mfree_adpcm_a
	bsr	mfree_pcm_a
	lea	bank_a_fn,a1
	bsr	read_data	*d1=size,d2=addr.
	bsr	print_em
	bmi	all_end_	*case error
	move.l	d1,size1
	beq	all_end_	*case error
	move.l	d2,address1	*読み込んだのはadpcm data
	bsr	new_bank_a
@@:
	lea	bank_b_fn,a1
	tst.b	(a1)
	beq	@f
	bsr	open_check
	bsr	print_em
	bmi	all_end_	*case error
	clr.b	data_b
	bsr	mfree_adpcm_b
	bsr	mfree_pcm_b
	lea	bank_b_fn,a1
	bsr	read_data	*d1=size,d2=addr.
	bsr	print_em
	bmi	all_end_	*case error
	move.l	d1,size2
	beq	all_end_	*case error
	move.l	d2,address2
	bsr	new_bank_b
@@:
	bra	oeg0

zvt_init:
	bsr	other_init
	IOCS	_OS_CUROF
	bsr	g_init
	bra	val_init

editor_start:			*エディター部のスタート
	bsr	use_gram??
	bne	gram_error
ok_ed_go:
	bsr	zvt_init
oeg0:
	bsr	zen_clr		*全画面クリア
	bsr	scr_init
	bsr	set_vect
	IOCS	_OS_CUROF
	move.w	#%000000000011111_0,$e82200+4*2	*text line palet(color code4)blue
	move.w	#%000001111100000_0,$e82200+8*2	*text line palet(color code8)red
	move.w	#13324,$e82200+12*2		*▲カーソルの色
	bsr	g_recover
	lea	ent_path(pc),a0
	DOS	_CURDRV
	move.w	d0,(a0)+
	move.b	#'\',(a0)+
	pea	(a0)
	clr.w	-(sp)
	DOS	_CURDIR
	addq.w	#6,sp
					*メインルーチン
num_of_comm:	equ	17
max_size:	equ	12582912	*12Mb
					*ワーク初期化
	clr.b	bank_sel
	moveq.l	#0,d6
	move.b	d6,last_point
	bsr	disp_point

m_lp:
	IOCS	_B_KEYINP
				*alphabet
	move.w	d0,d1
	bsr	mk_capital1
	cmpi.b	#'A',d1		*select bank a
	beq	bank_change_a
	cmpi.b	#'B',d1		*select bank b
	beq	bank_change_b
	cmpi.b	#'C',d1		*波形の切り出し
	beq	truncate
	cmpi.b	#'D',d1		*DISK
	beq	disk_mode
	cmpi.b	#'E',d1		*effect
	beq	effect
	cmpi.b	#'F',d1		*change rate(FRQ)
	beq	_chg_rate
	cmpi.b	#'I',d1		*波形の頭だし
	beq	quantize
	cmpi.b	#'J',d1		*周波数微調整
	beq	adjust_frq
	cmpi.b	#'K',d1		*PITCH CHANGE
	beq	pitch_chg
	cmpi.b	#'M',d1		*monitor switch
	beq	M_ON
	cmpi.b	#'N',d1		*ポルタメント
	beq	portament
	cmpi.b	#'O',d1		*pan pot
	beq	_OUT
	cmpi.b	#'P',d1		*再生
	beq	playing
	cmpi.b	#'Q',d1		*esc+q = all end
	beq	all_end
	cmpi.b	#'R',d1		*録音
	beq	record
	cmpi.b	#'S',d1		*input smp size
	beq	input_smsz
	cmpi.b	#'T',d1		*input smp time
	beq	input_smpt
	cmpi.b	#'V',d1		*LEVEL change
	beq	level_chg
	cmpi.b	#'X',d1		*BANK CHANGE
	beq	bank_change
	cmpi.b	#'Y',d1		*deconvolution
	beq	deconvolution_ope
	cmpi.b	#'Z',d1		*convolution
	beq	convolution_ope
	cmpi.b	#'@',d1		*trigger
	beq	_AUTO_

	cmpi.b	#$1b,d0		*ESC
	seq	ESC_ON
	cmpi.b	#$0d,d0		*return/enter
	beq	CR_key
	cmpi.b	#' ',d0		*SPC=return/enter
	beq	CR_key
	cmpi.b	#'.',d0		*point end
	beq	data_move?
	cmpi.b	#',',d0		*point middle
	beq	data_move?
	cmpi.b	#'+',d0		*MIX 2 pcm data
	beq	mixing
	cmpi.b	#'-',d0		*SUB 2 pcm data
	beq	logic_ope
	cmpi.b	#'/',d0		*insert data
	beq	insert
	cmpi.b	#'*',d0		*PCMデータ表示モード
	beq	ast_ope
	cmpi.b	#'=',d0		*data copy
	beq	copy_data
	cmpi.b	#'?',d0		*情報
	beq	information

	move.b	d0,d1
	sub.b	#$30,d1
	cmpi.b	#9,d1
	bhi	_ido
	tst.b	d1
	bpl	data_move?

*移動方向キー
_ido:
	move.w	d0,d1
	lsr.w	#8,d1

	cmpi.b	#2*8+0,d1	*tab -> data type change
	beq	_chg_type
	cmpi.b	#7*8+7,d1	*data clear
	beq	data_clr

	cmpi.b	#3,d6
	bhi	case_btm
*ue??
	cmpi.w	#7*8+4,d1
	bne	shita??
_ue?:
	tst.b	d6
	bne	set_u
*	moveq.l	#num_of_comm,d6
	bra	move_point
set_u:
	moveq.l	#-1,d0
	bra	move_point
shita??:
	cmpi.b	#7*8+6,d1
	bne	hidari??
_sht?:
	cmp.b	#num_of_comm,d6
	bne	nml_mv_
*	moveq.l	#0,d6
	moveq.l	#0,d0
	bra	move_point
nml_mv_:
	moveq.l	#1,d0
	bra	move_point
case_btm:
hidari??:
	cmpi.b	#7*8+3,d1
	beq	_ue?
*migi??:
	cmpi.b	#7*8+5,d1
	beq	_sht?

*ue??
	cmpi.w	#7*8+4,d1
	bne	shita_??
	cmpi.b	#9,d6
	bls	_ue?
	cmpi.b	#16,d6
	bcs	go_sr??
	moveq.l	#-8,d0
	bra	move_point
go_sr??:
	cmpi.b	#12,d6
	bls	nml_ue
	moveq.l	#0,d0
	moveq.l	#7,d6
	bra	move_point
nml_ue:
	moveq.l	#-6,d0
	bra	move_point
shita_??:
	cmpi.b	#7*8+6,d1
	bne	tsugi_k
	cmp.b	#9,d6
	bhi	_sht?
	cmpi.b	#8,d6
	bcs	nml_sht
	moveq.l	#8,d0
	bra	move_point
nml_sht:
	moveq.l	#6,d0
	bra	move_point

tsugi_k
	bra	m_lp

move_point:
	add.b	d0,d6
	bsr	del_point
	bsr	disp_point
	bra	m_lp
del_point:
	moveq.l	#0,d7
	move.b	last_point(pc),d0
	bra	get_go_dsp
disp_point:
	moveq.l	#%1000,d7
	move.b	d6,last_point
	move.b	d6,d0
get_go_dsp:
	ext.w	d0
	add.w	d0,d0
	move.w	jump_tbl(pc,d0.w),a0	*jump!
	jmp	jump_tbl(pc,a0.w)

jump_tbl:				*CURSOR OPERATION
	dc.w	_stpt1-jump_tbl,_edpt1-jump_tbl
	dc.w	_stpt2-jump_tbl,_edpt2-jump_tbl
	dc.w	_mon-jump_tbl,_dt_tp-jump_tbl
	dc.w	_smp_tm-jump_tbl,_smp_rt-jump_tbl
	dc.w	_rec-jump_tbl,_files-jump_tbl
	dc.w	_out_as-jump_tbl,_at_md-jump_tbl
	dc.w	_smp_sz-jump_tbl,_level-jump_tbl
	dc.w	_pitch-jump_tbl,_mix-jump_tbl
	dc.w	_play-jump_tbl,_quit-jump_tbl

CR_key:
	bsr	del_point

	cmpi.b	#4,d6
	beq	M_ON
	cmpi.b	#17,d6
	beq	all_end2
	cmpi.b	#10,d6
	beq	_OUT
	cmpi.b	#11,d6
	beq	_AUTO_
	cmpi.b	#7,d6
	beq	_chg_rate
	cmpi.b	#5,d6
	beq	_chg_type
	cmpi.b	#6,d6
	beq	input_smpt
	cmpi.b	#12,d6
	beq	input_smsz
	cmpi.b	#9,d6
	beq	disk_mode
	cmpi.b	#8,d6
	beq	record
	cmpi.b	#16,d6
	beq	playing
	cmpi.b	#13,d6
	beq	level_chg
	cmpi.b	#14,d6
	beq	pitch_chg
	cmpi.b	#15,d6
	beq	mixing

	bsr	disp_point
	bra	m_lp
complete:
	bsr	del_point
	bsr	disp_point
	bra	m_lp

information:			*情報をプリント
	movem.l	d0-d7/a0-a6,reg_buff2
	bsr	clr_bottom

	display2	bank_a,%1111,0,28,8
	display2	i_adpcmadr,%0011,0,29,20
	lea	suji(pc),a1
	move.l	address1(pc),d0
	bsr	get_hex32
	display2	suji,%0011,19,29,8

	display2	i_pcmadr,%0011,29,29,21
	lea	suji(pc),a1
	move.l	buffer1(pc),d0
	bsr	get_hex32
	display2	suji,%0011,49,29,8

	display2	i_start,%0011,0,30,20
	lea	suji(pc),a1
	move.l	start_point1(pc),d0
	move.l	end_point1(pc),d7
	move.l	d7,d6
	tst.b	data_type
	bne	inf1
	sub.l	buffer1(pc),d0
	sub.l	buffer1(pc),d7
	sub.l	start_point1(pc),d6
	beq	inf2
	addq.l	#2,d6
	bra	inf2
inf1:
	sub.l	address1(pc),d0
	sub.l	address1(pc),d7
	sub.l	start_point1(pc),d6
	beq	inf2
	addq.l	#1,d6
inf2:
	bsr	get_hex32
	display2	suji,%0011,19,30,8

	display2	i_end,%0011,29,30,21
	lea	suji(pc),a1
	move.l	d7,d0
	bsr	get_hex32
	display2	suji,%0011,49,30,8

	display2	i_size,%0011,0,31,20
	lea	suji(pc),a1
	move.l	size1(pc),d0
	bsr	get_hex32
	display2	suji,%0011,19,31,8

	display2	i_efsz,%0011,29,31,20
	lea	suji(pc),a1
	move.l	d6,d0
	bsr	get_hex32
	display2	suji,%0011,49,31,8

	bsr	get_spc
*バンクB
	bsr	clr_bottom

	display2	bank_b,%1111,0,28,8
	display2	i_adpcmadr,%0011,0,29,20
	lea	suji(pc),a1
	move.l	address2(pc),d0
	bsr	get_hex32
	display2	suji,%0011,19,29,8

	display2	i_pcmadr,%0011,29,29,21
	lea	suji(pc),a1
	move.l	buffer2(pc),d0
	bsr	get_hex32
	display2	suji,%0011,49,29,8

	display2	i_start,%0011,0,30,20
	lea	suji(pc),a1
	move.l	start_point2(pc),d0
	move.l	end_point2(pc),d7
	move.l	d7,d6
	tst.b	data_type
	bne	inf3
	sub.l	buffer2(pc),d0
	sub.l	buffer2(pc),d7
	sub.l	start_point2(pc),d6
	beq	inf4
	addq.l	#2,d6
	bra	inf4
inf3:
	sub.l	address2(pc),d0
	sub.l	address2(pc),d7
	sub.l	start_point2(pc),d6
	beq	inf4
	addq.l	#1,d6
inf4:
	bsr	get_hex32
	display2	suji,%0011,19,30,8

	display2	i_end,%0011,29,30,21
	lea	suji(pc),a1
	move.l	d7,d0
	bsr	get_hex32
	display2	suji,%0011,49,30,8

	display2	i_size,%0011,0,31,20
	lea	suji(pc),a1
	move.l	size2(pc),d0
	bsr	get_hex32
	display2	suji,%0011,19,31,8

	display2	i_efsz,%0011,29,31,20
	lea	suji(pc),a1
	move.l	d6,d0
	bsr	get_hex32
	display2	suji,%0011,49,31,8

	bsr	get_spc

	bsr	clr_bottom

	display2	max_free,%0011,0,28,10
	pea	$ffffff
	DOS	_MALLOC
	addq.w	#4,sp
	andi.l	#$ffffff,d0
	move.l	d0,d7
	lea	suji(pc),a1
	bsr	get_hex32
	display2	suji,%0011,10,28,8

	display2	kakko,%0011,18,28,10
	lea	suji(pc),a1
	move.l	d7,d0
	bsr	num_to_str
	display2	suji+2,%0011,19,28,8


	bsr	get_spc
	movem.l	reg_buff2,d0-d7/a0-a6
	bra	non_ope

playing:				*再生
	clr.b	played		*marker clear
	bsr	get_pt1
	moveq.l	#0,d7
	moveq.l	#0,d1
	IOCS	_ADPCMMOD	*まず停止

	move.b	data_a(pc),d0
	or.b	data_b(pc),d0
	beq	ext_play

	lea	address1(pc),a0
	tst.b	bank_sel
	beq	play??
	lea	20(a0),a0		*bank_b base addr.
	tst.b	data_b
	beq	ext_play
	bra	do_play
play??:
	tst.b	data_a
	beq	ext_play
do_play:
	move.b	sampling_rate(pc),d1
	lsl.w	#8,d1
	move.b	out_assign(pc),d1

	tst.b	data_type		*データモードがpcmのとき
	beq	play_pcm

	bsr	put_pt1
	movea.l	pstart(a0),a1
	move.l	pend(a0),d2
	sub.l	a1,d2
	addq.l	#1,d2
*	cmpi.l	#$ff00,d2
*	bls	go_ADPCMOUT
*	bsr	disp_pl_mes
go_ADPCMOUT:
	st	played			*set marker
	IOCS	_ADPCMOUT
ext_play:
	tst.b	played			*演奏してなければ関係無し
	beq	del_plmes
	moveq.l	#0,d0
	bsr	mt_on_off		*monitor mode強制オフ
del_plmes:
*	bsr	put_pt1			*メッセージ消去
	bra	complete
play_pcm:				*データモードがpcm
	tst.l	paddress(a0)
	beq	make_adpcm
plp00:
	tst.b	bank_sel
	bne	plp01
	bsr	calc_es_adpcm_a
	bra	plp02
plp01:
	bsr	calc_es_adpcm_b
plp02:
	bsr	put_pt1
	movea.l	pstart(a0),a1
	move.l	pend(a0),d2
	sub.l	a1,d2
	addq.l	#1,d2
*	cmpi.l	#$ff00,d2
*	bls	go_ADPCMOUT2
*	bsr	disp_pl_mes
go_ADPCMOUT2:
	st	played			*set marker
	IOCS	_ADPCMOUT

	tst.b	bank_sel
	bne	plp03
	bsr	calc_es_pcm_a
	bra	ext_play
plp03:
	bsr	calc_es_pcm_b
	bra	ext_play
*disp_pl_mes:
*	movem.l	d0-d4/a1,-(sp)
*	display2	pl_mes,%1110,43,15,11	*メッセージ表示
*	movem.l	(sp)+,d0-d4/a1
*	rts

make_adpcm:
	bsr	dsp_cv_mes
	tst.b	bank_sel
	bne	m_a_b
	bsr	make_adpcm_a
	bmi	ext_play
	bra	plp00
m_a_b:
	bsr	make_adpcm_b
	bmi	ext_play
	bra	plp00
make_adpcm_a:
	movem.l	d0-d7/a0-a6,-(sp)
	move.l	size1,d1
	move.l	d1,-(sp)
	DOS	_MALLOC
	addq.w	#4,sp
	move.l	d0,mm_error
	bsr	print_em
	bmi	ext_make_aa			*case:error
	movea.l	d0,a0
	move.l	d0,address1
	movea.l	buffer1(pc),a1
	move.l	size1(pc),d0
	bsr	pcm_to_adpcm
	moveq.l	#0,d0
ext_make_aa:
	tst.l	d0
	movem.l	(sp)+,d0-d7/a0-a6
	rts
make_adpcm_b:
	movem.l	d0-d7/a0-a6,-(sp)
	move.l	size2(pc),d1
	move.l	d1,-(sp)
	DOS	_MALLOC
	addq.w	#4,sp
	move.l	d0,mm_error
	bsr	print_em
	bmi	ext_make_ab			*case:error
	movea.l	d0,a0
	move.l	d0,address2
	movea.l	buffer2(pc),a1
	move.l	size2(pc),d0
	bsr	pcm_to_adpcm
	moveq.l	#0,d0
ext_make_ab:
	tst.l	d0
	movem.l	(sp)+,d0-d7/a0-a6
	rts

paddress:	equ	0
pbuffer:	equ	4
psize:		equ	8
pstart:		equ	12
pend:		equ	16

record:				*録音
	movem.l	d0-d7/a0-a6,-(sp)
	moveq.l	#0,d7
	clr.l	work1

	lea	address1(pc),a0
	tst.b	bank_sel
	bne	free_b
*free_a:
	clr.b	data_a
	bsr	mfree_adpcm_a
	bsr	mfree_pcm_a
	bra	do_rec
free_b:
	lea	20(a0),a0		*bank_b
	clr.b	data_b
	bsr	mfree_adpcm_b
	bsr	mfree_pcm_b
do_rec:
	clr.l	pstart(a0)
	clr.l	pend(a0)

	bsr	get_pt1		*ﾒｯｾｰｼﾞｴﾘｱの背景をゲット

	move.l	rec_data_size(pc),d2
	move.l	d2,psize(a0)	*save size
	move.l	d2,-(sp)	*d2=rec size
	DOS	_MALLOC
	addq.w	#4,sp
	move.l	d0,mm_error
	bsr	print_em
	bmi	ext_rcd		*case:error
	move.l	d0,a1
	move.l	d0,paddress(a0)

	movem.l	d2/a1,-(sp)

	move.w	sr,-(sp)
	ori.w	#$0700,sr
	move.b	#$10,CCR3	*動作しない
	st	CSR3
	move.b	#$88,$e92003
	tst.b	trigger		*オートモードの時はadpcm all endしない
	bne	non_aae
	move.b	#%0000_0001,$e92001	*adpcm all end
	bra	@f
non_aae:
	addq.w	#1,a1		*peakで検出した１バイトもデータに入れるため
	subq.l	#1,d2		*つじつま合わせ
@@:
	clr.b	DMADSTAT.w
	move.w	(sp)+,sr
*	bsr	set_pan_frq

	move.b	#$c0,DCR3
	move.b	#$82,OCR3	*reqによる8ビット転送
	move.b	#FC_SUPER_DATA,MFC3
	move.b	#FC_SUPER_DATA,DFC3
	move.b	#$04,SCR3	*メモリー側のみ増加
	move.l	a1,MAR3		*set dest. addr.
	move.l	#$e92003,DAR3	*pcm addr.
	move.w	d2,MTC3		*set counter

	cmpi.l	#$ffff,d2
	bls	chk_trg
				*データ長が$ffffより長いケース
	move.l	d2,d3		*d3=size
	move.l	d2,d1
	move.l	#$ffff,d2
	bsr	wari
	tst.l	d2
	beq	_6bai
	addq.l	#1,d1
_6bai:
	move.l	d1,d7
	add.l	d1,d1
	move.l	d1,d2
	add.l	d1,d1
	add.l	d2,d1		d1=d1*6
	move.l	d1,-(sp)
	DOS	_MALLOC
	addq.w	#4,sp
	move.l	d0,mm_error
	bsr	print_em
	bmi	ext_rcd		*case:error
	move.l	d0,work1
	move.l	d0,a2		*a2=aray table
mk_at_l:
	cmpi.l	#$ffff,d3
	bcs	last_at
	move.l	a1,(a2)+	*addr
	move.w	#$ffff,(a2)+	*len
	adda.l	#$ffff,a1
	sub.l	#$ffff,d3
	bne	mk_at_l
	bra	set_at_dma
last_at:
	move.l	a1,(a2)+	*addr
	move.w	d3,(a2)+	*len
set_at_dma:
	move.b	#%1000_1010,OCR3
	move.w	d7,BTC3		*num of aray tbl
	move.l	work1(pc),BAR3	*aray tbl addr

chk_trg:
	move.b	trigger(pc),d0
	and.b	monitor_mode(pc),d0
	beq	go_rec
	display2	peak_mes,%1110,41,15,13

	lea	$e92003,a1
	moveq.l	#$08,d1
	moveq.l	#$80,d2
peak_lp:
	move.b	(a1),d7
	cmp.b	d1,d7
	beq	peak_lp
	cmp.b	d2,d7
	beq	peak_lp
go_rec:
	display2	rc_mes,%1110,41,15,13	*メッセージ表示
	movem.l	(sp)+,d2/a1

	move.b	d7,(a1)

	move.b	#$88,CCR3	*dma go!
	move.b	#$4,$e92001	*pcm rec mode
rc00:
	moveq.l	#0,d1
	IOCS	_BITSNS
	btst.l	#1,d0
	beq	chk_mtc3
	bsr	adpcm_end
	moveq.l	#0,d7
	tst.b	bank_sel
	bne	@f
	bsr	_bank_a
	bsr	waku1
	bsr	init_hex_area_a
	bra	ext_rcd
@@:
	bsr	_bank_b
	bsr	waku2
	bsr	init_hex_area_b
	bra	ext_rcd
chk_mtc3:
	tst.w	MTC3
	bne	rc00		*dmaが動作終了するまでループ
exit_rc00:
	bsr	put_pt1

	tst.b	bank_sel
	bne	mark_dtb

	st	data_a
	bsr	waku1
	tst.b	data_type
	bne	rc01
	bsr	get_pt1
	display2	cv_mes,%1110,41,15,14
	bsr	make_pcm_a
	bmi	ext_rcd
	movea.l	4(a0),a6	*a6=buffer
	move.l	8(a0),d6
	move.w	#scr_wy,genten_y
	bsr	dsp_wv
	bra	rc01_
rc01:
	movea.l	(a0),a6		*a6=buffer
	move.l	8(a0),d6
	move.w	#scr_wy,genten_y
	bsr	dsp_wv_2
rc01_:
	bsr	init_hex_area_a
	bra	ext_rcd
mark_dtb:
	st	data_b
	bsr	waku2
	tst.b	data_type
	bne	rc02
	bsr	get_pt1
	display2	cv_mes,%1110,41,15,14
	bsr	make_pcm_b
	bmi	ext_rcd
	movea.l	4(a0),a6	*a6=buffer
	move.l	8(a0),d6
	move.w	#scr_wy+plus_,genten_y
	bsr	dsp_wv
	bra	rc02_
rc02:
	movea.l	(a0),a6		*a6=buffer
	move.l	8(a0),d6
	move.w	#scr_wy+plus_,genten_y
	bsr	dsp_wv_2
rc02_:
	bsr	init_hex_area_b

ext_rcd:
	tst.l	work1
	beq	extr1
	move.l	work1(pc),-(sp)
	DOS	_MFREE		*bye aray table
	addq.w	#4,sp
extr1:
	tst.b	monitor_mode
	bne	extr2
	move.b	#$08,$e92003
	move.b	#%0000_0001,$e92001	*adpcm all end
extr2:
	movem.l	(sp)+,d0-d7/a0-a6
	bsr	put_pt1			*メッセージ消去
	bra	complete

meyasu:	equ	48		*頭だしの目安(0 levelが幾つあったらデータエンドにするか)
quantize:			*波形の頭だし
	lea	address1(pc),a0
*	moveq.l	#0,d0
	tst.b	bank_sel
	beq	qt0
	lea	20(a0),a0		*bank_b
	tst.b	data_b
	beq	exit_qt
	bra	qt1
qt0:
	tst.b	data_a
	beq	exit_qt
qt1:
	move.l	8(a0),d2	*d2=size
	tst.b	data_type
	bne	qt2
*	moveq.l	#4,d0		*pcm
	lsl.l	#2,d2		*size=size*4(PCM DATA SIZE)
qt2:
*	movea.l	(a0,d0.w),a1	*base data addr.
	movea.l	pstart(a0),a1	*base data addr.
	tst.b	data_type
	beq	case_qt_pcm
				*case adpcm
	lea	(a1,d2.l),a2	*a2=end addr
	move.l	a1,12(a0)	*default
	move.l	a2,16(a0)
get_hd_lp:
	cmpa.l	a1,a2
	beq	exit_qt		*最後まで来てしまった…
	move.b	(a1)+,d0
	cmpi.b	#$80,d0
	beq	get_hd_lp
	cmpi.b	#$08,d0
	beq	get_hd_lp
	subq.w	#1,a1
	move.l	a1,12(a0)	*save start
	addq.w	#1,a1
get_tl:
	moveq.l	#0,d1		*found counter
get_tl_lp:
	cmpa.l	a1,a2
	beq	exit_qt
	move.b	(a1)+,d0
	cmpi.b	#$80,d0
	beq	@f
	cmpi.b	#$08,d0
	bne	get_tl_lp
@@:
	move.l	a1,work1
get_tl_lp1:
	cmpa.l	a1,a2
	beq	exit_qt
	move.b	(a1)+,d0
	cmpi.b	#$80,d0
	beq	@f
	cmpi.b	#$08,d0
	bne	get_tl
@@:
	addq.l	#1,d1
	cmpi.b	#meyasu,d1
	bcs	get_tl_lp1
	move.l	work1(pc),d0
	subq.l	#1,d0
	move.l	d0,16(a0)	*save end
exit_qt:
	tst.b	bank_sel
	bne	qt_b
	bsr	init_hex_area_a
	bra	m_lp
qt_b:
	bsr	init_hex_area_b
	bra	m_lp
lmt:	equ	2
case_qt_pcm:			*データモードがＰＣＭのケース
	lea	(a1,d2.l),a2	*a2=end addr
	move.l	a1,12(a0)	*default
	move.l	a2,16(a0)
	moveq.l	#0,d3
get_hd_lp_:
	cmpa.l	a1,a2
	bls	exit_qt		*最後まで来てしまった…
	move.w	(a1),d0
	sub.w	d3,d0
	move.w	(a1)+,d3
	tst.w	d0
	bpl	@f
	neg.w	d0
@@:
	cmpi.w	#lmt,d0
	bls	get_hd_lp_
	subq.w	#2,a1
	move.l	a1,12(a0)	*save start
	addq.w	#2,a1
get_tl_:
	moveq.l	#0,d1		*found counter
	moveq.l	#0,d3
get_tl_lp_:
	cmpa.l	a1,a2
	bls	exit_qt
	move.w	(a1),d0
	sub.w	d3,d0
	move.w	(a1)+,d3
	tst.w	d0
	bpl	@f
	neg.w	d0
@@:
	cmpi.w	#lmt,d0
	bhi	get_tl_lp_
	move.l	a1,work1
get_tl_lp1_:
	cmpa.l	a1,a2
	bls	exit_qt
	move.w	(a1),d0
	sub.w	d3,d0
	move.w	(a1)+,d3
	tst.w	d0
	bpl	@f
	neg.w	d0
@@:
	cmpi.w	#lmt,d0
	bhi	get_tl_
	addq.l	#1,d1		*find=find+1
	cmpi.b	#meyasu*2,d1
	bcs	get_tl_lp1_
	move.l	work1(pc),d0
	subq.l	#2,d0
	move.l	d0,16(a0)	*save end
	bra	exit_qt

truncate:			*波形の切り出し
	movem.l	d6/a6,-(sp)
	bsr	get_pt1
	display2	cv_mes,%1110,41,15,14

	lea	address1(pc),a0
	clr.l	trn_rvs-address1(a0)
	tst.b	bank_sel
	beq	tr1
	tst.b	data_b		*データbある?
	beq	ext_tr
	lea	20(a0),a0		*bank_b
	bra	tr2
tr1:
	tst.b	data_a		*データaある?
	beq	ext_tr
tr2
	tst.b	bank_sel
	bne	tr__b

	tst.l	4(a0)
	bne	go_tr_pcm_a
	bsr	make_pcm_a
	bmi	ext_tr
go_tr_pcm_a:
	bsr	do_tr_pcm

	pea	(a0)		*push base addr

	tst.l	paddress(a0)
	bne	tr3
	bsr	make_adpcm_a
	bra	tr4
tr3:
	movea.l	4(a0),a1
	move.l	8(a0),d0
	movea.l	(a0),a0
	bsr	pcm_to_adpcm	*pcm data -> adpcm
tr4:
	move.l	(sp)+,a0

	move.l	psize(a0),-(sp)
	move.l	paddress(a0),-(sp)
	DOS	_SETBLOCK
	addq.w	#8,sp

	clr.l	12(a0)		*start_point
	clr.l	16(a0)		*end_point
	bsr	waku1
	bsr	init_hex_area_a
	movea.l	4(a0),a6	*a6=buffer
	move.l	8(a0),d6
	move.w	#scr_wy,genten_y
	bsr	dsp_wv
	bra	ext_tr
tr__b:
	tst.l	4(a0)
	bne	go_tr_pcm_b
	bsr	make_pcm_b
	bmi	ext_tr
go_tr_pcm_b:
	bsr	do_tr_pcm

	pea	(a0)		*push base addr.

	tst.l	paddress(a0)
	bne	tr5
	bsr	make_adpcm_b
	bra	tr6
tr5:
	movea.l	4(a0),a1
	move.l	8(a0),d0
	movea.l	(a0),a0
	bsr	pcm_to_adpcm	*pcm data -> adpcm
tr6:
	move.l	(sp)+,a0

	move.l	psize(a0),-(sp)
	move.l	paddress(a0),-(sp)
	DOS	_SETBLOCK
	addq.w	#8,sp

	clr.l	12(a0)		*start_point
	clr.l	16(a0)		*end_point
	bsr	waku2
	bsr	init_hex_area_b
	movea.l	4(a0),a6	*a6=buffer
	move.l	8(a0),d6
	move.w	#scr_wy+plus_,genten_y
	bsr	dsp_wv
ext_tr:
	bsr	put_pt1
	movem.l	(sp)+,d6/a6
	bra	m_lp

do_tr_pcm:			*実際にデータを転送
	tst.l	4(a0)
	beq	ext_do_tr
	tst.b	data_type
	beq	set_prm_tr
	bsr	calc_es
	addq.l	#2,trn_rvs
set_prm_tr:
	movea.l	12(a0),a1	*start point(src)
	movea.l	4(a0),a2	*buffer(dest)
	moveq.l	#%00000101,d1	*mode
	move.l	16(a0),d2	*end_point
	sub.l	a1,d2		*d1=n bytes
	addq.l	#2,d2
	add.l	trn_rvs(pc),d2
	move.l	d2,d6   	*save size
	lsr.l	#2,d6
	move.l	d6,8(a0)
	bsr	trans_dma

	move.l	d2,-(sp)
	pea	(a2)
	DOS	_SETBLOCK
	addq.w	#8,sp
ext_do_tr:
	rts
calc_es:
	tst.b	bank_sel
	bne	calc_es0
	bsr	calc_es_pcm_a
	rts
calc_es0:
	bsr	calc_es_pcm_b
	rts

make_pcm_a:			*pcmデータを作る
	movem.l	d0-d7/a0-a6,-(sp)
	move.l	size1(pc),d1
	lsl.l	#2,d1			*d1=d1*4 pcmデータバッファは
	move.l	d1,-(sp)		*adpcmバッファの4倍の大きさが必要
	DOS	_MALLOC
	addq.w	#4,sp
	move.l	d0,mm_error
	bsr	print_em
	bmi	ext_mkpa		*case:error
	movea.l	d0,a1
	move.l	d0,buffer1
	move.l	address1(pc),a0
	move.l	size1(pc),d0
	bsr	just_adpcm_to_pcm
	moveq.l	#0,d0
ext_mkpa:
	tst.l	d0
	movem.l	(sp)+,d0-d7/a0-a6
	rts

make_pcm_b:			*pcmデータを作る
	movem.l	d0-d7/a0-a6,-(sp)
	move.l	size2(pc),d1
	lsl.l	#2,d1			*d1=d1*4 pcmデータバッファは
	move.l	d1,-(sp)		*adpcmバッファの4倍の大きさが必要
	DOS	_MALLOC
	addq.w	#4,sp
	move.l	d0,mm_error
	bsr	print_em
	bmi	ext_mkpb		*case:error
	movea.l	d0,a1
	move.l	d0,buffer2
	move.l	address2(pc),a0
	move.l	size2(pc),d0
	bsr	just_adpcm_to_pcm
	moveq.l	#0,d0
ext_mkpb:
	tst.l	d0
	movem.l	(sp)+,d0-d7/a0-a6
	rts

ast_ope:			*データ表示モードチェンジ
	pea	m_lp(pc)
	not.b	prt_md
	movem.l	d0-d7/a0-a6,-(sp)
	moveq.l	#3,d6
ast_lp:
	moveq.l	#'5',d0
	bsr	data_move_
	dbra	d6,ast_lp
	movem.l	(sp)+,d0-d7/a0-a6
	bra	ext_dtp

data_move?:
	pea	m_lp(pc)	*帰還先をプッシュ(z80みたい…)
data_move_:
	tst.b	data_type
	beq	case_dt_PCM
	moveq.l	#1,d2		*ADPCMデータの場合
	moveq.l	#16,d4
	bra	which_point

case_dt_PCM:			*PCMデータの場合
	moveq.l	#2,d2
	moveq.l	#32,d4
which_point:
stpt1?:				*start point bank a
	tst.b	d6
	bne	edpt1?
	tst.b	data_a
	beq	ext_dtp
	moveq.l	#11,d3		*set y
	lea	1.w,a6		*単にデータレジスタとして使っているだけ(以下同じ)
	lea	address1(pc),a1
	bsr	ope_stp
	tst.b	data_type
	beq	@f
	tst.l	address1
	bne	sv_stpt1
	bra	exit_stpt1
@@:
	tst.l	buffer1
	beq	exit_stpt1
sv_stpt1:
	move.l	a2,start_point1
exit_stpt1:
ofsst1:				*オフセット値表示
	move.l	start_point1(pc),d0
	tst.b	data_type
	bne	@f
	sub.l	buffer1(pc),d0
	bra	_ts0
@@:
	sub.l	address1(pc),d0
_ts0:
	bsr	num_to_str
	display2	suji,%0011,41,0,10

	move.l	end_point1(pc),d0
	tst.b	data_type
	bne	@f
	sub.l	start_point1(pc),d0
	bcs	_ts2
	addq.l	#2,d0
	bra	_ts1
@@:
	sub.l	start_point1(pc),d0
	bcs	_ts2
	addq.l	#1,d0
_ts1:
	bsr	num_to_str
	display2	suji,%0011,83,0,10
_ts2:
	rts

edpt1?:
	cmpi.b	#1,d6
	bne	stpt2?
	tst.b	data_a
	beq	ext_dtp
	moveq.l	#12,d3		*set y
	lea	1.w,a6
	lea	address1(pc),a1
	bsr	ope_edp
	tst.b	data_type
	beq	@f
	tst.l	address1
	bne	sv_edpt1
	bra	exit_edpt1
@@:
	tst.l	buffer1
	beq	exit_edpt1
sv_edpt1:
	move.l	a2,end_point1
exit_edpt1:
ofsed1:				*オフセット値表示
	move.l	end_point1(pc),d0
	tst.b	data_type
	bne	@f
	sub.l	buffer1(pc),d0
	bra	_ts3
@@:
	sub.l	address1(pc),d0
_ts3:
	bsr	num_to_str
	display2	suji,%0011,60,0,10

	move.l	end_point1(pc),d0
	tst.b	data_type
	bne	@f
	sub.l	start_point1(pc),d0
	bcs	_ts5
	addq.l	#2,d0
	bra	_ts4
@@:
	sub.l	start_point1(pc),d0
	bcs	_ts5
	addq.l	#1,d0
_ts4:
	bsr	num_to_str
	display2	suji,%0011,83,0,10
_ts5:
	rts

stpt2?:
	cmpi.b	#2,d6
	bne	edpt2?
	tst.b	data_b
	beq	ext_dtp
	moveq.l	#25,d3		*set y
	lea	plus_+1,a6
	lea	address2(pc),a1
	bsr	ope_stp
	tst.b	data_type
	beq	@f
	tst.l	address2
	bne	sv_stpt2
	bra	exit_stpt2
@@:
	tst.l	buffer2
	beq	exit_stpt2
sv_stpt2:
	move.l	a2,start_point2
exit_stpt2:
ofsst2:				*オフセット値表示
	move.l	start_point2(pc),d0
	tst.b	data_type
	bne	@f
	sub.l	buffer2(pc),d0
	bra	_ts6
@@:
	sub.l	address2(pc),d0
_ts6:
	bsr	num_to_str
	display2	suji,%0011,41,14,10

	move.l	end_point2(pc),d0
	tst.b	data_type
	bne	@f
	sub.l	start_point2(pc),d0
	bcs	_ts8
	addq.l	#2,d0
	bra	_ts7
@@:
	sub.l	start_point2(pc),d0
	bcs	_ts8
	addq.l	#1,d0
_ts7:
	bsr	num_to_str
	display2	suji,%0011,83,14,10
_ts8:
	rts

edpt2?:
	cmpi.b	#3,d6
	bne	ext_dtp		*メインループ帰還
	tst.b	data_b
	beq	ext_dtp
	moveq.l	#26,d3		*set y
	lea	plus_+1,a6
	lea	address2(pc),a1
	bsr	ope_edp
	tst.b	data_type
	beq	@f
	tst.l	address2
	bne	sv_edpt2
	bra	exit_edpt2
@@:
	tst.l	buffer2
	beq	exit_edpt2
sv_edpt2:
	move.l	a2,end_point2
exit_edpt2:
ofsed2:				*オフセット値表示
	move.l	end_point2(pc),d0
	tst.b	data_type
	bne	@f
	sub.l	buffer2(pc),d0
	bra	_ts9
@@:
	sub.l	address2(pc),d0
_ts9:
	bsr	num_to_str
	display2	suji,%0011,60,14,10

	move.l	end_point2(pc),d0
	tst.b	data_type
	bne	@f
	sub.l	start_point2(pc),d0
	bcs	_ts11
	addq.l	#2,d0
	bra	_ts10
@@:
	sub.l	start_point2(pc),d0
	bcs	_ts11
	addq.l	#1,d0
_ts10:
	bsr	num_to_str
	display2	suji,%0011,83,14,10
_ts11:
	rts

ope_stp:
	move.l	psize(a1),d5
	tst.b	data_type
	beq	stpt1_buf
	movea.l	paddress(a1),a3
	lea	-1(a3,d5.l),a4	*end addr
	bra	st1
stpt1_buf:
	movea.l	pbuffer(a1),a3
	lsl.l	#2,d5
	lea	-2(a3,d5.l),a4	*end addr
st1:
	move.l	pstart(a1),d1
	movea.l	d1,a2
	bne	go_kchk_st1
	lea	(a3),a2
go_kchk_st1:
	lea	(a3),a0		*a0=start
	move.l	pend(a1),d1
	movea.l	d1,a5
	bne	st1_
	lea	(a4),a5		*a5=end
st1_:
	bra	k_chk

ope_edp:
	move.l	psize(a1),d5
	tst.b	data_type
	beq	edpt1_buf
	movea.l	paddress(a1),a3
	lea	-1(a3,d5.l),a4
	bra	ed1
edpt1_buf:
	movea.l	pbuffer(a1),a3
	lsl.l	#2,d5
	lea	-2(a3,d5.l),a4
ed1:
	move.l	pend(a1),d1
	movea.l	d1,a2
	bne	go_kchk_ed1
	lea	(a3),a2
go_kchk_ed1:
	lea	(a4),a5		*a5=end
	move.l	pstart(a1),d1
	movea.l	d1,a0		*a0=start
	bne	ed1_
	lea	(a3),a0
ed1_:
*	bra	k_chk

k_chk:				*キー入力チェック
	moveq.l	#0,d1
	move.b	d0,d1
	sub.b	#$30,d1
	bmi	kchk_period
	cmpi.b	#9,d1
	bhi	kchk_period
	add.w	d1,d1
	move.w	k_j_tbl(pc,d1.w),a1
	jmp	k_j_tbl(pc,a1.w)

k_j_tbl:	dc.w	kchk0-k_j_tbl
		dc.w	kchk1-k_j_tbl,ext_dtp-k_j_tbl,kchk3-k_j_tbl
		dc.w	kchk4-k_j_tbl,kchk5-k_j_tbl,kchk6-k_j_tbl
		dc.w	kchk7-k_j_tbl,ext_dtp-k_j_tbl,kchk9-k_j_tbl

chk_edge1	macro
	local	abc
	move.l	a3,d0
	btst.l	#0,d3
	bne	abc
	add.l	d2,d0
abc:
	endm

chk_edge2	macro
	local	abc
	move.l	a4,d0
	btst.l	#0,d3
	beq	abc
	sub.l	d2,d0
abc:
	endm

*	a3=start address
*	a4=end address
kchk4:
	chk_edge1
	cmpa.l	d0,a2
	beq	ext_dtp
	suba.l	d2,a2
	bra	do_disp_dt
kchk6:
	chk_edge2
	cmpa.l	d0,a2
	beq	ext_dtp
	adda.l	d2,a2
	bra	do_disp_dt
kchk1:
	lea	(a2),a1
	move.l	d5,d4
	lsr.l	#5,d4
	bclr.l	#0,d4
	tst.l	d4
	bne	k1__
	move.l	d2,d4
k1__:
	suba.l	d4,a1
	chk_edge1
	cmp.l	a1,d0
	bge	copy_d0		*スタートに帰る
	lea	(a1),a2
	bra	do_disp_dt
kchk3:
	move.l	d5,d4
	lsr.l	#5,d4
	bclr.l	#0,d4
	tst.l	d4
	bne	k3__
	move.l	d2,d4
k3__:
	lea	(a2,d4.l),a1
	chk_edge2
	cmp.l	a1,d0
	ble	copy_d0		*エンドにする
	lea	(a1),a2
	bra	do_disp_dt
kchk7:
	lea	(a2),a1
	suba.l	d4,a1
	chk_edge1
	cmp.l	a1,d0
	bge	copy_d0		*スタートに帰る
	lea	(a1),a2
	bra	do_disp_dt
kchk9:
	lea	(a2,d4.l),a1
	chk_edge2
	cmp.l	a1,d0
	ble	copy_d0		*エンドにする
	lea	(a1),a2
	bra	do_disp_dt
kchk0:
	lea	(a3),a2
	btst.l	#0,d3
	bne	do_disp_dt	*case start p
	add.l	d2,a2
	bra	do_disp_dt
copy_d0:
	move.l	d0,a2
	bra	do_disp_dt

kchk5:				*単なる表示(init_hex_areaで使用)
	cmpa.l	a2,a4
	ble	kchk_pd_	*エンドにする
	cmpa.l	a2,a3
	bge	kchk0		*スタートに帰る
	bra	do_disp_dt

kchk_period:
	cmpi.b	#'.',d0
	bne	kchk_cmm
kchk_pd_:
	lea	(a4),a2
	btst.l	#0,d3
	beq	do_disp_dt	*case start p
	sub.l	d2,a2
	bra	do_disp_dt
kchk_cmm:
	cmpi.b	#',',d0
	bne	ext_dtp
	move.l	d5,d1
	lsr.l	d1
	bclr.l	#0,d1
	lea	(a3,d1.l),a2	*真ん中に…
	bra	do_disp_dt

do_disp_dt:
	move.l	a3,d0
	beq	ext_dtp		*dataがない
	cmpa.l	a0,a2
	bcc	ddd1
	lea	(a0,d2.w),a2
	bra	ddd2
ddd1:
	cmpa.l	a5,a2
	bls	ddd2
	move.l	a5,a2
	sub.l	d2,a2
ddd2:
	tst.b	data_type
	beq	dt_print16

	*ＡＤＰＣＭのデータのケース
dt_print8:
	* < d3=y座標
	* < a2=middle print point addr.
	lea	prt_bf(pc),a1
	lea	-13(a2),a2
	moveq.l	#27-1,d2	*number of disp.
prtd8_lp:
	cmpa.l	a2,a3
	bhi	prt_ast8
	cmpa.l	a2,a4
	bcs	prt_ast8
	move.b	(a2),d0
	bsr	get_hex8
	addq.w	#2,a1
	move.b	#' ',(a1)+
prtd8_lp0:
	addq.w	#1,a2		*ad=ad+1.b
	dbra	d2,prtd8_lp
	moveq.l	#%0011,d1	*atr
	moveq.l	#14,d2		*print start x
	moveq.l	#27*3-1,d4	*len
	lea	prt_bf(pc),a1
	IOCS	_B_PUTMES
	lea	-14(a2),a2	*入力時の値に戻す
	bra	tate_sen_
ext_dtp:
	rts
prt_ast8:
	move.b	#'*',(a1)+
	move.b	#'*',(a1)+
	move.b	#' ',(a1)+
	bra	prtd8_lp0

get_hex8:			*値→16進数文字列(2bytes)
	* < d0=data value
	* < a1=格納したいアドレス
	* > (a1)=ascii numbers
	* X d0,d1,d4,a1
	addq.w	#2,a1
	clr.b	(a1)
	move.b	d0,d1
	andi.b	#$0f,d1
	add.b	#$30,d1
	cmpi.b	#'9',d1
	bls	its_hex8
	addq.b	#7,d1
its_hex8:
	move.b	d1,-(a1)
	lsr.b	#4,d0

	add.b	#$30,d0
	cmpi.b	#'9',d0
	bls	its_hex8_
	addq.b	#7,d0
its_hex8_:
	move.b	d0,-(a1)
	rts

tate_sen_:		*赤い縦線を引く
	* < a2=pointer
	* < a3=adress/buffer
	move.l	a2,d1
	sub.l	a3,d1
	move.l	#scr_wx,d0
	bsr	kake		*d0.l*d1.l=d1.l
	move.l	d5,d2
	bsr	wari		*d1=d1/d2(d5)=(760*point)/size...d2
	tst.l	d2
	beq	plus_ofs
	addq.l	#1,d1		*余りがあるなら… +1
plus_ofs:
	add.w	#start_x,d1	*offset
	move.l	d1,d0		*x
	move.l	a6,d1		*y

	cmpi.b	#11,d3
	bne	tate_0
	movea.l	last_txt_adas(pc),a0
	lea	$e40000,a1
	bsr	txt_ln
	move.l	a0,last_txt_adas
	rts
tate_0:
	cmpi.b	#12,d3
	bne	tate_1
	movea.l	last_txt_adae(pc),a0
	lea	$e60000,a1
	bsr	txt_ln
	move.l	a0,last_txt_adae
	rts
tate_1:
	cmpi.b	#25,d3
	bne	tate_2
	movea.l	last_txt_adbs(pc),a0
	lea	$e40000,a1
	bsr	txt_ln
	move.l	a0,last_txt_adbs
	rts
tate_2:
	movea.l	last_txt_adbe(pc),a0
	lea	$e60000,a1
	bsr	txt_ln
	move.l	a0,last_txt_adbe
	rts

dt_print16:
	* < d3=y座標
	* < a2=middle print point addr.
	lea	prt_bf(pc),a1
	lea	-16(a2),a2
	moveq.l	#17-1,d2	*number of disp.
	moveq.l	#0,d0
prtd16_lp:
	cmpa.l	a2,a3
	bhi	prt_ast
	cmpa.l	a2,a4
	bcs	prt_ast
	move.w	(a2),d0
	tst.b	prt_md
	beq	pl_only
	bsr	get_bcd
	bra	inc_ptr
pl_only:
	bsr	get_hex
	addq.w	#4,a1
inc_ptr:
	move.b	#' ',(a1)+
prtd16_lp0:
	addq.w	#2,a2		*ad=ad+1.w
	dbra	d2,prtd16_lp
	moveq.l	#%0011,d1	*atr
	moveq.l	#12,d2		*print start x
	moveq.l	#17*5-1,d4	*len
	lea	prt_bf(pc),a1
	IOCS	_B_PUTMES
	lea	-18(a2),a2	*入力時の値に戻す
	bra	tate_sen_

prt_ast:
	move.b	#'*',(a1)+
	move.b	#'*',(a1)+
	move.b	#'*',(a1)+
	move.b	#'*',(a1)+
	move.b	#' ',(a1)+
	bra	prtd16_lp0

get_hex:			*値→16進数文字列(2bytes)
	* < d0=data value
	* < a1=格納したいアドレス
	* > (a1)=ascii numbers
	* X d0,d1,d4,a1
	addq.w	#4,a1
	clr.b	(a1)
	moveq.l	#3,d4
gh_lp:
	move.b	d0,d1
	andi.b	#$0f,d1
	add.b	#$30,d1
	cmpi.b	#'9',d1
	bls	its_hex
	addq.b	#7,d1
its_hex:
	move.b	d1,-(a1)
	lsr.w	#4,d0
	dbra	d4,gh_lp
	rts

get_bcd:			*値→16進数文字列(2bytes)マイナス値も生成
	* < d0=data value
	* < a1=格納したいアドレス
	* > (a1)=ascii numbers
	* X d0,d1,d4,a1
	tst.w	d0
	smi	pl_mi
	bpl	chk_big
	neg.w	d0
chk_big:
	cmpi.w	#9999,d0
	bls	clr_hjm
	move.l	#9999,d0
clr_hjm:
	clr.b	hajimete
	move.l	#1000,d1
gbcd_lp:
	divu	d1,d0
	move.b	d0,d4
	clr.w	d0
	swap	d0
	tst.b	d4
	bne	set_1bcd
	tst.b	hajimete
	bne	set_1bcd
	move.b	#$20,(a1)+
	bra	lp_ope
set_1bcd:
	tst.b	hajimete
	bne	add_0
	tst.b	pl_mi
	bpl	reset_hjm
	move.b	#'-',-(a1)
	addq.w	#1,a1
reset_hjm:
	st	hajimete
add_0:
	add.b	#'0',d4
	move.b	d4,(a1)+
lp_ope:
	divu	#10,d1
	tst.w	d1
	bne	gbcd_lp
	tst.b	hajimete
	bmi	bye_gbcd
	move.b	#'0',-1(a1)
bye_gbcd:
	rts

get_hex32:			*値→16進数文字列(4bytes)
	* < d0=data value
	* < a1=格納したいアドレス
	* > (a1)=ascii numbers
	* X d0,d1,d4,a1
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

kake:			*32ビット×32ビット=32ビット
	* < d0.l X d1.l
	* > d1.l
	* X d0-d4
	movem.l	d0/d2-d4,-(sp)
	move.l	d0,d2
	move.l	d1,d3
	swap	d2
	swap	d3

	move.w	d1,d4
	mulu	d0,d1
	mulu	d3,d0
	mulu	d2,d3
	mulu	d4,d2
	exg	d0,d3

	add.l	d3,d2

	move.w	d2,d3
	swap	d3
	clr.w	d3

	clr.w	d2
	addx.w	d3,d2
	swap	d2

	add.l	d3,d1
	addx.l	d2,d0
	movem.l	(sp)+,d0/d2-d4
	rts

wari:				*32ﾋﾞｯﾄ/32ﾋﾞｯﾄ=32ﾋﾞｯﾄ...32ﾋﾞｯﾄ
	* < d1.l/d2.l=d1.l ...d2.l
	cmpi.l	#$ffff,d2
	bls	divx		*16ビット以下の数値なら１命令で処理
	cmp.l	d1,d2
	beq	div01		*d1=d2商は１
	bls	div02		*１命令では無理なケース

	move.l	d1,d2		*商は０余りはd1.l
	moveq.l	#0,d1
	rts
div01:				*商は１余り０
	moveq.l	#1,d1
	moveq.l	#0,d2
	rts
div02:
	movem.l	d3-d5,-(sp)
	move.l	d2,d3
	clr.w	d3
	swap	d3
	addq.l	#1,d3
	move.l	d1,d4
	move.l	d2,d5
	move.l	d3,d2
	bsr	divx
	move.l	d5,d2
	divu	d3,d2
	divu	d2,d1
	andi.l	#$ffff,d1
div03:
	move.l	d5,d2
	move.l	d5,d3
	swap	d3
	mulu	d1,d2
	mulu	d1,d3
	swap	d3
	add.l	d3,d2
	sub.l	d4,d2
	bhi	div04
	neg.l	d2
	cmp.l	d2,d5
	bhi	div05
	addq.l	#1,d1
	bra	div03
div04:
	subq.l	#1,d1
	bra	div03
div05:
	movem.l	(sp)+,d3-d5
	rts
divx:
	movem.w	d1/d3,-(sp)
	clr.w	d1
	swap	d1
	divu	d2,d1
	move.w	d1,d3
	move.w	(sp)+,d1
	divu	d2,d1
	swap	d1
	moveq.l	#0,d2
	move.w	d1,d2
	move.w	d3,d1
	swap	d1
	move.w	(sp)+,d3
	rts

bank_change:				*アクセスバンクの切り換え
	moveq.l	#0,d7
	not.b	bank_sel
	bsr	_bank_a
	bsr	_bank_b
	bra	m_lp
bank_change_a:
	st	bank_sel
	bra	bank_change
bank_change_b:
	clr.b	bank_sel
	bra	bank_change

M_ON:					*MONITOR機能のオン／オフ
	moveq.l	#0,d7

	move.w	sr,-(sp)
	ori.w	#$0700,sr
	move.b	#$10,CCR3	*dma stop
	st	CSR3		*status clear
	move.b	#$88,$e92003
	clr.b	DMADSTAT.w
	move.w	(sp)+,sr

*	bsr	set_pan_frq

	move.b	monitor_mode(pc),d0
	not.b	d0			*0=off 1=on
	move.b	d0,-(sp)
	beq	Moff
Mon:
	move.b	#%0000_0100,$e92001	*adpcm rec start
	bra	M01
Moff:
	move.b	#%0000_0001,$e92001	*adpcm all end
M01:
	move.b	(sp)+,d0
	bsr	mt_on_off
	bra	complete

_OUT:					*パンポット
	moveq.l	#0,d7

	moveq.l	#0,d0
	move.b	out_assign(pc),d0
	addq.b	#1,d0
	cmpi.b	#4,d0
	bne	ok_pan
	moveq.l	#0,d0
ok_pan:
	move.b	d0,last_param+1			*ワークはＸＡＰＮＥＬ部と共用
	bsr	pan_pot
	bsr	set_pan_frq
	bra	complete

_AUTO_:					*オートサンプリング機能の有無
	moveq.l	#0,d7
	move.b	trigger(pc),d0
	not.b	d0
	bsr	trg_on_off
	move.b	trigger(pc),d0
	beq	complete
	clr.b	monitor_mode
	bra	M_ON

_chg_type:				*表示データの形式を変える
	moveq.l	#0,d7
	move.b	data_type(pc),d0
	not.b	d0
	bsr	dt_type
	bsr	get_pt1
	bsr	chg_tp_a
	bsr	chg_tp_b
	bsr	put_pt1
	bra	complete

dsp_cv_mes:
	movem.l	d0-d4/a1,-(sp)
	display2	cv_mes,%1110,41,15,14
	movem.l	(sp)+,d0-d4/a1
	rts

chg_tp_a:				*adpcm data -> pcm data(初めての時)
	tst.b	data_a
	beq	ext_chg_tp		*データエリアに何もない…
	tst.b	data_type
	bne	ad_ready_a?
	move.l	buffer1(pc),d0
	bne	hosei_a			*pcmデータはある
	bsr	dsp_cv_mes
	bsr	make_pcm_a
	bmi	hosei_a			*case:error
*	movem.l	d0-d7/a0-a6,-(sp)
*	move.l	size1(pc),d6
*	movea.l	buffer1(pc),a6
*	move.w	#scr_wy,genten_y
*	bsr	waku1
*	bsr	dsp_wv
*	movem.l	(sp)+,d0-d7/a0-a6
hosei_a:
	bsr	calc_es_pcm_a
	bsr	init_hex_area_a
	rts
ad_ready_a?:			*pcm data をadpcm dataへ(初めての時)
	move.l	address1(pc),d0
	bne	adrd_a0		*両方あるよ
	bsr	dsp_cv_mes
	bsr	make_adpcm_a
*	bmi	ext_chg_tp		*case:error
adrd_a0:
	bsr	calc_es_adpcm_a
	bsr	init_hex_area_a
	rts

chg_tp_b:			*pcm -> adpcm (初めての時)
	tst.b	data_b
	beq	ext_chg_tp	*データエリアに何もない…
	tst.b	data_type
	bne	ad_ready_b?
	move.l	buffer2(pc),d0
	bne	hosei_b			*pcmデータはある
	bsr	dsp_cv_mes
	bsr	make_pcm_b
	bmi	hosei_b			*case:error
*	movem.l	d0-d7/a0-a6,-(sp)
*	move.l	size2(pc),d6
*	movea.l	buffer2(pc),a6
*	move.w	#scr_wy+plus_,genten_y
*	bsr	waku2
*	bsr	dsp_wv
*	movem.l	(sp)+,d0-d7/a0-a6
hosei_b:
	bsr	calc_es_pcm_b
	bsr	init_hex_area_b
ext_chg_tp:
	rts

ad_ready_b?:			*pcm dataをadpcm dataへ(初めての時)
	move.l	address2(pc),d0
	bne	adrd_b0		*両方あるよ
	bsr	dsp_cv_mes
	bsr	make_adpcm_b
*	bmi	ext_chg_tp		*case:error
adrd_b0:
	bsr	calc_es_adpcm_b
	bsr	init_hex_area_b
	bra	ext_chg_tp

calc_es_pcm_a:			*end_point,start_pointの計算
	movem.l	d0-d1,-(sp)
	move.l	start_point1(pc),d0
	beq	cepa1
	move.l	address1(pc),d1
	beq	cepa1
	sub.l	d1,d0
	lsl.l	#2,d0
	move.l	buffer1(pc),d1
	beq	cepa1
	add.l	d1,d0
	move.l	d0,start_point1

	move.l	end_point1(pc),d0
	sub.l	address1(pc),d0
	lsl.l	#2,d0
	add.l	buffer1(pc),d0
	move.l	d0,end_point1
cepa1:
	movem.l	(sp)+,d0-d1
	rts
calc_es_adpcm_a:		*start_point,end_pintの計算
	movem.l	d0-d1,-(sp)
	move.l	start_point1(pc),d0
	beq	ceaa1
	move.l	buffer1(pc),d1
	beq	ceaa1
	sub.l	d1,d0
	lsr.l	#2,d0
	move.l	address1(pc),d1
	beq	ceaa1
	add.l	d1,d0
	move.l	d0,start_point1

	move.l	end_point1(pc),d0
	sub.l	buffer1(pc),d0
	lsr.l	#2,d0
	add.l	address1(pc),d0
	move.l	d0,end_point1
ceaa1:
	movem.l	(sp)+,d0-d1
	rts
calc_es_pcm_b:
	movem.l	d0-d1,-(sp)
	move.l	start_point2(pc),d0
	beq	cepb1
	move.l	address2(pc),d1
	beq	cepb1
	sub.l	d1,d0
	lsl.l	#2,d0
	move.l	buffer2(pc),d1
	beq	cepb1
	add.l	d1,d0
	move.l	d0,start_point2

	move.l	end_point2(pc),d0
	sub.l	address2(pc),d0
	lsl.l	#2,d0
	add.l	buffer2(pc),d0
	move.l	d0,end_point2
cepb1:
	movem.l	(sp)+,d0-d1
	rts
calc_es_adpcm_b:		*start_point,end_pintの計算
	movem.l	d0-d1,-(sp)
	move.l	start_point2(pc),d0
	beq	ceab1
	move.l	buffer2(pc),d1
	beq	ceab1
	sub.l	d1,d0
	lsr.l	#2,d0
	move.l	address2(pc),d1
	beq	ceab1
	add.l	d1,d0
	move.l	d0,start_point2

	move.l	end_point2(pc),d0
	sub.l	buffer2(pc),d0
	lsr.l	#2,d0
	add.l	address2(pc),d0
	move.l	d0,end_point2
ceab1:
	movem.l	(sp)+,d0-d1
	rts

_chg_rate:				*サンプリングレートの設定
	moveq.l	#0,d7
	moveq.l	#0,d0
	move.b	sampling_rate(pc),d0
	addq.b	#1,d0
	cmpi.b	#5,d0
	bne	ok_rate
	moveq.l	#0,d0
ok_rate:
	move.b	d0,-(sp)
	lsl.w	d0
	lea	rate_tbl(pc),a0
	move.w	(a0,d0.w),d0		*freq
	move.l	rec_data_size(pc),d1
	add.l	d1,d1			*d1=d1*2(sz=sz*2)
	divu	d0,d1			*d1=time
	bne	disp_st
	moveq.l	#1,d1			*１秒以下は皆１秒とする
disp_st:
	move.b	d1,d0
	andi.l	#$ff,d0
	bsr	rec_time
	move.b	(sp)+,d0
	move.b	d0,last_param		*ワークはＸＡＰＮＥＬ部と共用
	bsr	smp_rate
	bsr	set_pan_frq
	bra	complete

input_smpt:				*input smapling time
	bsr	del_point
	bsr	clr_bottom
	display2	smp_tm,%1111,0,28,13
	IOCS	_OS_CURON
	display2	input_smpt_mes,%0111,0,29,41
i_t_again:
	moveq.l	#0,d1
	moveq.l	#30,d2
	IOCS	_B_LOCATE
	IOCS	_B_CLR_ST
	display2	sec,%0111,5,30,6
	moveq.l	#0,d1
	moveq.l	#30,d2
	IOCS	_B_LOCATE
	lea	input_buffer(pc),a1
	move.b	#4,(a1)
	pea	(a1)
	DOS	_GETS			*一行入力
	addq.w	#4,sp
	addq.w	#2,a1
	bsr	get_num			*d1=秒数
	tst.l	d1
	beq	cansel__
	bmi	cansel__
	moveq.l	#0,d0
	move.b	sampling_rate(pc),d0
	lsl.w	d0
	lea	rate_tbl(pc),a0
	move.w	(a0,d0.w),d0
	mulu	d1,d0
	lsr.l	d0			*d0=必要バイト数
	cmpi.l	#max_size,d0
	bhi	i_t_again
	move.l	d0,rec_data_size
	move.w	d1,recording_time
cansel__:
	IOCS	_OS_CUROF
	bsr	scr_init
	bra	complete

input_smsz:				*input smapling data size
	bsr	del_point
	bsr	clr_bottom
	display2	smp_sz,%1111,0,28,13
	IOCS	_OS_CURON
	display2	input_smsz_mes,%0111,0,29,42
i_t_again_sz:
	moveq.l	#0,d1
	moveq.l	#30,d2
	IOCS	_B_LOCATE
	IOCS	_B_CLR_ST
	display2	bytes,%0111,9,30,10
	moveq.l	#0,d1
	moveq.l	#30,d2
	IOCS	_B_LOCATE
	lea	input_buffer(pc),a1
	move.b	#8,(a1)
	pea	(a1)
	DOS	_GETS			*一行入力
	addq.w	#4,sp
	addq.w	#2,a1			*chr data buffer
	bsr	get_num			*d1=秒数
	tst.l	d1
	beq	cansel_
	bmi	cansel_
	cmpi.l	#max_size,d1
	bhi	i_t_again_sz
	moveq.l	#0,d0
	move.b	sampling_rate(pc),d0
	lsl.w	d0
	lea	rate_tbl(pc),a0
	move.w	(a0,d0.w),d0	*freq
	move.l	d1,d2
	add.l	d2,d2			*d2=d2*2(sz=sz*2)
	divu	d0,d2			*d2=time
	bne	set_dt_wk
	moveq.l	#1,d2			*１秒以下は皆１秒とする
set_dt_wk:
	move.l	d1,rec_data_size
	move.w	d2,recording_time
cansel_:
	IOCS	_OS_CUROF
	bsr	scr_init
	bra	complete

get_num:				*キャラクターコードから数値へ
	* < (a1)=chr
	* > d1=number
	* > (a1)=next chr
	*
	cmpi.b	#'+',(a1)
	sne	pl_mi
	beq	skip_sgn
	cmpi.b	#'-',(a1)
	seq	pl_mi		*'-'ならマーク
	bne	get_num0
skip_sgn:
	addq.w	#1,a1
get_num0:
	moveq.l	#0,d1
	moveq.l	#0,d0
num_lp01:
	move.b	(a1)+,d0
	sub.b	#$30,d0
	bmi	num_exit
	cmp.b	#9,d0
	bhi	num_exit

	add.l	d1,d1
	move.l	d1,-(sp)
	lsl.l	#2,d1
	add.l	(sp)+,d1	*d1=d1*10
	add.l	d0,d1		*d1=d1+d0
	bra	num_lp01
num_exit:
	tst.b	pl_mi
	beq	num_exit_
	neg.l	d1
num_exit_:
	rts

adjust_frq:			*周波数の微調整
	movem.l	d0-d7/a0-a6,reg_buff2
	move.b	data_a(pc),d0
	or.b	data_b(pc),d0
	beq	ext_ajfr

	bsr	clr_bottom
	display2	adj_frq,%1111,0,28,16
	display2	adj_mes1,%0111,0,29,31
	display2	Hz,%0111,6,30,4
	moveq.l	#0,d1
	moveq.l	#30,d2
	IOCS	_B_LOCATE
	IOCS	_OS_CURON

	lea	input_buffer(pc),a1
	move.b	#5,(a1)		*入力文字数
	pea	(a1)
	DOS	_GETS		*一行入力
	addq.w	#4,sp

	addq.w	#2,a1
	IOCS	_OS_CUROF

	cmpi.b	#$20,(a1)
	bls	ext_ajfr	*スペースよりも小さい時はキャンセル

	bsr	get_num
	move.l	d1,d6		*d6=ソース周波数
	beq	ext_ajfr
	bmi	ext_ajfr	*数が０やマイナスならキャンセル
	cmpi.l	#65535,d6
	bhi	ext_ajfr

	bsr	clr_bottom
	display2	adj_frq,%1111,0,28,16
	display2	input_buffer+2,%0011,17,28,5
	display2	yajirushi,%0111,22,28,7
	display2	adj_mes2,%0111,0,29,36
	display2	Hz,%0111,6,30,4
	moveq.l	#0,d1
	moveq.l	#30,d2
	IOCS	_B_LOCATE
	IOCS	_OS_CURON

	lea	input_buffer(pc),a1
	move.b	#5,(a1)		*入力文字数
	pea	(a1)
	DOS	_GETS		*一行入力
	addq.w	#4,sp

	addq.w	#2,a1
	IOCS	_OS_CUROF

	cmpi.b	#$20,(a1)
	bls	ext_ajfr	*スペースよりも小さい時はキャンセル

	bsr	get_num
	move.l	d1,d7
	beq	ext_ajfr
	bmi	ext_ajfr	*数が０やマイナスならキャンセル
	cmpi.l	#65535,d7
	bhi	ext_ajfr
	cmp.l	d6,d7
	beq	ext_ajfr	*変換が無意味

	display2	input_buffer+2,%0011,28,28,5
	display2	Hz,%0111,33,28,4
	moveq.l	#0,d1
	moveq.l	#29,d2
	IOCS	_B_LOCATE
	IOCS	_B_CLR_ST

	bsr	get_pt1
	display2	cv_mes,%1110,41,15,14
				*実際に作業を行う…
	lea	address1(pc),a0
	tst.b	bank_sel
	beq	ajfr1

	tst.b	data_b		*bank_bの下準備
	beq	ext_ajfr
	lea	20(a0),a0		*bank_b base addr.
	tst.l	pbuffer(a0)
	bne	ajfr0
	bsr	make_pcm_b
	bmi	ext_ajfr
ajfr0:
	tst.b	data_type
	beq	ajfr2
	bsr	calc_es_pcm_b
	bra	ajfr2
ajfr1:				*bank_aの下準備
	tst.b	data_a
	beq	ext_ajfr
	tst.l	pbuffer(a0)
	bne	ajfr11
	bsr	make_pcm_a
	bmi	ext_ajfr
ajfr11:
	tst.b	data_type
	beq	ajfr2
	bsr	calc_es_pcm_a
ajfr2:
	movea.l	pstart(a0),a2	*a2=pcm start addr.
	move.l	pend(a0),d4
	sub.l	a2,d4
	addq.l	#2,d4
	lsr.l	d4		*d4=number of data
				*テンポラリエリア確保
	move.l	#-1,-(sp)
	DOS	_MALLOC
	addq.w	#4,sp
	andi.l	#$00ffffff,d0
	move.l	d0,d5
	move.l	d0,-(sp)	*取れる限り確保
	DOS	_MALLOC
	addq.w	#4,sp
	move.l	d0,mm_error
	bsr	print_em
	bmi	ext_ajfr
	move.l	d0,a1		*destination
	move.l	d0,work1
	add.l	a1,d5
	bsr	do_ajfr
	bsr	print_em
	bmi	ext_ajfr
	bra	pchg_endope

do_ajfr:
	* < d4.l=data count
	* < d5.l=mem.block end address
	* < d6.w=source frq
	* < d7.w=destination frq
	* < a1.l=destination pcm data address
	* < a2.l=source pcm data address
	* > a1.l=destination pcm data size
	clr.l	mm_error	*error init
	exg.l	d6,d7
	divu	d6,d7
	move.w	d7,d1		*d1=step
	clr.w	d7
	divu	d6,d7
	swap	d7
	tst.w	d7
	beq	@f
	add.l	#$0001_0000,d7
	clr.w	d7
@@:
	swap	d7		*d7=revise
	moveq.l	#0,d3
	tst.w	d1
	bne	nz_d1
doa_lp00:
	move.w	(a2)+,d0
	add.w	d7,d3
	bcc	@f
	move.w	d0,(a1)+
	cmp.l	a1,d5
	bhi	@f
	st.b	mm_error
	bra	exit_doa_lp01
@@:
	subq.l	#1,d4
	bne	doa_lp00
	bra	exit_doa
nz_d1:
	move.w	d1,d6
	move.w	(a2)+,d0
	move.w	(a2),d2
	cmp.l	pend(a0),a2
	bls	@f
	move.w	d0,d2
@@:
	add.w	d7,d3
	bcc	@f
	addq.w	#1,d6
@@:
	ext.l	d0
	ext.l	d2
	movem.l	d1/d3,-(sp)
	sub.l	d0,d2
	divs	d6,d2
	move.w	d2,d1		*d1=step
	clr.w	d2
	divu	d6,d2
	swap	d2
	tst.w	d2
	beq	@f
	add.l	#$0001_0000,d2
	clr.w	d2
@@:
	swap	d2		*d2=revise
	moveq.l	#0,d3
doa_lp01:
	move.w	d0,(a1)+
	cmp.l	a1,d5
	bhi	@f
	st.b	mm_error
	bra	exit_doa_lp01
@@:
	add.w	d1,d0
	add.w	d2,d3
	bcc	@f
	tst.w	d1
	bpl	doa_pls
	subq.w	#1,d0
	bra	@f
doa_pls:
	addq.w	#1,d0
@@:
	subq.w	#1,d6
	bne	doa_lp01
exit_doa_lp01:
	movem.l	(sp)+,d1/d3
	tst.l	mm_error
	bmi	exit_doa_
	subq.l	#1,d4
	bne	nz_d1
exit_doa:
	sub.l	work1(pc),a1
exit_doa_:
	rts

portament:			*ポルタメント
	movem.l	d0-d7/a0-a6,reg_buff2
	move.b	data_a(pc),d0
	or.b	data_b(pc),d0
	beq	ext_bend

	bsr	clr_bottom
	display2	bnd_frq,%1111,0,28,16
	display2	adj_mes1,%0111,0,29,31
	display2	Hz,%0111,6,30,4
	moveq.l	#0,d1
	moveq.l	#30,d2
	IOCS	_B_LOCATE
	IOCS	_OS_CURON

	lea	input_buffer(pc),a1
	move.b	#5,(a1)		*入力文字数
	pea	(a1)
	DOS	_GETS		*一行入力
	addq.w	#4,sp

	addq.w	#2,a1
	IOCS	_OS_CUROF

	cmpi.b	#$20,(a1)
	bls	ext_bend	*スペースよりも小さい時はキャンセル

	bsr	get_num
	move.l	d1,d6		*d6=ソース周波数
	beq	ext_bend
	bmi	ext_bend	*数が０やマイナスならキャンセル
	cmpi.l	#65535,d6
	bhi	ext_bend

	bsr	clr_bottom
	display2	bnd_frq,%1111,0,28,16
	display2	input_buffer+2,%0011,17,28,5
	display2	yajirushi,%0111,22,28,7
	display2	adj_mes2,%0111,0,29,36
	display2	Hz,%0111,6,30,4
	moveq.l	#0,d1
	moveq.l	#30,d2
	IOCS	_B_LOCATE
	IOCS	_OS_CURON

	lea	input_buffer(pc),a1
	move.b	#5,(a1)		*入力文字数
	pea	(a1)
	DOS	_GETS		*一行入力
	addq.w	#4,sp

	addq.w	#2,a1
	IOCS	_OS_CUROF

	cmpi.b	#$20,(a1)
	bls	ext_bend	*スペースよりも小さい時はキャンセル

	bsr	get_num
	move.l	d1,d7
	beq	ext_bend
	bmi	ext_bend	*数が０やマイナスならキャンセル
	cmpi.l	#65535,d7
	bhi	ext_bend
	cmp.l	d6,d7
	beq	ext_bend	*変換が無意味

	display2	input_buffer+2,%0011,28,28,5
	display2	Hz,%0111,33,28,4
	moveq.l	#0,d1
	moveq.l	#29,d2
	IOCS	_B_LOCATE
	IOCS	_B_CLR_ST

	bsr	get_pt1
	display2	cv_mes,%1110,41,15,14
				*実際に作業を行う…
	lea	address1(pc),a0
	tst.b	bank_sel
	beq	bend1

	tst.b	data_b		*bank_bの下準備
	beq	ext_bend
	lea	20(a0),a0		*bank_b base addr.
	tst.l	pbuffer(a0)
	bne	bend0
	bsr	make_pcm_b
	bmi	ext_bend
bend0:
	tst.b	data_type
	beq	bend2
	bsr	calc_es_pcm_b
	bra	bend2
bend1:				*bank_aの下準備
	tst.b	data_a
	beq	ext_bend
	tst.l	pbuffer(a0)
	bne	bend11
	bsr	make_pcm_a
	bmi	ext_bend
bend11:
	tst.b	data_type
	beq	bend2
	bsr	calc_es_pcm_a
bend2:
	movea.l	pstart(a0),a2	*a2=pcm start addr.
	move.l	pend(a0),d4
	sub.l	a2,d4
	addq.l	#2,d4
	lsr.l	d4		*d4=number of data
				*テンポラリエリア確保
	move.l	#-1,-(sp)
	DOS	_MALLOC
	addq.w	#4,sp
	andi.l	#$00ffffff,d0
	move.l	d0,d5
	move.l	d0,-(sp)	*取れる限り確保
	DOS	_MALLOC
	addq.w	#4,sp
	move.l	d0,mm_error
	bsr	print_em
	bmi	ext_bend
	move.l	d0,a1		*destination
	move.l	d0,work1
	add.l	a1,d5
	bsr	do_autobend
	bsr	print_em
	bmi	ext_bend
	bra	pchg_endope

do_autobend:
	* < d4.l=data count
	* < d5.l=mem.block end address
	* < d6.w=source frq
	* < d7.w=destination frq
	* < a1.l=destination pcm data address
	* < a2.l=source pcm data address
	* > a1.l=destination pcm data size
	* X d0-d7
	clr.l	mm_error	*error init
	exg.l	d6,d7
	move.l	d6,atb_frqsrc
	move.l	d6,atb_frqnow
				*周波数変化率計算
	move.l	d4,d1
	move.l	d7,d0
	sub.l	d6,d0
	bpl	@f
	neg.l	d0
	bsr	wari2		*d0.l/d1.l=d0.l...d1.l
	neg.l	d0
	move.l	d0,atb_step		*周波数変化率
	move.l	#-1,atb_sgn
	bra	atb0
@@:
	bsr	wari2		*d0.l/d1.l=d0.l...d1.l
	move.l	d0,atb_step		*周波数変化率
	move.l	#1,atb_sgn
atb0:
	swap	d1
	clr.w	d1
	move.l	d1,d0
	move.l	d4,d1
	bsr	wari2		*d0.l/d1.l=d0.l...d1.l
	tst.l	d1
	beq	@f
	addq.l	#1,d0		*revise
@@:
	move.w	d0,atb_rvs

	bsr	calc_frqchgrate

	moveq.l	#0,d3
	move.w	d3,atb_rvswk

	tst.b	atb_sgn
	bpl	nz_d1_b
doa_lp00_b:
	move.w	(a2)+,d0
	add.w	d7,d3
	bcc	@f
	move.w	d0,(a1)+
	cmp.l	a1,d5
	bhi	@f
	st.b	mm_error
	bra	exit_doa_lp01_b
@@:
	bsr	calc_frqchgrate
	subq.l	#1,d4
	bne	doa_lp00_b
	bra	exit_doa_b
nz_d1_b:
	move.w	d1,d6
	move.w	(a2)+,d0
	move.w	(a2),d2
	cmpi.l	#1,d4		*最後かどうか
	bne	@f
	move.w	d0,d2
@@:
	add.w	d7,d3
	bcc	@f
	addq.w	#1,d6
@@:
	ext.l	d0
	ext.l	d2
	movem.l	d1/d3,-(sp)
	sub.l	d0,d2
	divs	d6,d2
	move.w	d2,d1		*d1=step
	clr.w	d2
	divu	d6,d2
	swap	d2
	tst.w	d2
	beq	@f
	add.l	#$0001_0000,d2
	clr.w	d2
@@:
	swap	d2		*d2=revise
	moveq.l	#0,d3
doa_lp01_b:
	move.w	d0,(a1)+
	cmp.l	a1,d5
	bhi	@f
	st.b	mm_error
	bra	exit_doa_lp01_b
@@:
	add.w	d1,d0
	add.w	d2,d3
	bcc	@f
	tst.w	d1
	bpl	doa_pls_b
	subq.w	#1,d0
	bra	@f
doa_pls_b:
	addq.w	#1,d0
@@:
	subq.w	#1,d6
	bne	doa_lp01_b
exit_doa_lp01_b:
	movem.l	(sp)+,d1/d3
	tst.l	mm_error
	bmi	exit_doa_b_
	bsr	calc_frqchgrate
	subq.l	#1,d4
	bne	nz_d1_b
exit_doa_b:
	sub.l	work1(pc),a1
exit_doa_b_:
	rts

calc_frqchgrate:			*変換パラメータの計算
	movem.l	atb_frqsrc(pc),d6-d7
	add.l	atb_step(pc),d7
	move.w	atb_rvs(pc),d1
	add.w	d1,atb_rvswk
	bcc	@f
	add.l	atb_sgn(pc),d7
@@:
	move.l	d7,atb_frqnow

	divu	d6,d7
	move.w	d7,d1		*d1=step
	clr.w	d7
	divu	d6,d7
	swap	d7
	tst.w	d7
	beq	@f
	add.l	#$0001_0000,d7
	clr.w	d7
@@:
	swap	d7		*d7=revise
	rts

level_chg:			*ボリュームの変更
	movem.l	d0-d7/a0-a6,reg_buff2
	move.b	data_a(pc),d0
	or.b	data_b(pc),d0
	beq	ext_lvch

	bsr	clr_bottom
	display2	lvch_mes,%1111,0,28,13
	display2	lvch_mes2,%0111,0,29,58
	display2	_pcnt,%0111,3,30,1
	moveq.l	#0,d1
	moveq.l	#30,d2
	IOCS	_B_LOCATE
	IOCS	_OS_CURON

	lea	input_buffer(pc),a1
	move.b	#3,(a1)		*入力文字数
	pea	(a1)
	DOS	_GETS		*一行入力
	addq.w	#4,sp

	addq.w	#2,a1
	IOCS	_OS_CUROF

	cmpi.b	#$20,(a1)
	bls	ext_lvch	*スペースよりも小さい時はキャンセル

	bsr	get_num
	tst.l	d1
	beq	ext_lvch
	bmi	ext_lvch	*数が０やマイナスならキャンセル

	move.l	d1,d6		*d6=パーセンテージ

	bsr	get_pt1
	display2	cv_mes,%1110,41,15,14
				*実際に作業を行う…
	lea	address1(pc),a0
	tst.b	bank_sel
	beq	lvch1

	tst.b	data_b		*bank_bの下準備
	beq	ext_lvch
	lea	20(a0),a0		*bank_b base addr.
	tst.l	pbuffer(a0)
	bne	lvch0
	bsr	make_pcm_b
	bmi	ext_lvch
lvch0:
	tst.b	data_type
	beq	lvch2
	bsr	calc_es_pcm_b
	bra	lvch2
lvch1:				*bank_aの下準備
	tst.b	data_a
	beq	ext_lvch
	tst.l	pbuffer(a0)
	bne	lvch11
	bsr	make_pcm_a
	bmi	ext_lvch
lvch11:
	tst.b	data_type
	beq	lvch2
	bsr	calc_es_pcm_a
lvch2:
	movea.l	pstart(a0),a1	*a1=pcm start addr.
	move.l	pend(a0),d0
	sub.l	a1,d0
	addq.l	#2,d0
	lsr.l	d0		*d0=number of data

	bsr	do_level	*pcm dataを実際に加工

atoshimatsu:
	tst.b	bank_sel
	bne	lvch3
				*bank_aの後始末
	tst.l	address1
	bne	lvch21		*既にadpcm dataがある
	bsr	make_adpcm_a	*adpcm dataが無かった
	bmi	ext_lvch
	bra	lvch22
lvch21:
	movea.l	pbuffer(a0),a1
	move.l	psize(a0),d0
	movea.l	paddress(a0),a0
	bsr	pcm_to_adpcm
lvch22:
	bsr	waku1
	movea.l	buffer1(pc),a6
	move.l	size1(pc),d6
	move.w	#scr_wy,genten_y
	bsr	dsp_wv
	tst.b	data_type
	beq	lvch23
	bsr	calc_es_adpcm_a
lvch23:
	bsr	init_hex_area_a
	bsr	put_pt1
	bra	ext_lvch

lvch3:				*bak_bの後始末
	tst.l	address2
	bne	lvch31
	bsr	make_adpcm_b
	bmi	ext_lvch
	bra	lvch32
lvch31:
	movea.l	pbuffer(a0),a1
	move.l	psize(a0),d0
	movea.l	paddress(a0),a0
	bsr	pcm_to_adpcm
lvch32:
	bsr	waku2
	movea.l	buffer2(pc),a6
	move.l	size2(pc),d6
	move.w	#scr_wy+plus_,genten_y
	bsr	dsp_wv
	tst.b	data_type
	beq	lvch33
	bsr	calc_es_adpcm_b
lvch33:
	bsr	init_hex_area_b
	bsr	put_pt1
ext_ajfr:
ext_bend:
ext_lvch:
ext_efct:
	bra	non_ope

do_level:			*パーセンテージを１６進変換
	* < d0=data count
	* < a1=pcm data address
	* < d6=percentage(1%～300%)
	movem.l	d0/d2/d6/a1,-(sp)
	lsl.l	#7,d6		*d6=d6*128
	divu	#100,d6
	andi.l	#$ffff,d6	*d6=0～200→0～256
lvch_lp:
	moveq.l	#0,d2
	move.w	(a1),d2

	muls	d6,d2		*d6(0-256)にはパーセントが
	asr.l	#7,d2		*入っている
*	bsr	chk_ovf		*over flow check

	move.w	d2,(a1)+	*store pcm data to buffer

	subq.l	#1,d0
	bne	lvch_lp
	movem.l	(sp)+,d0/d2/d6/a1
	rts

pitch_chg:			*ピッチの変更
	movem.l	d0-d7/a0-a6,reg_buff2
	move.b	data_a(pc),d0
	or.b	data_b(pc),d0
	beq	ext_lvch
	bsr	clr_bottom
	display2	ptch_mes,%1111,0,28,13
	display2	ptch_mes2,%0111,0,29,51
	display2	_ksft,%0111,3,30,12
	moveq.l	#0,d1
	moveq.l	#30,d2
	IOCS	_B_LOCATE
	IOCS	_OS_CURON

	lea	input_buffer(pc),a1
	move.b	#3,(a1)		*入力文字数
	pea	(a1)
	DOS	_GETS		*一行入力
	addq.w	#4,sp

	addq.w	#2,a1
	IOCS	_OS_CUROF

	cmpi.b	#$20,(a1)
	bls	ext_ptch	*スペースよりも小さい時はキャンセル

	bsr	get_num
	tst.l	d1
	beq	ext_ptch	*０はキャンセル
	move.l	d1,d6
	tst.l	d1
	bpl	chk_abs
	neg.l	d1
chk_abs:
	cmpi.b	#12,d1
	bhi	ext_ptch	*±12以上は無効とする

	bsr	get_pt1
	bsr	dsp_cv_mes
				*実際に作業を行う…
	lea	address1(pc),a0
	tst.b	bank_sel
	beq	ptch1

	tst.b	data_b		*bank_bの下準備
	beq	ext_ptch
	lea	20(a0),a0		*bank_b base addr.
	tst.l	pbuffer(a0)
	bne	ptch0
	bsr	make_pcm_b
	bmi	ext_ptch
ptch0:
	tst.b	data_type
	beq	ptch2
	bsr	calc_es_pcm_b
	bra	ptch2
ptch1:				*bank_aの下準備
	tst.b	data_a
	beq	ext_ptch
	tst.l	pbuffer(a0)
	bne	ptch11
	bsr	make_pcm_a
	bmi	ext_ptch
ptch11:
	tst.b	data_type
	beq	ptch2
	bsr	calc_es_pcm_a
ptch2:
	movea.l	pstart(a0),a2	*a2=pcm start addr.
	move.l	pend(a0),d1
	sub.l	a2,d1
	addq.l	#2,d1

	add.l	d1,d1		*取敢えず2倍
	move.l	d1,-(sp)	*コンバート先のメモリを確保
	DOS	_MALLOC
	addq.w	#4,sp
	move.l	d0,mm_error
	bsr	print_em
	bmi	ext_ptch	*case:error
	movea.l	d0,a1		*a1=destination address
	move.l	d0,work1

	lsr.l	#2,d1
	move.l	d1,d0		*d0=number of data

	add.w	#12,d6
	add.w	d6,d6
	cmpi.b	#$18,d6
	bcs	_shift_down		*ピッチを下げるケース
*shift_up				*ピッチを上げるケース
	cmpi.l	#8,d0		*サイズが小さすぎる
	bls	ext_ptch
	lea	frq_tbl,a5
	move.w	-26(a5,d6.w),d4
	move.b	#1,d6		*set switch
	bra	bsr_do_pitch
_shift_down:
	lea	frq_tbl2(pc),a5
	move.w	(a5,d6.w),d4
	st	d6		*set switch
bsr_do_pitch:
	bsr	do_pitch	*実際にデータを加工(out:a1=dest data size)
pchg_endope:
	* < a1.l=destination pcm data size
	move.l	a1,d3		*d3=size too

	move.l	d3,-(sp)		*new len
	move.l	work1(pc),-(sp)		*memptr
	DOS	_SETBLOCK		*pcm bufferの縮小
	addq.w	#8,sp

	move.l	psize(a0),d4
	lsl.l	#2,d4
	add.l	pbuffer(a0),d4	*d4=end addr.
	sub.l	pend(a0),d4
	subq.l	#2,d4		*use later
	move.l	d4,d5		*d4=d5=rear pcm data size

	move.l	pstart(a0),d1
	sub.l	pbuffer(a0),d1	*d1=front pcm size

	add.l	d3,d5
	add.l	d1,d5		*d5=new pcm data size
	move.l	d5,-(sp)
	DOS	_MALLOC
	addq.w	#4,sp
	move.l	d0,mm_error
	bsr	print_em
	bmi	ext_ptch_	*case:error
	move.l	d0,work2

	movea.l	d0,a2		*dest. addr
	move.l	d1,d2		*d2=trans. size
	moveq.l	#%0000_0101,d1
	movea.l	pbuffer(a0),a1	*source
	bsr	trans_dma	*先頭部の転送

	adda.l	d2,a2		*dest. addr
	move.l	work1(pc),a1	*source
	move.l	d3,d2		*size
	bsr	trans_dma	*コンバートして出来たデータの転送
	move.l	a2,pstart(a0)	*new start point

	adda.l	d2,a2		*dest. addr.
	move.l	pend(a0),a1	*source
	addq.w	#2,a1
	move.l	d4,d2		*size
	bsr	trans_dma	*後部の転送
	subq.w	#2,a2
	move.l	a2,pend(a0)	*new end point

	move.l	work1(pc),-(sp)		*ワーク開放
	DOS	_MFREE
	addq.w	#4,sp

	move.l	pbuffer(a0),-(sp)	*pcm buffer開放
	DOS	_MFREE
	addq.w	#4,sp

	move.l	paddress(a0),d0
	beq	reduce_mem
	move.l	d0,-(sp)		*adpcm buffer開放
	DOS	_MFREE
	addq.w	#4,sp
reduce_mem:
*	move.l	d3,-(sp)		*new len
*	move.l	work2(pc),-(sp)		*memptr
*	DOS	_SETBLOCK		*pcm bufferの縮小
*	addq.w	#8,sp

	lsr.l	#2,d5
	move.l	d5,psize(a0)		*new adpcm size
	move.l	work2,pbuffer(a0)	*new pcm addr.

				*adpcmに戻す作業
	tst.b	bank_sel
	bne	ptch3
				*bank_aの後始末
	bsr	make_adpcm_a	*adpcm dataが無かった
	bmi	ext_ptch
	bsr	waku1
	movea.l	buffer1(pc),a6
	move.l	size1(pc),d6
	move.w	#scr_wy,genten_y
	bsr	dsp_wv
	tst.b	data_type
	beq	ptch23
	bsr	calc_es_adpcm_a
ptch23:
	bsr	init_hex_area_a
	bsr	put_pt1
	bra	ext_ptch

ptch3:				*bak_bの後始末
	bsr	make_adpcm_b
	bmi	ext_ptch
	bsr	waku2
	movea.l	buffer2(pc),a6
	move.l	size2(pc),d6
	move.w	#scr_wy+plus_,genten_y
	bsr	dsp_wv
	tst.b	data_type
	beq	ptch33
	bsr	calc_es_adpcm_b
ptch33:
	bsr	init_hex_area_b
	bsr	put_pt1
ext_ptch:
	bra	non_ope
ext_ptch_:
	move.l	work1(pc),d0
	beq	ext_ptch
	move.l	d0,-(sp)
	DOS	_MFREE
	addq.w	#4,sp
	bra	non_ope

do_pitch:
	* < a2=source addr.
	* < a1=destination addr.
	* < d0=number of source data
	* < d4=shift parameter
 	* < d6=direction of pitch shift(-:shift down +:shift up)
	* > a1=destination size
	* - ALL
	movem.l	d0-d7/a0/a2-a6,-(sp)
	moveq.l	#0,d7
	moveq.l	#0,d3
	moveq.l	#0,d5

	pea	(a1)		*keep start address
cvt_lp:
	move.w	(a2)+,d2

	tst.b	d5	*初めて
	bne	_up_or_down
	seq.b	d5
	bra	_wrt_dst_pcm_dt
_up_or_down:
	tst.b	d6
	bmi	_do_shift_down

	move.w	d4,d1
	add.w	d1,d3
	bcc	_check_dsz
	bra	_wrt_dst_pcm_dt
_do_shift_down:
	move.w	d4,d1
	beq	@f
	add.w	d1,d3
	bcc	_wrt_dst_pcm_dt
@@:
	move.w	d2,d1
	add.w	d7,d1
	asr.w	d1		*線形補間
	move.w	d1,(a1)+	*store additional pcm data
_wrt_dst_pcm_dt:
	move.w	d2,(a1)+	*store pcm data to buffer
_check_dsz:
	move.w	d2,d7
	subq.l	#1,d0
	bne	cvt_lp
	suba.l	(sp)+,a1	*a1=size
	movem.l	(sp)+,d0-d7/a0/a2-a6
	rts

mixing:				*２つのデータをミックスする
	movem.l	d0-d7/a0-a6,reg_buff2
	clr.b	work2		*init work2
	clr.b	mix_mode	*set mix mode
	move.b	data_a(pc),d0
	and.b	data_b(pc),d0
	beq	ext_mix		*データが２つ無いならキャンセル

	bsr	clr_bottom
	lea	mix_mes(pc),a0
dsp_mix_mes:
	display2_	(a0),%1111,0,28,8
	display2	mix_mes2,%0111,0,29,22
	display2	mix_mes3,%0111,0,30,30
	moveq.l	#22,d1
	moveq.l	#29,d2
	IOCS	_B_LOCATE
	IOCS	_OS_CURON
	DOS	_INKEY
	cmpi.b	#'1',d0
	sne	work1		*memory direction
	beq	@f
	cmpi.b	#'2',d0
	bne	ext_mix
@@:
	IOCS	_OS_CUROF

	bsr	get_pt1
	display2	cv_mes,%1110,41,15,14
				*実際に作業を行う…
	tst.l	buffer2
	bne	@f
	move.b	#2,work2	*mark
	bsr	make_pcm_b
	bmi	ext_mix
@@:
	tst.b	data_type
	beq	@f
	bsr	calc_es_pcm_b
@@:				*bank_aの下準備
	tst.l	buffer1
	bne	@f
	bsr	make_pcm_a
	ori.b	#1,work2	*mark
	bmi	ext_mix
@@:
	tst.b	data_type
	beq	@f
	bsr	calc_es_pcm_a
@@:
	tst.b	work1		*direction?
	beq	case_atob
*case_btoa:			*BからAへ
	move.l	start_point2(pc),a1
	move.l	end_point2(pc),d1
	sub.l	a1,d1
	addq.l	#2,d1
	lsr.l	#2,d1

	move.l	buffer1(pc),a2
	move.l	size1(pc),d2

	move.l	start_point1(pc),d0
	sub.l	a2,d0
	lsr.l	#2,d0
	move.l	d0,offset

	bsr	mix_pcm

	bsr	waku1
	bsr	mfree_adpcm_a	*出来たデータを転送先へ
	bsr	mfree_pcm_a
	move.l	dest_size(pc),size1
	move.l	buffer3(pc),buffer1
	clr.l	start_point1
	clr.l	end_point1
	bsr	make_adpcm_a
	bmi	ext_mix
	move.l	buffer1(pc),a6
	move.l	size1(pc),d6
	move.w	#genten_a,genten_y
	bsr	dsp_wv

	btst.b	#1,work2(pc)
	beq	after_mix
	bsr	waku2
	move.l	buffer2(pc),a6
	move.l	size2(pc),d6
	move.w	#genten_b,genten_y
	bsr	dsp_wv
	bra	after_mix
case_atob:			*AからBへ
	move.l	start_point1(pc),a1
	move.l	end_point1(pc),d1
	sub.l	a1,d1
	addq.l	#2,d1
	lsr.l	#2,d1

	move.l	buffer2(pc),a2
	move.l	size2(pc),d2

	move.l	start_point2(pc),d0
	sub.l	a2,d0
	lsr.l	#2,d0
	move.l	d0,offset

	bsr	mix_pcm

	bsr	waku2
	bsr	mfree_adpcm_b	*出来たデータを転送先へ
	bsr	mfree_pcm_b
	move.l	dest_size(pc),size2
	move.l	buffer3(pc),buffer2
	clr.l	start_point2
	clr.l	end_point2
	bsr	make_adpcm_b
	bmi	ext_mix
	move.l	buffer2(pc),a6
	move.l	size2(pc),d6
	move.w	#genten_b,genten_y
	bsr	dsp_wv

	btst.b	#0,work2(pc)
	beq	after_mix
	bsr	waku1
	move.l	buffer1(pc),a6
	move.l	size1(pc),d6
	move.w	#genten_a,genten_y
	bsr	dsp_wv
after_mix:
	tst.b	data_type	*後始末
	beq	@f
	bsr	calc_es_adpcm_a
	bsr	calc_es_adpcm_b
@@:
	bsr	init_hex_area_a
	bsr	init_hex_area_b
	bsr	put_pt1
ext_mix:
	bra	non_ope

logic_ope:			*２つのデータの論理演算を行なう。
	movem.l	d0-d7/a0-a6,reg_buff2
	clr.b	work2		*init work2

	move.b	data_a(pc),d0
	and.b	data_b(pc),d0
	beq	ext_minus	*データが２つ無いならキャンセル

	bsr	clr_bottom
	lea	minus_mes(pc),a0
	display2_	(a0),%1111,0,28,20
	display2	minus_mes1,%0111,0,29,20
	display2	minus_mes2,%0111,0,30,28
	moveq.l	#21,d1
	moveq.l	#29,d2
	IOCS	_B_LOCATE
	IOCS	_OS_CURON
	DOS	_INKEY
	cmpi.b	#'1',d0
	bcs	ext_minus
	cmpi.b	#'5',d0
	bhi	ext_minus
	sub.b	#'1',d0
	move.b	d0,mix_mode
	andi.w	#$00ff,d0
	mulu	#31,d0
	lea	minus_mes3(pc),a2
	add.l	d0,a2
	IOCS	_OS_CUROF

	bsr	clr_bottom
	display2_	(a0),%1111,0,28,20
	display2	mix_mes2,%0111,0,29,22
	display2_	(a2),%0111,0,30,30
	moveq.l	#22,d1
	moveq.l	#29,d2
	IOCS	_B_LOCATE
	IOCS	_OS_CURON
	DOS	_INKEY
	cmpi.b	#'1',d0
	sne	work1		*memory direction
	beq	@f
	cmpi.b	#'2',d0
	bne	ext_minus
@@:				*オフセット入力
	IOCS	_OS_CUROF

	bsr	get_pt1
	display2	cv_mes,%1110,41,15,14
				*実際に作業を行う…
	tst.l	buffer2
	bne	@f
	move.b	#2,work2	*mark
	bsr	make_pcm_b
	bmi	ext_minus
@@:
	tst.b	data_type
	beq	@f
	bsr	calc_es_pcm_b
@@:				*bank_aの下準備
	tst.l	buffer1
	bne	@f
	bsr	make_pcm_a
	ori.b	#1,work2	*mark
	bmi	ext_minus
@@:
	tst.b	data_type
	beq	@f
	bsr	calc_es_pcm_a
@@:
	tst.b	work1		*direction?
	beq	case_atob_minus
*case_btoa:			*BからAへ
	move.l	start_point2(pc),a1
	move.l	end_point2(pc),d1
	sub.l	a1,d1
	addq.l	#2,d1
	lsr.l	#2,d1

	move.l	buffer1(pc),a2
	move.l	size1(pc),d2

	move.l	start_point1(pc),d0
	sub.l	a2,d0
	lsr.l	#2,d0
	move.l	d0,offset

	bsr	logic_pcm

	bsr	waku1
	bsr	mfree_adpcm_a	*出来たデータを転送先へ
	bsr	mfree_pcm_a
	move.l	dest_size(pc),size1
	move.l	buffer3(pc),buffer1
	clr.l	start_point1
	clr.l	end_point1
	bsr	make_adpcm_a
	bmi	ext_minus
	move.l	buffer1(pc),a6
	move.l	size1(pc),d6
	move.w	#genten_a,genten_y
	bsr	dsp_wv

	btst.b	#1,work2(pc)
	beq	after_minus
	bsr	waku2
	move.l	buffer2(pc),a6
	move.l	size2(pc),d6
	move.w	#genten_b,genten_y
	bsr	dsp_wv
	bra	after_minus
case_atob_minus:			*AからBへ
	move.l	start_point1(pc),a1
	move.l	end_point1(pc),d1
	sub.l	a1,d1
	addq.l	#2,d1
	lsr.l	#2,d1

	move.l	buffer2(pc),a2
	move.l	size2(pc),d2

	move.l	start_point2(pc),d0
	sub.l	a2,d0
	lsr.l	#2,d0
	move.l	d0,offset

	bsr	logic_pcm

	bsr	waku2
	bsr	mfree_adpcm_b	*出来たデータを転送先へ
	bsr	mfree_pcm_b
	move.l	dest_size(pc),size2
	move.l	buffer3(pc),buffer2
	clr.l	start_point2
	clr.l	end_point2
	bsr	make_adpcm_b
	bmi	ext_minus
	move.l	buffer2(pc),a6
	move.l	size2(pc),d6
	move.w	#genten_b,genten_y
	bsr	dsp_wv

	btst.b	#0,work2(pc)
	beq	after_minus
	bsr	waku1
	move.l	buffer1(pc),a6
	move.l	size1(pc),d6
	move.w	#genten_a,genten_y
	bsr	dsp_wv
after_minus:
	tst.b	data_type	*後始末
	beq	@f
	bsr	calc_es_adpcm_a
	bsr	calc_es_adpcm_b
@@:
	bsr	init_hex_area_a
	bsr	init_hex_area_b
	bsr	put_pt1
ext_minus:
	bra	non_ope

logic_pcm:			*２つのＰＣＭデータの論理演算を行なう
	* < d1=size1,d2=size2
	* < a1=buffer1,a2=buffer2
	* < offset
	* > buffer3,dest_size
	movem.l	d0-d7/a0-a6,-(sp)
	move.l	d1,d3
	add.l	offset(pc),d3
	cmp.l	d2,d3
	bhi	@f
	move.l	d2,d3		*d2の方が大きかったら
@@:
	move.l	d3,dest_size
	lsl.l	#2,d3		*4倍のワークエリアが必要
	move.l	d3,-(sp)
	DOS 	_MALLOC
	addq.w	#4,sp
	move.l	d0,mm_error
	bsr	print_em
	bmi	ext_do_logic
	move.l	d0,buffer3

	movem.l	d1-d2/a1-a2,-(sp)
	move.l	d3,d2		*size
	moveq.l	#%00000101,d1	*mode
	movea.l	a2,a1		*source
	movea.l	buffer3(pc),a2	*dest
	bsr	trans_dma
	movem.l	(sp)+,d1-d2/a1-a2

	move.l	dest_size(pc),d3
	sub.l	d2,d3		*お尻の部分をクリア
	beq	go_logic
	move.l	buffer3(pc),a0
	add.l	d2,d2
	add.l	d2,d2
	add.l	d2,a0
	moveq.l	#0,d0
@@:
	move.l	d0,(a0)+	*いっぺんに２つクリア
	subq.l	#1,d3
	bne	@b
go_logic:
	*a1=use parameter
	movea.l	buffer3(pc),a2
	move.l	offset(pc),d2
	add.l	d2,d2
	add.l	d2,d2
	adda.l	d2,a2
	move.l	d1,d0
	bsr	do_logic
ext_do_logic:
	movem.l	(sp)+,d0-d7/a0-a6
	rts

do_logic:
	* < a1=source pcm data
	* < a2=dest.  pcm data
	* < d0.l=data size(adpcm range)
	add.l	d0,d0
	moveq.l	#0,d1
	move.b	mix_mode(pc),d1
	add.w	d1,d1
	move.w	minus_tbl(pc,d1.w),d1
	jmp	minus_tbl(pc,d1.w)
minus_tbl:
	dc.w	minus_ADD-minus_tbl
	dc.w	minus_SUB-minus_tbl
	dc.w	minus_OR-minus_tbl
	dc.w	minus_AND-minus_tbl
	dc.w	minus_EOR-minus_tbl
minus_ADD:
@@:
	move.w	(a1)+,d2
	add.w	(a2),d2
*	bsr	chk_ovf
	move.w	d2,(a2)+
	subq.l	#1,d0
	bne	@b
	rts
minus_SUB:
@@:
	move.w	(a1)+,d2
	sub.w	(a2),d2
*	bsr	chk_ovf
	move.w	d2,(a2)+
	subq.l	#1,d0
	bne	@b
	rts
minus_OR:
@@:
	move.w	(a1)+,d2
	or.w	(a2),d2
*	bsr	chk_ovf
	move.w	d2,(a2)+
	subq.l	#1,d0
	bne	@b
	rts
minus_AND:
@@:
	move.w	(a1)+,d2
	and.w	(a2),d2
*	bsr	chk_ovf
	move.w	d2,(a2)+
	subq.l	#1,d0
	bne	@b
	rts
minus_EOR:
@@:
	move.w	(a1)+,d2
	move.w	(a2),d1
	eor.w	d1,d2
*	bsr	chk_ovf
	move.w	d2,(a2)+
	subq.l	#1,d0
	bne	@b
	rts

deconvolution_ope:		*２つのデータの逆たたみ込みを行なう
	movem.l	d0-d7/a0-a6,reg_buff2
	clr.b	work2		*init work2
	move.b	data_a(pc),d0
	and.b	data_b(pc),d0
	beq	ext_deconvol	*データが２つ無いならキャンセル

	bsr	clr_bottom
	lea	deconvol_mes(pc),a0
	display2_	(a0),%1111,0,28,13
	display2	mix_mes2,%0111,0,29,22
	display2	deconvol_mes3,%0111,0,30,26
	moveq.l	#22,d1
	moveq.l	#29,d2
	IOCS	_B_LOCATE
	IOCS	_OS_CURON
	DOS	_INKEY
	cmpi.b	#'1',d0
	sne	work1		*memory direction
	beq	@f
	cmpi.b	#'2',d0
	bne	ext_deconvol
@@:			*オフセット入力
	IOCS	_OS_CUROF

	bsr	get_pt1
	display2	cv_mes,%1110,41,15,14
				*実際に作業を行う…
	tst.l	buffer2
	bne	@f
	move.b	#2,work2	*mark
	bsr	make_pcm_b
	bmi	ext_deconvol
@@:
	tst.b	data_type
	beq	@f
	bsr	calc_es_pcm_b
@@:				*bank_aの下準備
	tst.l	buffer1
	bne	@f
	bsr	make_pcm_a
	ori.b	#1,work2	*mark
	bmi	ext_deconvol
@@:
	tst.b	data_type
	beq	@f
	bsr	calc_es_pcm_a
@@:
	tst.b	work1		*direction?
	beq	case_atob_deconvol
*case_btoa:			*BからAへ
	move.l	start_point2(pc),a1
	move.l	end_point2(pc),d1
	sub.l	a1,d1
	addq.l	#2,d1
	lsr.l	#2,d1

	move.l	buffer1(pc),a2
	move.l	end_point1(pc),d2
	sub.l	start_point1(pc),d2
	addq.l	#2,d2
	lsr.l	#2,d2
	move.l	size1(pc),d3

	move.l	start_point1(pc),d0
	sub.l	a2,d0
	lsr.l	#2,d0
	move.l	d0,offset
	add.l	d2,d0
	sub.l	d0,d3

	bsr	deconvolution_pcm

	bsr	waku1
	bsr	mfree_adpcm_a	*出来たデータを転送先へ
	bsr	mfree_pcm_a
	move.l	dest_size(pc),size1
	move.l	buffer3(pc),buffer1
	clr.l	start_point1
	clr.l	end_point1
	bsr	make_adpcm_a
	bmi	ext_deconvol
	move.l	buffer1(pc),a6
	move.l	size1(pc),d6
	move.w	#genten_a,genten_y
	bsr	dsp_wv

	btst.b	#1,work2(pc)
	beq	after_deconvol
	bsr	waku2
	move.l	buffer2(pc),a6
	move.l	size2(pc),d6
	move.w	#genten_b,genten_y
	bsr	dsp_wv
	bra	after_deconvol
case_atob_deconvol:			*AからBへ
	move.l	start_point1(pc),a1
	move.l	end_point1(pc),d1
	sub.l	a1,d1
	addq.l	#2,d1
	lsr.l	#2,d1

	move.l	buffer2(pc),a2
	move.l	end_point2(pc),d2
	sub.l	start_point2(pc),d2
	addq.l	#2,d2
	lsr.l	#2,d2
	move.l	size2(pc),d3

	move.l	start_point2(pc),d0
	sub.l	a2,d0
	lsr.l	#2,d0
	move.l	d0,offset
	add.l	d2,d0
	sub.l	d0,d3

	bsr	deconvolution_pcm

	bsr	waku2
	bsr	mfree_adpcm_b	*出来たデータを転送先へ
	bsr	mfree_pcm_b
	move.l	dest_size(pc),size2
	move.l	buffer3(pc),buffer2
	clr.l	start_point2
	clr.l	end_point2
	bsr	make_adpcm_b
	bmi	ext_deconvol
	move.l	buffer2(pc),a6
	move.l	size2(pc),d6
	move.w	#genten_b,genten_y
	bsr	dsp_wv

	btst.b	#0,work2(pc)
	beq	after_deconvol
	bsr	waku1
	move.l	buffer1(pc),a6
	move.l	size1(pc),d6
	move.w	#genten_a,genten_y
	bsr	dsp_wv
after_deconvol:
	tst.b	data_type	*後始末
	beq	@f
	bsr	calc_es_adpcm_a
	bsr	calc_es_adpcm_b
@@:
	bsr	init_hex_area_a
	bsr	init_hex_area_b
	bsr	put_pt1
ext_deconvol:
	bra	non_ope

deconvolution_pcm:		*２つのＰＣＭデータの逆たたみ込みを行なう
	* < a1=data1 addr, d1=size1
	* < a2=data2 base addr
	* < offset of data2
	* < d2=effective size2, d3=tail size
	* > buffer3,dest_size
	movem.l	d0-d7/a0-a6,-(sp)

	move.l	#-1,-(sp)
	DOS	_MALLOC
	addq.w	#4,sp
	andi.l	#$00ffffff,d0
	move.l	d0,d5
	move.l	d5,-(sp)	*取れる限り確保(d5:size)
	DOS	_MALLOC
	addq.w	#4,sp
mem_er_dcnvl:
	move.l	d0,mm_error
	bsr	print_em
	bmi	ext_do_deconvol
	move.l	d0,buffer3

	move.l	offset(pc),d0
	bne	@f
	moveq.l	#0,d4		*total size
	move.l	buffer3(pc),a3	*オフセットなしならば
	bra	imp_clear_zero_dc
@@:
	lsl.l	#2,d0
	cmp.l	d5,d0
	bls	@f
go_mem_er_dcvl:
	moveq.l	#-1,d0		*memory error
	bra	mem_er_dcnvl
@@:
	movem.l	d1-d2/a1-a2,-(sp)	*データ２の頭の部分をコピー
	move.l	d0,d2		*size
	move.l	d0,d4
	moveq.l	#%00000101,d1	*mode
	movea.l	a2,a1		*source
	movea.l	buffer3(pc),a2	*dest
	lea	(a2,d2.l),a3	*next
	bsr	trans_dma
	movem.l	(sp)+,d1-d2/a1-a2
imp_clear_zero_dc:
	move.l	d2,d7
	lsl.l	#3,d7

	add.l	d7,d4
	cmp.l	d5,d4
	bhi	go_mem_er_dcvl	*memory error

				*a1,d1調整
	lsl.l	d1		*data1 count
@@:
	tst.w	(a1)		*１桁目が０だとダメ
	bne	@f
	addq.w	#2,a1
	subq.l	#2,d1
	bhi	@b
	bra	ext_do_deconvol	*異常なケース
@@:
	move.l	a3,a0
	lsr.l	#2,d7
	move.l	offset(pc),d0
	lsl.l	#2,d0
	lea	(a2,d0.l),a4	*destのスタートポイントから

	move.w	(a1),d4
@@:
	move.w	(a4)+,d0
	muls	d4,d0
	move.l	d0,(a0)+
	subq.l	#1,d7
	bne	@b

	moveq.l	#16,d6		*shift counter
	move.l	d2,d4
	lsl.l	d4		*data2 count
	move.l	a1,a4
	move.l	a3,a5		*あとで使用
	pea	(a2)
dimp_lp0:
	move.l	a4,a1
	move.l	a3,a0
	move.l	(a0),d7
	divs	(a1),d7
	move.w	d7,(a2)+	*答えを保存
	move.l	d4,d2
	cmp.l	d1,d2
	bls	@f
	move.l	d1,d2
@@:
	tst.w	d7		*答えが０ならば演算は不要
	beq	next_a1_dc
dimp_lp1:
	move.w	(a1)+,d0
	muls	d7,d0
	asr.l	d6,d0
	sub.l	d0,(a0)+
	subq.l	#1,d2
	bne	dimp_lp1
next_a1_dc:
	addq.w	#4,a3
	subq.l	#1,d4
	bne	dimp_lp0

	move.l	a2,d0
	move.l	(sp)+,a0
	sub.l	a0,d0
	lsr.l	d0
@@:
	move.w	(a0)+,(a5)+
	subq.l	#1,d0
	bne	@b

	move.l	a5,d4
	sub.l	buffer3(pc),d4	*calc again total size

	lsl.l	#2,d3		*tail転送
	add.l	d3,d4
	cmp.l	d5,d4
	bhi	go_mem_er_dcvl	*memory error
	move.l	d3,d2		*size
	beq	@f
	moveq.l	#%00000101,d1	*mode
	movea.l	a2,a1		*source(a2は丁度いい値になっている)
	movea.l	a5,a2		*dest
	bsr	trans_dma
@@:
	move.l	d4,-(sp)
	move.l	buffer3(pc),-(sp)
	DOS	_SETBLOCK
	addq.w	#8,sp
	lsr.l	#2,d4
	move.l	d4,dest_size
ext_do_deconvol:
	movem.l	(sp)+,d0-d7/a0-a6
	rts

convolution_ope:		*２つのデータのたたみ込みを行なう
	movem.l	d0-d7/a0-a6,reg_buff2
	clr.b	work2		*init work2
	move.b	data_a(pc),d0
	and.b	data_b(pc),d0
	beq	ext_convol	*データが２つ無いならキャンセル

	bsr	clr_bottom
	lea	convol_mes(pc),a0
	display2_	(a0),%1111,0,28,11
	display2	mix_mes2,%0111,0,29,22
	display2	convol_mes3,%0111,0,30,26
	moveq.l	#22,d1
	moveq.l	#29,d2
	IOCS	_B_LOCATE
	IOCS	_OS_CURON
	DOS	_INKEY
	cmpi.b	#'1',d0
	sne	work1		*memory direction
	beq	@f
	cmpi.b	#'2',d0
	bne	ext_convol
@@:			*オフセット入力
	IOCS	_OS_CUROF

	bsr	get_pt1
	display2	cv_mes,%1110,41,15,14
				*実際に作業を行う…
	tst.l	buffer2
	bne	@f
	move.b	#2,work2	*mark
	bsr	make_pcm_b
	bmi	ext_convol
@@:
	tst.b	data_type
	beq	@f
	bsr	calc_es_pcm_b
@@:				*bank_aの下準備
	tst.l	buffer1
	bne	@f
	bsr	make_pcm_a
	ori.b	#1,work2	*mark
	bmi	ext_convol
@@:
	tst.b	data_type
	beq	@f
	bsr	calc_es_pcm_a
@@:
	tst.b	work1		*direction?
	beq	case_atob_convol
*case_btoa:			*BからAへ
	move.l	start_point2(pc),a1
	move.l	end_point2(pc),d1
	sub.l	a1,d1
	addq.l	#2,d1
	lsr.l	#2,d1

	move.l	buffer1(pc),a2
	move.l	end_point1(pc),d2
	sub.l	start_point1(pc),d2
	addq.l	#2,d2
	lsr.l	#2,d2
	move.l	size1(pc),d3

	move.l	start_point1(pc),d0
	sub.l	a2,d0
	lsr.l	#2,d0
	move.l	d0,offset
	add.l	d2,d0
	sub.l	d0,d3

	bsr	convolution_pcm

	bsr	waku1
	bsr	mfree_adpcm_a	*出来たデータを転送先へ
	bsr	mfree_pcm_a
	move.l	dest_size(pc),size1
	move.l	buffer3(pc),buffer1
	clr.l	start_point1
	clr.l	end_point1
	bsr	make_adpcm_a
	bmi	ext_convol
	move.l	buffer1(pc),a6
	move.l	size1(pc),d6
	move.w	#genten_a,genten_y
	bsr	dsp_wv

	btst.b	#1,work2(pc)
	beq	after_convol
	bsr	waku2
	move.l	buffer2(pc),a6
	move.l	size2(pc),d6
	move.w	#genten_b,genten_y
	bsr	dsp_wv
	bra	after_convol
case_atob_convol:			*AからBへ
	move.l	start_point1(pc),a1
	move.l	end_point1(pc),d1
	sub.l	a1,d1
	addq.l	#2,d1
	lsr.l	#2,d1

	move.l	buffer2(pc),a2
	move.l	end_point2(pc),d2
	sub.l	start_point2(pc),d2
	addq.l	#2,d2
	lsr.l	#2,d2
	move.l	size2(pc),d3

	move.l	start_point2(pc),d0
	sub.l	a2,d0
	lsr.l	#2,d0
	move.l	d0,offset
	add.l	d2,d0
	sub.l	d0,d3

	bsr	convolution_pcm

	bsr	waku2
	bsr	mfree_adpcm_b	*出来たデータを転送先へ
	bsr	mfree_pcm_b
	move.l	dest_size(pc),size2
	move.l	buffer3(pc),buffer2
	clr.l	start_point2
	clr.l	end_point2
	bsr	make_adpcm_b
	bmi	ext_convol
	move.l	buffer2(pc),a6
	move.l	size2(pc),d6
	move.w	#genten_b,genten_y
	bsr	dsp_wv

	btst.b	#0,work2(pc)
	beq	after_convol
	bsr	waku1
	move.l	buffer1(pc),a6
	move.l	size1(pc),d6
	move.w	#genten_a,genten_y
	bsr	dsp_wv
after_convol:
	tst.b	data_type	*後始末
	beq	@f
	bsr	calc_es_adpcm_a
	bsr	calc_es_adpcm_b
@@:
	bsr	init_hex_area_a
	bsr	init_hex_area_b
	bsr	put_pt1
ext_convol:
	bra	non_ope

convolution_pcm:		*２つのＰＣＭデータのたたみ込みを行なう
	* < a1=data1 addr, d1=size1
	* < a2=data2 base addr
	* < offset of data2
	* < d2=effective size2, d3=tail size
	* > buffer3,dest_size
	movem.l	d0-d7/a0-a6,-(sp)

	move.l	#-1,-(sp)
	DOS	_MALLOC
	addq.w	#4,sp
	andi.l	#$00ffffff,d0
	move.l	d0,d5
	move.l	d5,-(sp)	*取れる限り確保(d5:size)
	DOS	_MALLOC
	addq.w	#4,sp
mem_er_cnvl:
	move.l	d0,mm_error
	bsr	print_em
	bmi	ext_do_convol
	move.l	d0,buffer3

	move.l	offset(pc),d0
	bne	@f
	moveq.l	#0,d4		*total size
	move.l	buffer3(pc),a3	*オフセットなしならば
	bra	imp_clear_zero
@@:
	lsl.l	#2,d0
	cmp.l	d5,d0
	bls	@f
go_mem_er_cvl:
	moveq.l	#-1,d0		*memory error
	bra	mem_er_cnvl
@@:
	movem.l	d1-d2/a1-a2,-(sp)	*データ２の頭の部分をコピー
	move.l	d0,d2		*size
	move.l	d0,d4
	moveq.l	#%00000101,d1	*mode
	movea.l	a2,a1		*source
	movea.l	buffer3(pc),a2	*dest
	lea	(a2,d2.l),a3	*next
	bsr	trans_dma
	movem.l	(sp)+,d1-d2/a1-a2
imp_clear_zero:
	move.l	d1,d6
	move.l	d2,d7
	lsl.l	#3,d6
	lsl.l	#3,d7
	add.l	d6,d7
	subq.l	#4,d7

	add.l	d7,d4
	cmp.l	d5,d4
	bhi	go_mem_er_cvl	*memory error

	move.l	a3,a0
	lsr.l	#2,d7
	move.l	d7,d6		*あとで使用
@@:
	clr.l	(a0)+		*zero clear
	subq.l	#1,d7
	bne	@b

	move.l	offset(pc),d0
	lsl.l	#2,d0
	lea	(a2,d0.l),a4	*data2 effective addr.

	move.l	d2,d4
	lsl.l	d4		*data2 count

	lsl.l	d1		*data1 count

	move.l	a3,a5		*あとで使用
imp_lp0:
	move.w	(a1)+,d7
	beq	next_a1_
	move.l	d4,d2
	move.l	a4,a2
	move.l	a3,a0
imp_lp1:
	move.w	(a2)+,d0
	muls	d7,d0
	add.l	d0,(a0)+
	subq.l	#1,d2
	bne	imp_lp1
next_a1_:
	addq.w	#4,a3
	subq.l	#1,d1
	bne	imp_lp0

	move.l	a5,a0
@@:
	move.l	(a5)+,d0
	swap	d0
	move.w	d0,(a0)+
	subq.l	#1,d6
	bne	@b

	move.l	a0,d4
	sub.l	buffer3(pc),d4	*calc again total size

	lsl.l	#2,d3		*tail転送
	add.l	d3,d4
	cmp.l	d5,d4
	bhi	go_mem_er_cvl	*memory error
	move.l	d3,d2		*size
	beq	@f
	moveq.l	#%00000101,d1	*mode
	movea.l	a2,a1		*source(a2は丁度いい値になっている)
	movea.l	a0,a2		*dest
	bsr	trans_dma
@@:
	move.l	d4,-(sp)
	move.l	buffer3(pc),-(sp)
	DOS	_SETBLOCK
	addq.w	#8,sp
	lsr.l	#2,d4
	move.l	d4,dest_size
ext_do_convol:
	movem.l	(sp)+,d0-d7/a0-a6
	rts

insert:				*データをはめこむ
	movem.l	d0-d7/a0-a6,reg_buff2
	clr.b	work2		*init work2
	st.b	mix_mode
	move.b	data_a(pc),d0
	and.b	data_b(pc),d0
	beq	ext_mix		*データが２つ無いならキャンセル

	bsr	clr_bottom
	lea	ins_mes(pc),a0
	bra	dsp_mix_mes

copy_data:			*データのコピー
	movem.l	d0-d7/a0-a6,reg_buff2
	move.b	data_a(pc),d0
	or.b	data_b(pc),d0
	beq	ext_mix		*データが両方無いならキャンセル

	bsr	clr_bottom
	display2	cpd_mes,%1111,0,28,9
	display2	mix_mes2,%0111,0,29,22
	display2	copy_mes3,%0111,0,30,19
	moveq.l	#22,d1
	moveq.l	#29,d2
	IOCS	_B_LOCATE
	IOCS	_OS_CURON
	DOS	_INKEY
	cmpi.b	#'1',d0
	sne	work1		*memory direction
	beq	cpd1
	cmpi.b	#'2',d0
	beq	cpd1
	bra	ext_cpd
cpd1:
	IOCS	_OS_CUROF

	bsr	get_pt1
	display2	cv_mes,%1110,41,15,14

	tst.b	work1
	bne	copy_btoa
*copy_atob:
	tst.b	data_a
	beq	ext_cpd
	move.l	end_point1(pc),a5
	move.l	start_point1(pc),a4
	tst.b	data_type
	bne	cab0
	sub.l	buffer1(pc),a5
	sub.l	buffer1(pc),a4
	lea	buffer2(pc),a3
	bra	cab00
cab0:
	sub.l	address1(pc),a5
	sub.l	address1(pc),a4
	lea	address2(pc),a3
cab00:
	clr.l	start_point2
	clr.l	end_point2
	bsr	waku2
	move.l	size1(pc),d2
	move.l	d2,size2	*d2=size
	move.l	address1(pc),a1	*a1=source
	bsr	mfree_adpcm_b
	tst.l	address1
	beq	cab1		*goto pcm data trans
	move.l	d2,-(sp)
	DOS	_MALLOC
	addq.w	#4,sp
	move.l	d0,mm_error
	bsr	print_em
	bmi	ext_cpd		*case:error
	movea.l	d0,a2		*a2=destination
	move.l	a2,address2
	moveq.l	#5,d1		*mode
	bsr	trans_dma
cab1:				*copy pcm data
	lsl.l	#2,d2		*size
	move.l	buffer1(pc),a1
	bsr	mfree_pcm_b
	tst.l	buffer1
	beq	cab2
	move.l	d2,-(sp)
	DOS	_MALLOC
	addq.w	#4,sp
	move.l	d0,mm_error
	bsr	print_em
	beq	cab11
	bsr	mfree_adpcm_b	*case:error
	bra	ext_cpd
cab11:
	movea.l	d0,a2
	move.l	a2,buffer2
	moveq.l	#5,d1		*mode
	bsr	trans_dma
	move.l	size2(pc),d6	*size
	move.l	a2,a6		*addr.
	move.w	#genten_b,genten_y
	bsr	dsp_wv
	bra	cab2_
cab2:
	move.l	size2(pc),d6	*size
	move.l	address2(pc),a6	*address
	move.w	#genten_b,genten_y
	bsr	dsp_wv_2
cab2_:
	st	data_b
	add.l	(a3),a4		*dest start point
	move.l	a4,start_point2
	add.l	(a3),a5		*dest end point
	move.l	a5,end_point2
	bsr	init_hex_area_b
	bra	ext_cpd
copy_btoa:
	tst.b	data_b
	beq	ext_cpd
	move.l	end_point2(pc),a5
	move.l	start_point2(pc),a4
	tst.b	data_type
	bne	cba0
	sub.l	buffer2(pc),a5
	sub.l	buffer2(pc),a4
	lea	buffer1(pc),a3
	bra	cba00
cba0:
	sub.l	address2(pc),a5
	sub.l	address2(pc),a4
	lea	address1(pc),a3
cba00:
	clr.l	start_point1
	clr.l	end_point1
	bsr	waku1
	move.l	size2(pc),d2
	move.l	d2,size1	*d2=size
	move.l	address2(pc),a1	*a1=source
	bsr	mfree_adpcm_a
	tst.l	address2
	beq	cba1		*goto pcm data trans
	move.l	d2,-(sp)
	DOS	_MALLOC
	addq.w	#4,sp
	move.l	d0,mm_error
	bsr	print_em
	bmi	ext_cpd		*case:error
	movea.l	d0,a2		*a2=destination
	move.l	a2,address1
	moveq.l	#5,d1		*mode
	bsr	trans_dma
cba1:				*copy pcm data
	lsl.l	#2,d2		*size
	move.l	buffer2(pc),a1
	bsr	mfree_pcm_a
	tst.l	buffer2
	beq	cba2
	move.l	d2,-(sp)
	DOS	_MALLOC
	addq.w	#4,sp
	move.l	d0,mm_error
	bsr	print_em
	beq	cba11
	bsr	mfree_adpcm_a	*case:error
	bra	ext_cpd
cba11:
	movea.l	d0,a2
	move.l	a2,buffer1
	moveq.l	#5,d1		*mode
	bsr	trans_dma
	move.l	size1(pc),d6	*size
	move.l	a2,a6		*addr.
	move.w	#genten_a,genten_y
	bsr	dsp_wv
	bra	cba2_
cba2:
	move.l	size1(pc),d6	*size
	move.l	address1(pc),a6	*address
	move.w	#genten_a,genten_y
	bsr	dsp_wv_2
cba2_:
	st	data_a
	add.l	(a3),a4		*dest start point
	move.l	a4,start_point1
	add.l	(a3),a5		*dest end point
	move.l	a5,end_point1
	bsr	init_hex_area_a
ext_cpd:
	bsr	put_pt1
	bra	non_ope

effect:				*エフェクトをかける
	movem.l	d0-d7/a0-a6,reg_buff2
	move.b	data_a(pc),d0
	or.b	data_b(pc),d0
	beq	ext_efct

	bsr	clr_bottom
	display2	efct_mes,%1111,0,28,13
	display2	disk_md_mes2,%0111,0,29,16	*select job.
	display2	efct_mes2,%0111,0,30,96		*menu
	moveq.l	#16,d1
	moveq.l	#29,d2
	IOCS	_B_LOCATE
	IOCS	_OS_CURON
	DOS	_INKEY

	move.b	d0,d1
	andi.b	#$DF,d1

	moveq.l	#0,d2		*d2=0
	cmpi.b	#'U',d1		*up / down
	beq	efct

	addq.b	#2,d2		*d2=2
	cmpi.b	#'L',d1		*left / right
	beq	efct

	addq.b	#2,d2		*d2=4
	cmpi.b	#'A',d1		*add 0
	beq	efct

	addq.l	#2,d2		*d2=6
	cmpi.b	#'R',d1		*reverse
	beq	efct

	addq.b	#2,d2		*d2=8
	cmpi.b	#'C',d1		*chorus
	beq	efct

	addq.b	#2,d2		*d2=10
	cmpi.b	#'D',d1		*delay
	beq	efct

	addq.b	#2,d2		*d2=12
	cmpi.b	#'S',d1		*sfx
	beq	efct

	addq.b	#2,d2		*d2=14
	cmpi.b	#'F',d1		*filter
	beq	efct

	addq.b	#2,d2		*d2=16
	cmpi.b	#'E',d1		*fade in/out
	beq	efct

	moveq.l	#0,d2		*d2=0
	cmpi.b	#'1',d0
	beq	efct

	addq.b	#2,d2		*d2=2
	cmpi.b	#'2',d0
	beq	efct

	addq.b	#2,d2		*d2=4
	cmpi.b	#'3',d0
	beq	efct

	addq.b	#2,d2		*d2=6
	cmpi.b	#'4',d0
	beq	efct

	addq.b	#2,d2		*d2=8
	cmpi.b	#'5',d0
	beq	efct

	addq.b	#2,d2		*d2=10
	cmpi.b	#'6',d0
	beq	efct

	addq.b	#2,d2		*d2=12
	cmpi.b	#'7',d0
	beq	efct

	addq.b	#2,d2		*d2=14
	cmpi.b	#'8',d0
	beq	efct

	addq.b	#2,d2		*d2=16
	cmpi.b	#'9',d0
	beq	efct

	bra	ext_efct
efct:
	move.w	d2,work1	*mark job number
	cmpi.b	#4,d2
	bls	@f
	cmpi.b	#16,d2
	bne	efct_curof
@@:
	bsr	clr_bottom
	tst.b	d2
	beq	case_ud		*up down
	cmpi.b	#4,d2
	beq	case_ad
	cmpi.b	#16,d2
	beq	case_en
	display2	l_r_mes,%1111,0,28,16	*left or right
	bra	dsp_ofs?
case_ad:
	display2	ad_mes,%1111,0,28,15	*add 0 to tail
	display2	howmany0,%0111,0,29,31	*input offset value
	moveq.l	#0,d1
	moveq.l	#30,d2
	IOCS	_B_LOCATE
	bra	getlninp
case_ud:
	display2	up_down_mes,%1111,0,28,13	*up or down
	bra	dsp_ofs?
case_en:
	display2	env_mes,%1111,0,28,18	*change an envelope
	bra	get_inout
dsp_ofs?:
	display2	up_down_mes2,%0111,0,29,19	*input offset value
	moveq.l	#19,d1
	moveq.l	#29,d2
	IOCS	_B_LOCATE
getlninp:
	lea	input_buffer(pc),a1
	move.b	#6,(a1)
	pea	(a1)
	DOS	_GETS		*一行入力
	addq.w	#4,sp

	addq.w	#2,a1		*chr data buffer

	cmpi.b	#$20,(a1)
	bls	ext_efct	*スペースよりも小さい時はキャンセル

	bsr	get_num
	move.l	d1,d0
	bpl	@f
	neg.l	d0
@@:
	cmpi.l	#65535,d0
	bhi	ext_efct	*数が巨大すぎるならキャンセル

	tst.b	data_type
	bne	sv_ofst
	move.w	work1(pc),d0	*job内容によりパラメータが適性かどうか検査
	cmpi.w	#4,d0
	bne	@f
even_pls:
	addq.w	#1,d1		*case:add 0 to tail
	bclr.l	#0,d1		*.even plus
	bra	sv_ofst
@@:
	cmpi.b	#2,d0		*case:left/right shift
	bne	@f
	tst.w	d1
	bpl	even_pls
	subq.w	#1,d1
	bclr.l	#0,d1		*.even minus
	bra	sv_ofst
@@:
sv_ofst:
	move.w	d1,offset	*save offset
	beq	ext_efct	*数が０ならキャンセル
	bra	efct_curof
get_inout:				*in/out levelの設定
	display2	inout_mes,%0111,0,29,19	*input fade in/out level
	moveq.l	#19,d1
	moveq.l	#29,d2
	IOCS	_B_LOCATE

	lea	input_buffer(pc),a1
	move.b	#3,(a1)
	pea	(a1)
	DOS	_GETS		*一行入力
	addq.w	#4,sp

	addq.w	#2,a1		*chr data buffer

	cmpi.b	#$20,(a1)
	bls	ext_efct	*スペースよりも小さい時はキャンセル

	bsr	get_num
	cmpi.w	#127,d1
	bhi	ext_efct
	move.w	d1,work2	*save level

			*input fade in or out
	display2	sel_mode_mes,%0111,0,30,28
	moveq.l	#28,d1
	moveq.l	#30,d2
	IOCS	_B_LOCATE
	DOS	_INKEY
	cmpi.b	#'1',d0
	beq	sv_env_mode
	cmpi.b	#'2',d0
	bne	ext_efct
sv_env_mode:
	move.b	d0,work3

efct_curof:
	IOCS	_OS_CUROF

	bsr	get_pt1
	display2	cv_mes,%1110,41,15,14
				*実際に作業を行う…
	lea	address1(pc),a0
	tst.b	bank_sel
	beq	efct1

	tst.b	data_b		*bank_bの下準備
	beq	ext_efct
	lea	20(a0),a0		*bank_b base addr.
	tst.l	pbuffer(a0)
	bne	efct0
	bsr	make_pcm_b
	bmi	ext_efct
efct0:
	tst.b	data_type
	beq	efct2
	bsr	calc_es_pcm_b
	bra	efct2
efct1:				*bank_aの下準備
	tst.b	data_a
	beq	ext_efct
	tst.l	pbuffer(a0)
	bne	efct11
	bsr	make_pcm_a
	bmi	ext_efct
efct11:
	tst.b	data_type
	beq	efct2
	bsr	calc_es_pcm_a
efct2:
	movea.l	pstart(a0),a1	*a1=pcm start addr.
	move.l	pend(a0),d2
	sub.l	a1,d2
	addq.l	#2,d2		*d2=data size

	move.w	work1(pc),d6
	move.w	efct_jmp(pc,d6.w),a6
	jmp	efct_jmp(pc,a6.w)
efct3:
	bra	atoshimatsu
efct_jmp:
	dc.w	up_down_shift-efct_jmp,left_right-efct_jmp
	dc.w	add0-efct_jmp,reverse-efct_jmp
	dc.w	chorus-efct_jmp,delay-efct_jmp
	dc.w	sfx-efct_jmp,filting-efct_jmp
	dc.w	chg_env-efct_jmp
reverse:			*pcm dataを逆転する
	move.l	d2,-(sp)	*d2=size
	DOS	_MALLOC
	addq.w	#4,sp
	move.l	d0,mm_error
	bsr	print_em
	bmi	ext_efct	*case:error
	move.l	d0,work2
	move.l	d0,a2		*dest
	moveq.l	#5,d1		*mode
	bsr	trans_dma

	add.l	d2,a2
rev_lp:
	move.w	-(a2),(a1)+	*なんかMC68000って凄いよな…
	subq.l	#2,d2
	bne	rev_lp

	move.l	work2(pc),-(sp)
	DOS	_MFREE
	addq.w	#4,sp
	bra	efct3

up_down_shift:
	move.w	offset(pc),d3
	move.l	d2,d1
uds_lp:
	move.w	(a1),d2
	add.w	d3,d2
*	bsr	chk_ovf
	move.w	d2,(a1)+
	subq.l	#2,d1
	bne	uds_lp
	bra	efct3

left_right:
	* < a0=parameter base address
	* < a1=pcm start
	* < d2=data size
	move.w	offset(pc),d6
	ext.l	d6
	bsr	remk_ofs
	tst.l	d6
	bpl	case_right
*case_left:
	neg.l	d6
	move.l	a1,a2	*dest.
	add.l	d6,a1	*source
	move.l	psize(a0),d2
	lsl.l	#2,d2
	add.l	pbuffer(a0),d2
	sub.l	pstart(a0),d2	*start_p～data endまでのサイズ
	move.l	d2,d0		*保存
	sub.l	d6,d2		*転送バイト数
	bcc	@f
	moveq.l	#0,d6		*転送バイト数異常のケース
	move.l	d0,d2
	bra	sv_new_ep
@@:
	moveq.l	#5,d1	*mode
	bsr	trans_dma
sv_new_ep:
	sub.l	d6,pend(a0)	*new end point

	move.l	pstart(a0),d0
	sub.l	pbuffer(a0),d0	*start_pまでのサイズ
	add.l	d0,d2		*new size
	move.l	d2,-(sp)	*縮小
	move.l	pbuffer(a0),-(sp)
	DOS	_SETBLOCK
	addq.w	#8,sp

	lsr.l	#2,d2
	move.l	d2,psize(a0)
	tst.l	paddress(a0)
	beq	atoshimatsu	*adpcm bufferがないなら…

	move.l	d2,-(sp)
	move.l	paddress(a0),-(sp)
	DOS	_SETBLOCK
	addq.w	#8,sp
	bra	atoshimatsu

case_right:
	move.l	psize(a0),d4
	lsl.l	#2,d4
	add.l	d6,d4
	move.l	d4,-(sp)
	DOS	_MALLOC
	addq.w	#4,sp
	move.l	d0,mm_error
	bsr	print_em
	bmi	ext_efct	*case:error
	move.l	d0,a2
	move.l	a2,-(sp)	*保存
	move.l	pbuffer(a0),a1	*source
	move.l	pstart(a0),d2
	sub.l	pbuffer(a0),d2
	moveq.l	#5,d1
	bsr	trans_dma	*先頭部転送

	add.l	d2,a2
	move.l	d6,d0
	lsr.l	#2,d0
cr_lp:
	clr.l	(a2)+		*ダミー部
	subq.l	#1,d0
	bne	cr_lp

	move.l	pstart(a0),a1	*source
	move.l	psize(a0),d2
	lsl.l	#2,d2
	add.l	pbuffer(a0),d2
	sub.l	pstart(a0),d2	*start_p～data endまでのサイズ
	bsr	trans_dma	*メイン部を転送

	add.l	d6,pend(a0)
	move.l	pbuffer(a0),d0
	sub.l	d0,pstart(a0)
	sub.l	d0,pend(a0)	*一時的に…
mk_new_prm:
	tst.b	bank_sel
	bne	cr_bank_b
*cr_bank_a:
	bsr	mfree_adpcm_a
	bsr	mfree_pcm_a
	bra	cr_new_adr
cr_bank_b:
	bsr	mfree_adpcm_b
	bsr	mfree_pcm_b
cr_new_adr:
	move.l	(sp)+,d0
	move.l	d0,pbuffer(a0)
	add.l	d0,pstart(a0)
	add.l	d0,pend(a0)
	lsr.l	#2,d4
	move.l	d4,psize(a0)	*new size
	bra	atoshimatsu

add0:				*end pointの最後尾にゼロを加える
	* < a0=parameter base address
	* < a1=pcm start
	* < d2=data size
	moveq.l	#0,d6
	move.w	offset(pc),d6
	bsr	remk_ofs

	move.l	psize(a0),d4
	lsl.l	#2,d4
	add.l	d6,d4
	move.l	d4,-(sp)
	DOS	_MALLOC
	addq.w	#4,sp
	move.l	d0,mm_error
	bsr	print_em
	bmi	ext_efct	*case:error
	move.l	d0,a2		*destination
	move.l	a2,-(sp)	*保存
	move.l	pbuffer(a0),a1	*source
	move.l	pend(a0),d2
	sub.l	pbuffer(a0),d2
	addq.l	#2,d2
	moveq.l	#5,d1
	bsr	trans_dma	*先頭部転送

	add.l	d2,a2
	move.l	d6,d0
	lsr.l	#2,d0
cr_lp2:
	clr.l	(a2)+		*ダミー部
	subq.l	#1,d0
	bne	cr_lp2

	move.l	pend(a0),a1	*source
	addq.w	#2,a1
	move.l	psize(a0),d2
	lsl.l	#2,d2
	add.l	pbuffer(a0),d2
	sub.l	pend(a0),d2	*end_p～data endまでのサイズ
	subq.l	#2,d2
	bsr	trans_dma	*メイン部を転送

	add.l	d6,pend(a0)
	move.l	pbuffer(a0),d0
	sub.l	d0,pstart(a0)
	sub.l	d0,pend(a0)	*一時的に…
	bra	mk_new_prm

remk_ofs:		*オフセットをデータモードに応じて調整
	add.l	d6,d6
	tst.b	data_type
	bne	remk_ofs_adpcm
	rts
remk_ofs_adpcm:
	add.l	d6,d6
	rts
detune_up:	equ	64971	*64783
detune_down:	equ	570	*761
chorus:				*コーラス処理
	move.l	d2,-(sp)	*get work area
	DOS	_MALLOC
	addq.w	#4,sp
	move.l	d0,mm_error
	bsr	print_em
	bmi	ext_efct	*case:error
	move.l	d0,work2
	move.l	d0,a2
	moveq.l	#5,d1
	bsr	trans_dma	*copy to work area

	move.l	d2,d0
	lsr.l	d0		*d0=data count
	exg.l	a1,a2		*a1=destination a2=source
	moveq.l	#1,d6
	move.l	#detune_up,d4
	move.w	#40,work1	*ボリューム値
	bsr	do_pitch2
	move.l	#detune_down,d4
	st	d6
	move.w	#20,work1	*ボリューム値
	bsr	do_pitch2

	bsr	trans_dma	*パラメータはもう設定済み

	move.l	work2(pc),-(sp)
	DOS	_MFREE
	addq.w	#4,sp
	bra	efct3

delay:				*ディレイ処理
	move.l	d2,-(sp)	*get work area
	DOS	_MALLOC
	addq.w	#4,sp
	move.l	d0,mm_error
	bsr	print_em
	bmi	ext_efct	*case:error
	move.l	d0,work2
	move.l	d0,a2
	moveq.l	#5,d1
	bsr	trans_dma	*copy to work area

	pea	(a1)		*keep start data
	move.l	d2,d0
	lsr.l	d0		*d0=data count
	move.w	#10,work1
	moveq.l	#5,d3
	sub.b	sampling_rate(pc),d3
	mulu	#3900,d3
	move.l	d3,d4
	lsr.l	d4
dly_lp:
	adda.l	d3,a2
	sub.l	d4,d0
	beq	dly1
	bmi	dly1
	subq.w	#2,work1
	beq	dly1
	bmi	dly1

	bsr	do_mix2
	bra	dly_lp
dly1:
	move.l	(sp)+,a2	*a2=destination
	move.l	work2(pc),a1	*a1=source
	bsr	trans_dma	*パラメータはもう設定済み

	move.l	work2(pc),-(sp)
	DOS	_MFREE
	addq.w	#4,sp
	bra	efct3
sfx:				*特殊効果処理
	move.l	d2,-(sp)	*get work area
	DOS	_MALLOC
	addq.w	#4,sp
	move.l	d0,mm_error
	bsr	print_em
	bmi	ext_efct	*case:error
	move.l	d0,work2
	move.l	d0,a2
	moveq.l	#5,d1
	bsr	trans_dma	*copy to work area

	move.l	a1,-(sp)	*keep original start addr.
	move.w	#60,work1	*ボリューム値64/128
	exg.l	a1,a2		*a1=destination a2=source
	move.l	d2,d0
	lsr.l	d0		*d0=data count
	moveq.l	#1,d6		*up
	move.l	#65535,d4	*detune
	moveq.l	#5,d3
	sub.b	sampling_rate(pc),d3
	mulu	#400,d3
	move.l	d3,d5
	lsr.l	d5
sfx_lp:
	adda.l	d3,a1
	sub.l	d5,d0
	beq	sfx1
	bmi	sfx1
	subq.w	#4,work1
	beq	sfx1
	bmi	sfx1
	subq.w	#8,d4		*dec parameter

	bsr	do_pitch2
	bra	sfx_lp
sfx1:
	move.l	work2(pc),a1	*a1=source
	move.l	(sp)+,a2	*a2=destination
	bsr	trans_dma	*パラメータはもう設定済み

	move.l	work2(pc),-(sp)
	DOS	_MFREE
	addq.w	#4,sp
	bra	efct3

filting:			*平滑化を行う
	move.l	d2,-(sp)	*d2=size
	DOS	_MALLOC
	addq.w	#4,sp
	move.l	d0,mm_error
	bsr	print_em
	bmi	ext_efct	*case:error
	move.l	d0,work2
	move.l	d0,a2		*dest
	moveq.l	#5,d1		*mode
	bsr	trans_dma

	move.w	(a2)+,d0
	add.w	(a2),d0
	asr.w	d0
	move.w	d0,(a1)+
	subq.l	#2,d2
	beq	exit_filt
	moveq.l	#3,d1
flt_lp:
	cmpi.l	#2,d2
	beq	@f
	move.w	-2(a2),d0
	add.w	(a2)+,d0
	add.w	(a2),d0
	ext.l	d0
	divs	d1,d0
	move.w	d0,(a1)+
	subq.l	#2,d2
	bra	flt_lp
@@:
	move.w	-2(a2),d0
	add.w	(a2)+,d0
	asr.w	d0
	move.w	d0,(a1)+
exit_filt:
	move.l	work2(pc),-(sp)
	DOS	_MFREE
	addq.w	#4,sp
	bra	efct3

chg_env:			*エンベロープ変更
	* < a1.l=pcm data address
	* < d2.l=pcm data size
	* < work2.w=end in level(0-127)
	* < offset.w=offset point(0-65535)
	moveq.l	#0,d5

	cmpi.b	#'2',work3	*check mode
	beq	@f
				*case:fade in
	move.w	work2(pc),d5	*get in level
	moveq.l	#1,d7
	bra	calc_fio
@@:				*case:fade out
	move.w	#128,d5
	moveq.l	#-1,d7
calc_fio:
	move.l	d2,d1
	lsr.l	d1		*fade count
	move.l	#128,d0
	sub.w	work2(pc),d0	*get in/end level
	move.l	d1,d3
	bsr	wari2		*d0.l/d1.l=d0.l...d1.l
	move.w	d0,d2		*step
	move.l	d1,d0
	swap	d0
	clr.w	d0		*d0=あまり*65536
	move.l	d3,d1
	bsr	wari2		*d0.l/d1.l=d0.l...d1.l
	tst.l	d1
	beq	@f
	addq.w	#1,d0
@@:
	move.l	d3,d1
	* < d0.w=rvs
	* < d1.l=count
	* < d2.w=step
	* < d3.b=rvs work
	* < d5.b=now out level
	* < a1.l=address
	tst.b	d7
	bpl	@f
	neg.b	d2		*case:fade out
@@:
	moveq.l	#0,d3		*init rvs work
fio_lp01:
	move.w	(a1),d6
	muls	d5,d6
	asr.l	#7,d6
	move.w	d6,(a1)+
	add.b	d2,d5		*add step
	add.w	d0,d3		*calc revise param.
	bcc	@f
	add.b	d7,d5
@@:
	subq.l	#1,d1
	bne	fio_lp01
	bra	efct3

exit_chen:
	bra	efct3

wari2:				*32ﾋﾞｯﾄ/32ﾋﾞｯﾄ=32ﾋﾞｯﾄ...32ﾋﾞｯﾄ
	* < d0.l/d1.l=d0.l ...d1.l
	cmpi.l	#$ffff,d1
	bls	_divx		*16ビット以下の数値なら１命令で処理
	cmp.l	d0,d1
	beq	_div01		*d0=d1商は１
	bls	_div02		*１命令では無理なケース

	move.l	d0,d1		*商は０余りはd0.l
	moveq.l	#0,d0
	rts
_div01:				*商は１余り０
	moveq.l	#1,d0
	moveq.l	#0,d1
	rts
_div02:
	movem.l	d3-d5,-(sp)
	move.l	d1,d3
	clr.w	d3
	swap	d3
	addq.l	#1,d3
	move.l	d0,d4
	move.l	d1,d5
	move.l	d3,d1
	bsr	_divx
	move.l	d5,d1
	divu	d3,d1
	divu	d1,d0
	andi.l	#$ffff,d0
_div03:
	move.l	d5,d1
	move.l	d5,d3
	swap	d3
	mulu	d0,d1
	mulu	d0,d3
	swap	d3
	add.l	d3,d1
	sub.l	d4,d1
	bhi	_div04
	neg.l	d1
	cmp.l	d1,d5
	bhi	_div05
	addq.l	#1,d0
	bra	_div03
_div04:
	subq.l	#1,d0
	bra	_div03
_div05:
	movem.l	(sp)+,d3-d5
	rts
_divx:
	movem.w	d0/d3,-(sp)
	clr.w	d0
	swap	d0
	divu	d1,d0
	move.w	d0,d3
	move.w	(sp)+,d0
	divu	d1,d0
	swap	d0
	moveq.l	#0,d1
	move.w	d0,d1
	move.w	d3,d0
	swap	d0
	move.w	(sp)+,d3
	rts

do_mix2:
	* mix data1 to data2
	* < a1=source pcm data
	* < a2=dest.  pcm data
	* < d0.l=data count
	movem.l	d0-d7/a0-a6,-(sp)
mix_lp2:
	moveq.l	#0,d2
	move.w	(a1)+,d2
	muls	work1(pc),d2
	asr.l	#7,d2
	add.w	(a2),d2
*	bsr	chk_ovf
	move.w	d2,(a2)+
	subq.l	#1,d0
	bne	mix_lp2
	movem.l	(sp)+,d0-d7/a0-a6
	rts

do_pitch2:			*ミキシングもする
	* < a2=source addr.
	* < a1=destination
	* < d0=number of source data
	* < d4=shift parameter
 	* < d6=direction of pitch shift(-:shift down +:shift up)
	* < work1 (volume 1～128)
	* - ALL
	movem.l	d0-d7/a0-a6,-(sp)
	moveq.l	#0,d7
	moveq.l	#0,d3
	moveq.l	#0,d5

cvt_lp2:
	moveq.l	#0,d2
	move.w	(a2)+,d2
	muls	work1(pc),d2
	asr.l	#7,d2

	tst.b	d5	*初めて
	bne	_up_or_down2
	seq.b	d5
	bra	_wrt_dst_pcm_dt2
_up_or_down2:
	tst.b	d6
	bmi	_do_shift_down2

	move.w	d4,d1
	add.w	d1,d3
	bcc	_check_dsz2
	bra	_wrt_dst_pcm_dt2
_do_shift_down2:
	move.w	d4,d1
	beq	@f
	add.w	d1,d3
	bcc	_wrt_dst_pcm_dt2
@@:
	move.w	d2,d1
	add.w	d7,d1
	asr.w	d1		*線形補間
	add.w	d1,(a1)+	*store additional pcm data
	subq.l	#1,d0
	beq	dop2end
_wrt_dst_pcm_dt2:
	add.w	d2,(a1)+	*store pcm data to buffer
_check_dsz2:
	move.w	d2,d7
	subq.l	#1,d0
	bne	cvt_lp2
dop2end:
	movem.l	(sp)+,d0-d7/a0-a6
	rts

work:
bank_a:	dc.b	' BANK A ',0
bank_b:	dc.b	' BANK B ',0
stpt:	dc.b	'START POINT',0
edpt:	dc.b	'END   POINT',0
mt_on:	dc.b	'MONITOR:',0
on_msg:	dc.b	'ON ',0
of_msg:	dc.b	'OFF',0
dt_tp:	dc.b	'DATA TYPE:',0
tp_pcm:	dc.b	'PCM  ',0
tp_adp:	dc.b	'ADPCM',0
smp_tm:	dc.b	'SAMPLING TIME:',0
sec:	dc.b	$f3,'s',$f3,'e',$f3,'c',0
smp_sz:	dc.b	'SAMPLING SIZE:',0
bytes:	dc.b	$f3,'b',$f3,'y',$f3,'t',$f3,'e',$f3,'s',0
smp_rt:	dc.b	'SAMPLING RATE:',0
khz:	dc.b	$f3,'k',$f3,'H',$f3,'z',0
_39:	dc.b	' 3.9'
_52:	dc.b	' 5.2'
_78:	dc.b	' 7.8'
_104:	dc.b	'10.4'
_156:	dc.b	'15.6'
out_as:	dc.b	'PANPOT:',0
mute:	dc.b	'MUTE  ',0,0
lft:	dc.b	'LEFT  ',0,0
rgt:	dc.b	'RIGHT ',0,0
mdd:	dc.b	'MIDDLE',0,0
at_md:	dc.b	'TRIGGER:',0
quit:	dc.b	'EXIT',0
play:	dc.b	'PLAY',0
rec:	dc.b	'RECORD',0
files:	dc.b	'DISK',0
pitch:	dc.b	'PITCH',0
level:	dc.b	'VOLUME',0
mix:	dc.b	'COMPOUND',0
_dir:	dc.b	'    <DIR> ',0
input_smpt_mes:	dc.b	'For how long will you sample the sound?',0
input_smsz_mes:	dc.b	'How much will ADPCM data use the memory?',0
adj_frq:	dc.b	'ADJUST FREQUENCY',0
bnd_frq:	dc.b	'PITCH-BEND',0
yajirushi:	dc.b	$f3,'H',$f3,'z',' →',0
Hz:		dc.b	$f3,'H',$f3,'z',0
adj_mes1:	dc.b	'Input the source frequency(Hz).',0	*31
adj_mes2:	dc.b	'Input the destination frequency(Hz).',0	*36
sure??:		dc.b	'Are you sure? (Y/N)',0
disk_md_mes:	dc.b	'DISK MODE',0
disk_md_mes2:	dc.b	'Select your job.',0	*16
dm_menu:	dc.b	'1)LOAD  2)SAVE  3)SAVE BY 4 PHASES  4)TEST PLAY',0
pcm_test:	dc.b	' TEST PLAY ',0
load_md_mes_a:	dc.b	' LOAD ADPCM DATA ',0
load_md_mes_p:	dc.b	' LOAD PCM DATA ',0
save_md_mes_a:	dc.b	' SAVE ADPCM DATA ',0
save_md_mes_p:	dc.b	' SAVE PCM DATA ',0
save_4l_mes:	dc.b	' SAVE BY 4 PHASES ',0
save_4l_mes2:	dc.b	'Select function.',0	*17
save_4l_menu:	dc.b	'1)save by 4 levels  2)save by 4 scales',0	*41
save_4l_menu2:	dc.b	'1)direction of minus  2)direction of plus',0
inpt_md_mes:	dc.b	'Input file name,or hit only CR to select from file menu.',0
fn_width:	dc.b	'['
		dcb.b	91,' '
		dc.b	']',0
mem_er_mes:	dc.b	'OUT OF MEMORY',0
fop_er_mes:	dc.b	'FILE OPEN ERROR',0
rd_er_mes:	dc.b	'READ ERROR',0
wr_er_mes:	dc.b	'WRITE ERROR',0
dsk_f_mes:	dc.b	'DISK FULL',0
ast_mes:	dc.b	'****',0
cv_mes:		dc.b	' PROCESSING… ',0
peak_mes:	dc.b	'  WAITING…  ',0
rc_mes:		dc.b	' RECORDING… ',0
pl_mes:		dc.b	' PLAYING… ',0
lvch_mes:	dc.b	'VOLUME CHANGE',0
lvch_mes2:	dc.b	'What percentage do you want to change in? (1%～300%)',0
_pcnt:	dc.b	'%',0
ptch_mes:	dc.b	'PITCH CHANGE',0
ptch_mes2:	dc.b	'How many keys do you want to shift? (-12～12)',0
_ksft:	dc.b	'half tone(s)',0
mix_mes:	dc.b	'COMPOUND',0
mix_mes2:	dc.b	'Decide the direction.',0
copy_mes3:	dc.b	'1)A → B  2)A ← B',0
ins_mes:	dc.b	' INSERT ',0
*atob:	dc.b	'A→B',0
*btoa:	dc.b	'B→A',0
mix_mes4:	dc.b	'Offset count(s)=',0
minus_mes:	dc.b	'OPERATOR CALCULATION',0
minus_mes1:	dc.b	'Select the operator.',0
minus_mes2:	dc.b	'1)ADD 2)SUB 3)OR 4)AND 5)EOR',0
mix_mes3:
minus_mes3:	dc.b	'1)A + B → B  2)B + A → A    ',0
		dc.b	'1)A - B → B  2)B - A → A    ',0
		dc.b	'1)A or B → B  2)B or A → A  ',0
		dc.b	'1)A and B → B  2)B and A → A',0
		dc.b	'1)A eor B → B  2)B eor A → A',0
convol_mes:	dc.b	'CONVOLUTION',0
convol_mes3:	dc.b	'1)A * B → B  2)A * B → A',0
deconvol_mes:	dc.b	'DECONVOLUTION',0
deconvol_mes3:	dc.b	'1)B / A → B  2)A / B → A',0
cpd_mes:	dc.b	'DATA COPY',0
i_adpcmadr:	dc.b	'ADPCM DATA ADDRESS:',0
i_pcmadr:	dc.b	'   PCM DATA ADDRESS:',0
i_size:		dc.b	'   ADPCM DATA SIZE:',0
i_efsz:		dc.b	'EFFECTIVE DATA SIZE:',0
i_start:	dc.b	'       START POINT:',0
i_end:		dc.b	'          END POINT:',0
max_free:	dc.b	'FREE AREA:',0
kakko:		dc.b	'(        )',0
efct_mes:	dc.b	'EFFECTS SOUND',0
efct_mes2:	dc.b	'1)↑/↓  2)←/→  3)ADD 0 TO TAIL  4)REVERSE  5)CHORUS  6)DELAY  7)SFX  8)SMOOTHING  9)ENVELOPE',0
up_down_mes:	dc.b	'UP DOWN SHIFT',0
l_r_mes:	dc.b	'LEFT RIGHT SHIFT',0
ad_mes:		dc.b	"ADD '0' TO TAIL",0
env_mes:	dc.b	'CHANGE AN ENVELOPE',0
howmany0:	dc.b	'How many 0s do you want to add?',0
up_down_mes2:	dc.b	'Input offset value:',0
inout_mes:	dc.b	'Input in/out level:',0
sel_mode_mes:	dc.b	'Decide the mode(1.＜  2.＞):',0
title:		dc.b	$1b,'[36mΖ',$1b,'[35mｖт.х ',$1b,'[37m'
		dc.b	$F3,'V',$F3,'E',$F3,'R',$F3,'S',$F3,'I',$F3,'O',$F3,'N'
		dc.b	$f3,' ',$f3,'2',$f3,'.',$f3,'0',$f3,'6'
author:
		dc.b	$1b,'[m (C) 1991,1992,1993,1994 '
		dc.b	$1b,'[36mZENJI SOFT',$1b,'[m',13,10,0
hlp_mes:	dc.b	$1b,'[37m< USAGE >',$1b,'[m ZVT.X [-･/options...] [file name1] [file name2] [file name3]',13,10
		dc.b	$1b,'[37m< OPTION LIST >',13,10,$1b,'[m'
		dc.b	'-?                   Display the option list.',13,10
		dc.b	'-4<P or V>[+ or -]   Change the pitch or the volume of FILE1 by 4 phases.',13,10
		dc.b	'-A                   Convert 16bit PCM data into ADPCM data.',13,10
		dc.b	'-C                   Convert ADPCM data into 16bit PCM data.',13,10
		dc.b	'-G                   Work at VISUAL MODE by force.',13,10
		dc.b	'-I[offset value]     Insert FILE1 into FILE2, save for FILE3.',13,10
		dc.b	"-M[offset value]     Mix FILE1 and FILE2, save for FILE3.",13,10
		dc.b	'-P[+ or -]<0≦n≦12> Change the pitch of FILE1, and save for FILE2.',13,10
		dc.b	'-V<1%≦n%≦300%>     Change the volume of FILE1, and save for FILE2.',13,10
		dc.b	$1b,'[37m< NOTICE >',13,10
		dc.b	"･You can put '-P' and '-V' at any case.",13,10
		dc.b	'･You can put wild cards on file names.',13,10
		dc.b	$1b,'[m',0
fn_er_mes:	dc.b	$1b,'[47mDid you set parameters properly?',$1b,'[m',13,10,0
outm_er_mes:	dc.b	$1b,'[47mOut of memory.',$1b,'[m',13,10,0
done_it:	dc.b	'Operations are all set.',13,10
		dc.b	'A ',$1b,'[37m','♪SOUND',$1b,'[m mind in a '
		dc.b	$1b,'[37mSOUND',$1b,'[m',' body.',13,10,0
rer_mes:	dc.b	$1b,'[47mFile read error.',$1b,'[m',13,10,0
fop_mes:	dc.b	$1b,'[47mFile open error.',$1b,'[m',13,10,0
wer_mes:	dc.b	$1b,'[47mFile write error.',$1b,'[m',13,10,0
dkf_mes:	dc.b	$1b,'[47mDisk is full up.',$1b,'[m',13,10,0
ger_mes:	dc.b	$1b,'[47mG-RAM is unable to use.',$1b,'[m',13,10,0
proc_mes:	dc.b	$1b,'[46mPROCESSING...',$1b,'[m',13,10,0
input_fn2_mes:	dc.b	'Input file name:',0
monitor_mode:	dc.b	0	*off
data_type:	dc.b	0	*pcm=0(adpcm=1)
sampling_rate:	dc.b	4	*15kHz
out_assign:	dc.b	3	*middle
trigger:	dc.b	0	*off
vol_tbl_down:	dc.b	60,76,88
vol_tbl_up:	dc.b	114,126,140
conv_atop_cmd:	dc.b	0
conv_ptoa_cmd:	dc.b	0
mix_cmd:	dc.b	0
_4_cmd:		dc.b	0
wild_mode:	dc.b	0
files_hjm:	dc.b	0
hk_mode:	dc.b	$ff
no_files:	dc.b	0
_L00009b:
	.dc.b	$1b,'[>1l',$00
_L0000a1:
	.dc.b	$1b,'[m',$00,$00
nameptr:
	dc.b	'*.*',0
ofsbar:	dc.b	'[     :          ] [     :          ] [         :          ]',0
stp0:	dc.b	'START',0
stp1:	dc.b	'POINT',0
edp0:	dc.b	'  END',0
edp1:	dc.b	'POINT',0
efsz0:	dc.b	'EFFECTIVE',0
efsz1:	dc.b	'     SIZE',0
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
frq_tbl:*	case:pitch up
	*	for a=1 to 12
	*	  frq_tbl=(2^-(a/12))*65536
	*	next
	dc.w	61858
	dc.w	58386
	dc.w	55109
	dc.w	52016
	dc.w	49097
	dc.w	46341
	dc.w	43740
	dc.w	41285
	dc.w	38968
	dc.w	36781
	dc.w	34716
	dc.w	32768
frq_tbl2:*	case:pith down
	*	for a=12 to 1 step -1
	*	  frq_tbl=(2^(a/12))*65536-65536
	*	next
	dc.w	0
	dc.w	58179
	dc.w	51236
	dc.w	44682
	dc.w	38496
	dc.w	32657
	dc.w	27146
	dc.w	21944
	dc.w	17034
	dc.w	12400
	dc.w	8026
	dc.w	3897
scl_tbl_down:	dc.w	3897,8026,12400
scl_tbl_up:	dc.w	61858,58386,55109

sel_pnt:	dc.l	0	*!!!
oya_kai:	dc.l	0	*!!!

line_buf:
last_x:	ds.w	1
last_y:	ds.w	1
	ds.w	2
	dc.w	15	*color
	dc.w	$ffff	*line style
kakomi1:
	dc.w	0
	dc.w	0
	dc.w	767
	dc.w	scr_wy*2
	dc.w	1
	dc.w	%11001100_11001100
sen_1:
	dc.w	0
	dc.w	scr_wy
	dc.w	767
	dc.w	scr_wy
	dc.w	1
	dc.w	%11001100_11001100
kakomi2:
	dc.w	0
	dc.w	plus_
	dc.w	767
	dc.w	scr_wy*2+plus_
	dc.w	1
	dc.w	%11001100_11001100
sen_2:
	dc.w	0
	dc.w	scr_wy+plus_
	dc.w	767
	dc.w	scr_wy+plus_
	dc.w	1
	dc.w	%11001100_11001100

rate_tbl:
	dc.w	3900,5200,7800,10400,15600

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

sankaku:		*赤い三角形のカーソルのパターン
	dc.w	16,16
	dc.w	%00011000
	dc.w	%00011000
	dc.w	%00011000
	dc.w	%00011000
	dc.w	%00111100
	dc.w	%00111100
	dc.w	%00111100
	dc.w	%00111100
	dc.w	%01111110
	dc.w	%01111110
	dc.w	%01111110
	dc.w	%01111110
	dc.w	%00000000
	dc.w	%00000000
	dc.w	%00000000
	dc.w	%00000000

last_txt_adas:	dc.l	0
last_txt_adbs:	dc.l	0
last_txt_adae:	dc.l	0
last_txt_adbe:	dc.l	0

fnbfsz:		dc.l	$8000
truncate_cmd:	dc.l	0

pitch_cmd:	dc.b	0,0
volume_cmd:	dc.w	0,0
drive_name:	dcb.b	4,0

disk_mode:				*ディスクモード
	clr.b	use_fm
	movem.l	d0-d7/a0-a6,reg_buff2
	bsr	clr_bottom
	display2	disk_md_mes,%1111,0,28,9
	display2	disk_md_mes2,%0111,0,29,16
	display2	dm_menu,%0111,0,30,47
	moveq.l	#16,d1
	moveq.l	#29,d2
	IOCS	_B_LOCATE
	IOCS	_OS_CURON
	DOS	_INKEY

	move.b	d0,d1
	andi.b	#$DF,d1
	cmpi.b	#'L',d1
	beq	load_mode
	cmpi.b	#'S',d1
	beq	save_mode
	cmpi.b	#'T',d1
	beq	test_mode

	cmpi.b	#'1',d0
	beq	load_mode
	cmpi.b	#'2',d0
	beq	save_mode
	cmpi.b	#'3',d0
	beq	save_4levels
	cmpi.b	#'4',d0
	beq	test_mode
	bra	non_ope
main_scr:			*ﾌｧｲﾙﾒﾆｭｰから帰還した場合
	bsr	g_recover
	bsr	zen_clr		*全画面クリア
	bsr	scr_init
	movem.l	reg_buff2,d0-d7/a0-a6
	bra	complete

g_recover:
	moveq.l	#0,d1
	moveq.l	#0,d2
	move.w	#0,d3
	IOCS	_HOME		*gram疑似復活
	rts

g_giji_clr:
	moveq.l	#0,d1
	moveq.l	#0,d2
	move.w	#512,d3
	IOCS	_HOME		*gram疑似消去
	rts

save_4levels:			*4段階のデータをセーブする
	* X work3
	move.b	data_a(pc),d0
	or.b	data_b(pc),d0
	beq	non_ope		*データはない
	bsr	clr_bottom
	bsr	get_pt1
	display2	save_4l_mes+1,%1111,0,28,16
	display2	save_4l_mes2,%0111,0,29,16
	display2	save_4l_menu,%0111,0,30,41
	moveq.l	#16,d1
	moveq.l	#29,d2
	IOCS	_B_LOCATE
	DOS	_INKEY
	moveq.l	#0,d6
	cmpi.b	#'1',d0
	beq	sv4l_0
	addq.b	#1,d6		*memorize the function
	cmpi.b	#'2',d0
	bne	ext_sv4l
sv4l_0:
	moveq.l	#0,d1
	moveq.l	#29,d2
	IOCS	_B_LOCATE
	IOCS	_B_CLR_ST
	display2	mix_mes2,%0111,0,29,22
	display2	save_4l_menu2,%0111,0,30,44
	moveq.l	#22,d1
	moveq.l	#29,d2
	IOCS	_B_LOCATE
	DOS	_INKEY
	cmpi.b	#'1',d0
	sne	dirofchg	*memorize the direction 0=1(minus) ff=2(plus)
	beq	sv4l_1
	cmpi.b	#'2',d0
	bne	ext_sv4l
sv4l_1:
	lea	save_4l_mes(pc),a0	*後でファイルセレクトの時に最上部に表示
	moveq.l	#0,d1
	bsr	input_fn
	move.l	a1,d0
	beq	exit_sv_md
	move.l	a1,fn_adr
	bsr	dsp_cv_mes

	lea	address1(pc),a6
	tst.b	bank_sel
	beq	by4l0

	tst.b	data_b		*bank_b??
	beq	ext_by4l	*dataが無いなら…
	lea	20(a6),a6		*bank_b base addr.
	bra	by4l1
by4l0:
	tst.b	data_a		*bank_a??
	beq	ext_by4l	*dataが無いなら…
by4l1:
	tst.b	data_type
	beq	copy_pcmdt	*data modeがpcmならpcm dataはあるから…
				*data modeがadpcmの時…
	tst.l	pbuffer(a6)
	bne	copy_pcmdt2	*pcm dataもあるなら…(data typeはadpcm)

	move.l	psize(a6),d2
	lsl.l	#2,d2		*d2=pcm data size for work
	move.l	d2,-(sp)	*work用のバッファを確保
	DOS	_MALLOC
	addq.w	#4,sp
	move.l	d0,mm_error
	bsr	print_em
	bmi	ext_by4l
	move.l	d0,work3
	move.l	d0,a1			*a1=destination
	move.l	paddress(a6),a0		*a0=source
	move.l	psize(a6),d0
	movem.l	d6/a1/a6,-(sp)
	bsr	just_adpcm_to_pcm	*adpcm -> pcm
	movem.l	(sp)+,d6/a1/a6
	move.l	pstart(a6),d0
	sub.l	paddress(a6),d0
	lsl.l	#2,d0
	adda.l	d0,a1
	move.l	a1,buffer3
	move.l	pend(a6),d0
	sub.l	pstart(a6),d0
	addq.l	#1,d0
	move.l	d0,size3
	bra	do_by4l
copy_pcmdt:			*data mode is pcm
	move.l	pstart(a6),a1
	move.l	pend(a6),d2
	sub.l	a1,d2
	addq.l	#2,d2		*pcm data size
	move.l	d2,d0
	lsr.l	#2,d0
	move.l	d0,size3	*=adpcm data size
	bra	do_copy_pcmdt
copy_pcmdt2:			*data mode is adpcm
	move.l	pstart(a6),d0
	sub.l	paddress(a6),d0
	lsl.l	#2,d0
	move.l	pbuffer(a6),a1
	adda.l	d0,a1		*a1=source
	move.l	pend(a6),d2
	sub.l	pstart(a6),d2
	addq.l	#1,d2
	move.l	d2,size3	*=adpcm data size
	lsl.l	#2,d2		*d2=pcm data size
do_copy_pcmdt:
	move.l	d2,-(sp)	*get pcm work area
	DOS	_MALLOC
	addq.w	#4,sp
	move.l	d0,mm_error
	bsr	print_em
	bmi	ext_by4l
	move.l	d0,work3	*後でmfree
	move.l	d0,a2		*destination
	move.l	d0,buffer3
	moveq.l	#5,d1		*trans mode
	bsr	trans_dma
do_by4l:
	pea	modosu_sv4l(pc)	*push retuen adr
	tst.b	d6
	beq	by4levels
	bra	by4scales

modosu_sv4l:
	move.l	work3(pc),-(sp)
	DOS	_MFREE
	addq.w	#4,sp
ext_sv4l:
	bsr	put_pt1
	tst.b	use_fm
	bne	main_scr
	bra	non_ope

by4levels:			*４段階のボリュームでセーブ
	* < size3,buffer3
	* < (fn_adr)=file name adr

	movem.l	d0-d7/a0-a6,reg_buff3
	move.l	size3(pc),d2
	move.l	d2,-(sp)	*work用のバッファを確保
	DOS	_MALLOC
	addq.w	#4,sp
	move.l	d0,mm_error
	bsr	print_em
	bmi	ext_by4l_
	move.l	d0,address3	*save work address
	move.l	d0,a0		*destination
	move.l	buffer3(pc),a1	*source
	move.l	size3(pc),d0	*size
	bsr	pcm_to_adpcm	*adpcmデータへ変換
	move.b	#'4',fn_sj
	bsr	make_file_name
	move.l	fn_adr(pc),a1	*file name adr
	move.l	address3(pc),a2	*data adr
	move.l	size3(pc),d1	*d1=size
	bsr	write_data	*基本データをまずセーブ
	bsr	print_em
	bmi	ext_by4l	*case error

	tst.b	dirofchg
	bne	dir_up
	lea	vol_tbl_down(pc),a3	*case down
	bra	by4l_start
dir_up:				*case up
	lea	vol_tbl_up(pc),a3
by4l_start:			*４段階のボリューム値でセーブ
	moveq.l	#3-1,d7		*loop counter
	move.b 	#'3',fn_sj
by4l_lp:
	movem.l	d7/a3,-(sp)	*push counter

	move.l	buffer3(pc),a1
	move.l	size3(pc),d0
	add.l	d0,d0		*d0=pcm data count
	move.b	(a3,d7.w),d6
	bsr	do_level	*ボリューム変更処理へ

	move.l	buffer3(pc),a1
	move.l	address3(pc),a0
	move.l	size3(pc),d0
	bsr	pcm_to_adpcm	*adpcmデータへ変換

	movem.l	(sp)+,d7/a3

	bsr	make_file_name
	move.l	fn_adr(pc),a1	*file name adr
	move.l	address3(pc),a2	*data adr
	move.l	size3(pc),d1	*d1=size
	bsr	write_data
	bsr	print_em
	bmi	ext_by4l	*case error

	subq.b	#1,fn_sj
	dbra	d7,by4l_lp
ext_by4l:
	move.l	address3(pc),-(sp)
	DOS	_MFREE
	addq.w	#4,sp
ext_by4l_:
	movem.l	reg_buff3(pc),d0-d7/a0-a6
	rts

by4scales:			*４段階のスケールでセーブ
	* < size3,buffer3
	* < (fn_adr)=file name adr

	movem.l	d0-d7/a0-a6,reg_buff3
	move.l	size3(pc),d2
	not.b	dirofchg
	beq	get_memwk
	move.l	d2,d0
	lsr.l	d0
	add.l	d0,d2		*1.5倍取る
get_memwk:
	move.l	d2,-(sp)	*work用のバッファを確保
	DOS	_MALLOC
	addq.w	#4,sp
	move.l	d0,mm_error
	bsr	print_em
	bmi	ext_by4l_
	move.l	d0,address3	*save work address

	add.l	d2,d2
	move.l	d2,d0
	add.l	d2,d2		*d2=pcm data size
	tst.b	dirofchg
	beq	get_memwk_
	add.l	d0,d2		*pcm dataの1.5倍取る
get_memwk_:
	move.l	d2,-(sp)
	DOS	_MALLOC
	addq.w	#4,sp
	move.l	d0,mm_error
	bsr	print_em
	bmi	ext_by4l
	move.l	d0,work2	*pitchを変更したＰＣＭデータの格納位置

	move.l	size3(pc),d0	*size
	move.l	buffer3(pc),a1	*source
	move.l	address3(pc),a0	*destination
	bsr	pcm_to_adpcm	*adpcmデータへ変換
	move.b	#'4',fn_sj
	bsr	make_file_name
	move.l	fn_adr(pc),a1	*file name adr
	move.l	address3(pc),a2	*data adr
	move.l	size3(pc),d1	*d1=size
	bsr	write_data	*基本データをまずセーブ
	bsr	print_em
	bmi	ext_by4s	*case error

	move.b	dirofchg(pc),d6
	beq	dir_up_sc	*-1=pitch down,0=pitch up
	lea	scl_tbl_down(pc),a3	*case down
	bra	by4s_start
dir_up_sc:			*case up
	lea	scl_tbl_up(pc),a3
by4s_start:			*４段階のボリューム値でセーブ
	moveq.l	#3-1,d7		*loop counter
	move.b 	#'3',fn_sj
by4s_lp:
	move.l	buffer3(pc),a2	*source pcm addr.
	move.l	work2(pc),a1	*destination pcm addr.
	move.l	size3(pc),d0
	add.l	d0,d0		*d0=pcm data count
	move.w	(a3)+,d4	*d4=parameter (d6は頭で設定済み)
	bsr	do_pitch	*ボリューム変更処理へ

	move.l	a1,d0
	lsr.l	#2,d0		*size of source pcm data
	movem.l	d0/d6-d7/a3,-(sp)	*push counter
	move.l	work2(pc),a1	*source
	move.l	address3(pc),a0	*destination
	bsr	pcm_to_adpcm	*adpcmデータへ変換
	movem.l	(sp)+,d0/d6-d7/a3

	bsr	make_file_name
	move.l	fn_adr(pc),a1	*file name adr
	move.l	address3(pc),a2	*data adr
	move.l	d0,d1		*d1=size
	bsr	write_data
	bsr	print_em
	bmi	ext_by4s	*case error

	subq.b	#1,fn_sj
	dbra	d7,by4s_lp
ext_by4s:
	move.l	work2(pc),d0
	move.l	d0,-(sp)	*destination pcm data areaの開放
	DOS	_MFREE
	addq.w	#4,sp
	bra	ext_by4l

make_file_name:			*ファイルネームに数字を挿入
	* < fn_adr = file name adr.
	* < fn_sj  =数字
	* - all
	movem.l	d0-d1/a0,-(sp)
	movea.l	fn_adr(pc),a0
	moveq.l	#7,d1
mfn_lp:
	move.b	(a0)+,d0
	beq	mfn1		*0なら…
	cmpi.b	#'.',d0
	beq	mfn1
	dbra	d1,mfn_lp
	addq.w	#1,a0
mfn1:
	subq.w	#1,a0
	move.b	#'.',(a0)+
	move.b	fn_sj(pc),(a0)+	*set mark
	clr.b	(a0)+		*set end mark
	movem.l	(sp)+,d0-d1/a0
	rts

save_mode:
	move.b	data_type(pc),dt_mem	*保存
	lea	param_mem(pc),a1
	move.l	start_point1(pc),(a1)+
	move.l	end_point1(pc),(a1)+
	move.l	start_point2(pc),(a1)+
	move.l	end_point2(pc),(a1)+
	move.b	data_a(pc),d0
	or.b	data_b(pc),d0
	beq	non_ope		*データはない
	bsr	clr_bottom
	lea	save_md_mes_p(pc),a0	*後でファイルセレクトの時に最上部に表示
	moveq.l	#13-1,d4
	tst.b	data_type
	beq	@f
	lea	save_md_mes_a(pc),a0	*後でファイルセレクトの時に最上部に表示
	moveq.l	#15-1,d4
@@:
	lea.l	1(a0),a1
	moveq.l	#%1111,d1
	moveq.l	#0,d2
	moveq.l	#28,d3
	IOCS	_B_PUTMES
	moveq.l	#1,d1
	bsr	input_fn
	cmpa.l	#0,a1
	beq	exit_sv_md

	bsr	ext_chk

	lea	address1(pc),a0
	tst.b	bank_sel
	beq	sv1
	lea	20(a0),a0
	tst.b	data_b
	beq	exit_sv_md
	bra	sv11
sv1:
	tst.b	data_a
	beq	exit_sv_md
sv11:				*a0=base address
	move.l	pend(a0),d1
	move.l	pstart(a0),a2
	sub.l	a2,d1		*get size
	move.b	dt_mem(pc),d0
	cmp.b	data_type(pc),d0
	beq	@f
	bsr	modechg_sv
	move.l	pend(a0),d1
	move.l	pstart(a0),a2
	sub.l	a2,d1		*get size
	addq.l	#1,d1
	tst.b	data_type
	bne	dosvwr
	addq.l	#3,d1
	bra	dosvwr
@@:
	addq.l	#1,d1
	tst.b	data_type
	bne	dosvwr
	addq.l	#1,d1
dosvwr:
	bsr	write_data	*d1=size,a2=addr,a1=filename
	bsr	print_em
	bmi	exit_sv_md	*case error
exit_sv_md:
	move.b	dt_mem(pc),data_type
	lea	param_mem(pc),a1
	move.l	(a1)+,start_point1
	move.l	(a1)+,end_point1
	move.l	(a1)+,start_point2
	move.l	(a1)+,end_point2
	tst.b	use_fm
	bne	main_scr
	bra	non_ope

ext_chk:			*拡張子のチェック
	* X d0.l
	pea	(a1)
@@:
	move.b	(a1)+,d0
	beq	@f
	cmpi.b	#'.',d0
	bne	@b
	move.b	(a1)+,d0
	bsr	mk_capital0
	cmpi.b	#'P',d0
	bne	@f
	move.b	(a1)+,d0
	bsr	mk_capital0
	cmpi.b	#'C',d0
	bne	sv_p16?
	move.b	(a1)+,d0
	bsr	mk_capital0
	cmpi.b	#'M',d0
	bne	@f
	st.b	data_type
	bra	@f
sv_p16?:
	cmpi.b	#'1',d0
	bne	@f
	move.b	(a1)+,d0
	cmpi.b	#'6',d0
	bne	@f
	clr.b	data_type
@@:
	move.l	(sp)+,a1
	rts

modechg_sv:
	tst.b	bank_sel
	bne	modechg_ldb
	bra	modechg_lda

load_mode:
	bsr	clr_bottom
	lea	load_md_mes_p(pc),a0	*後でファイルセレクトの時に最上部に表示
	moveq.l	#13-1,d4
	move.b	data_type(pc),dt_mem
	beq	@f
	lea	load_md_mes_a(pc),a0	*後でファイルセレクトの時に最上部に表示
	moveq.l	#15-1,d4
@@:
	lea.l	1(a0),a1
	moveq.l	#%1111,d1
	moveq.l	#0,d2
	moveq.l	#28,d3
	IOCS	_B_PUTMES
	moveq.l	#0,d1
	bsr	input_fn
	cmpa.l	#0,a1
	beq	exit_ld_md
	tst.b	bank_sel	*which bank?
	bne	case_bank_b
*case_bank_a:				*bank_aの場合
	bsr	open_check
	bsr	print_em
	bmi	exit_ld_md	*case error

	move.l	a1,-(sp)
	move.l	a1,-(sp)

	clr.b	data_a
	bsr	mfree_adpcm_a
	bsr	mfree_pcm_a
	bsr	waku1		*!!
	move.l	(sp)+,a1
	lea	bank_a_fn(pc),a0
	move.b	#' ',(a0)+
stfna_lp:
	move.b	(a1)+,d0
	move.b	d0,(a0)+
	beq	@f
	cmpi.b	#'.',d0
	bne	stfna_lp
	move.l	a0,a2
	bra	stfna_lp
@@:
	move.l	(sp)+,a1
				*read data to BANK A
	bsr	read_data	*d1=size,d2=addr.
	bsr	print_em
	bmi	exit_ld_md	*case error
	move.b	(a2)+,d0
	bsr	mk_capital0
	cmpi.b	#'P',d0
	bne	@f
	move.b	(a2)+,d0
	bsr	mk_capital0
	cmpi.b	#'C',d0
	bne	a_p16?
	move.b	(a2)+,d0
	bsr	mk_capital0
	cmpi.b	#'M',d0
	bne	@f
	st.b	data_type
	bra	a_adpcm
a_p16?:
	cmpi.b	#'1',d0
	bne	@f
	move.b	(a2)+,d0
	cmpi.b	#'6',d0
	bne	@f
	clr.b	data_type
	bra	sv_buf1
@@:
	tst.b	data_type
	beq	sv_buf1
a_adpcm:
	move.l	d1,size1
	beq	exit_ld_md
	move.l	d2,address1	*読み込んだのはadpcm data
	st.b	data_type
	bsr	new_bank_a
	bra	exit_ld_md
sv_buf1:
	lsr.l	#2,d1		*pcmデータのサイズはadpcmのサイズに直す
	move.l	d1,size1
	beq	exit_ld_md
	move.l	d2,buffer1	*読み込んだのはpcm data
	clr.b	data_type
	bsr	new_bank_a
	bra	exit_ld_md

case_bank_b:			*bank_bの場合
	bsr	open_check
	bsr	print_em
	bmi	exit_ld_md	*case error

	move.l	a1,-(sp)
	move.l	a1,-(sp)

	clr.b	data_b
	bsr	mfree_adpcm_b
	bsr	mfree_pcm_b
	bsr	waku2		*!!
	move.l	(sp)+,a1
	lea	bank_b_fn(pc),a0
	move.b	#' ',(a0)+
stfnb_lp:
	move.b	(a1)+,d0
	move.b	d0,(a0)+
	beq	@f
	cmpi.b	#'.',d0
	bne	stfnb_lp
	move.l	a0,a2
	bra	stfnb_lp
@@:
	move.l	(sp)+,a1
				*read data to BANK B
	bsr	read_data	*d1=size,d2=addr.
	bsr	print_em
	bmi	exit_ld_md	*case error
	move.b	(a2)+,d0
	bsr	mk_capital0
	cmpi.b	#'P',d0
	bne	@f
	move.b	(a2)+,d0
	bsr	mk_capital0
	cmpi.b	#'C',d0
	bne	b_p16?
	move.b	(a2)+,d0
	bsr	mk_capital0
	cmpi.b	#'M',d0
	bne	@f
	st.b	data_type
	bra	b_adpcm
b_p16?:
	cmpi.b	#'1',d0
	bne	@f
	move.b	(a2)+,d0
	cmpi.b	#'6',d0
	bne	@f
	clr.b	data_type
	bra	sv_buf2
@@:
	tst.b	data_type
	beq	sv_buf2
b_adpcm:
	move.l	d1,size2
	beq	exit_ld_md
	move.l	d2,address2
	st.b	data_type
	bsr	new_bank_b
	bra	exit_ld_md
sv_buf2:
	lsr.l	#2,d1
	move.l	d1,size2
	beq	exit_ld_md
	move.l	d2,buffer2
	clr.b	data_type
	bsr	new_bank_b
exit_ld_md:
	move.b	dt_mem(pc),d0
	cmp.b	data_type(pc),d0
	beq	@f
	bsr	modechg_lda
	bsr	modechg_ldb
@@:
	tst.b	use_fm
	bne	main_scr
	bra	non_ope

modechg_lda:
	tst.b	data_a
	beq	ext_chg_tp_		*データエリアに何もない…
	tst.b	data_type
	bne	ad_ready_a?_
	move.l	buffer1(pc),d0
	bne	@f			*pcmデータはある
	bsr	make_pcm_a
	bmi	@f			*case:error
*	movem.l	d0-d7/a0-a6,-(sp)
*	move.l	size1(pc),d6
*	movea.l	buffer1(pc),a6
*	move.w	#scr_wy,genten_y
*	bsr	waku1
*	bsr	dsp_wv
*	movem.l	(sp)+,d0-d7/a0-a6
@@:
	bra	calc_es_pcm_a
ad_ready_a?_:
	move.l	address1(pc),d0
	bne	@f			*両方あるよ
	bsr	make_adpcm_a
@@:
	bra	calc_es_adpcm_a
ext_chg_tp_:
	rts

modechg_ldb:
	tst.b	data_b
	beq	ext_chg_tp_		*データエリアに何もない…
	tst.b	data_type
	bne	ad_ready_b?_
	move.l	buffer2(pc),d0
	bne	@f			*pcmデータはある
	bsr	make_pcm_b
	bmi	@f			*case:error
*	movem.l	d0-d7/a0-a6,-(sp)
*	move.l	size2(pc),d6
*	movea.l	buffer2(pc),a6
*	move.w	#scr_wy+plus_,genten_y
*	bsr	waku2
*	bsr	dsp_wv
*	movem.l	(sp)+,d0-d7/a0-a6
@@:
	bra	calc_es_pcm_b
ad_ready_b?_:
	move.l	address2(pc),d0
	bne	@f			*両方あるよ
	bsr	make_adpcm_b
@@:
	bra	calc_es_adpcm_b

new_bank_a:		*BANK Aに新しいデータが入った時
	bsr	waku1
	clr.l	start_point1
	clr.l	end_point1
	st	data_a		*データ入りました…

	tst.b	data_type
	bne	do_dispwv_a_2	*読み込んだのがadpcmデータなら波形は表示しない

	move.l	size1(pc),d1
	lsl.l	#2,d1
	move.l	d1,-(sp)
	DOS	_MALLOC
	addq.w	#4,sp
	move.l	d0,mm_error
	bsr	print_em
	beq	do_dispwv_a
	bsr	mfree_adpcm_a	*エラーの場合
	bsr	mfree_pcm_a
	clr.b	data_a
	rts
do_dispwv_a:
	movea.l	buffer1(pc),a6
	move.l	size1(pc),d6
	move.w	#85,genten_y
	bsr	dsp_wv
	bra	byebye_a
do_dispwv_a_2:
	movea.l	address1(pc),a6
	move.l	size1(pc),d6
	move.w	#85,genten_y
	bsr	dsp_wv_2
byebye_a:
	rts
new_bank_b:		*BANK Bに新しいデータが入った時
	bsr	waku2
	clr.l	start_point2
	clr.l	end_point2
	st	data_b		*データ入りました…

	tst.b	data_type
	bne	do_dispwv_b_2	*読み込んだのがadpcmデータなら波形は表示しない

	move.l	size2(pc),d1
	lsl.l	#2,d1
	move.l	d1,-(sp)
	DOS	_MALLOC
	addq.w	#4,sp
	move.l	d0,mm_error
	bsr	print_em
	beq	do_dispwv_b
	bsr	mfree_adpcm_b	*エラーの場合
	bsr	mfree_pcm_b
	clr.b	data_b
	rts
do_dispwv_b:
	movea.l	buffer2(pc),a6
	move.l	size2(pc),d6
	move.w	#scr_wy+plus_,genten_y
	bsr	dsp_wv
	bra	byebye_b
do_dispwv_b_2:
	movea.l	address2(pc),a6
	move.l	size2(pc),d6
	move.w	#scr_wy+plus_,genten_y
	bsr	dsp_wv_2
byebye_b:
	rts

mfree_adpcm_a:			*clear bank_a adpcm area
	move.l	address1(pc),d0
	beq	ext_maa
	move.l	d0,-(sp)
	DOS	_MFREE
	addq.w	#4,sp
	clr.l	address1
ext_maa:
	rts
mfree_adpcm_b:			*clear bank_b adpcm area
	move.l	address2(pc),d0
	beq	ext_mab
	move.l	d0,-(sp)
	DOS	_MFREE
	addq.w	#4,sp
	clr.l	address2
ext_mab:
	rts
mfree_pcm_a:			*clear bank_a pcm area
	move.l	buffer1(pc),d0
	beq	ext_mpa
	move.l	d0,-(sp)
	DOS	_MFREE
	addq.w	#4,sp
	clr.l	buffer1
ext_mpa:
	rts
mfree_pcm_b:			*clear bank_b pcm area
	move.l	buffer2(pc),d0
	beq	ext_mpb
	move.l	d0,-(sp)
	DOS	_MFREE
	addq.w	#4,sp
	clr.l	buffer2
ext_mpb:
	rts

input_fn:
	* < d1.l≠0　上書き確認必要
	* < d1.l＝1　上書き確認なし
	* > use_fm=1 file menuを使った
	* > a1=file name addr.
	* > a1=0 then cansel
	move.l	d1,-(sp)
	clr.b	use_fm
	moveq.l	#0,d1
	moveq.l	#29,d2
	IOCS	_B_LOCATE
	IOCS	_B_CLR_ST
	display2	inpt_md_mes,%0111,0,29,60
	display2	fn_width,%0111,0,30,91+2
	moveq.l	#1,d1
	moveq.l	#30,d2
	IOCS	_B_LOCATE
	IOCS	_OS_CURON

	lea	input_buffer(pc),a1
	move.b	#91,(a1)	*文字数
	pea	(a1)
	DOS	_GETS		*一行入力
	addq.w	#4,sp

	addq.w	#2,a1		*chr data buffer
	IOCS	_OS_CUROF

	move.l	(sp)+,d1
	move.l	#nameptr,work3
	cmpi.b	#$20,(a1)
	bls	i_from_m
	move.l	a1,work3
	bsr	check_wild
	bmi	i_from_m
	tst.l	d1
	beq	exit_ifn
	bsr	open_check
	tst.l	fo_error
	bmi	exit_ifn	*file not exist case
*	bsr	clr_bottom
	movem.l	d1-d4/a1,-(sp)
	display2	sure??,%0111,0,29,19
	moveq.l	#19,d1
	moveq.l	#29,d2
	IOCS	_B_LOCATE
	moveq.l	#0,d1
	IOCS	_B_ERA_ST
	IOCS	_OS_CURON
	DOS	_INKEY
	movem.l	(sp)+,d1-d4/a1
	move.b	d0,d1
	bsr	mk_capital1
	cmpi.b	#'Y',d1
	beq	exit_ifn
	cmpi.b	#$0d,d0
	beq	exit_ifn
	suba.l	a1,a1
exit_ifn:
	rts

check_wild:			*ワイルドカードがあるか
	* > minus = wild / zero = not wild
	* - ALL
	movem.l	d0/a1,-(sp)
cw_lp:
	move.b	(a1)+,d0
	beq	bye_cw_no	*file name end
*	cmpi.b	#'\',d0
*	beq	mukou_cw
	cmpi.b	#'*',d0
	beq	bye_cw_yes
	cmpi.b	#'?',d0
	bne	cw_lp
bye_cw_yes:
	moveq.l	#-1,d0
	movem.l	(sp)+,d0/a1
	rts
bye_cw_no:
	moveq.l	#0,d0
	movem.l	(sp)+,d0/a1
	rts
mukou_cw:
	move.l	#nameptr,work3
	bra	bye_cw_yes

i_from_m:			*ファイルメニューよりファイル選択
	* < a0=mode mes
	movem.l	d0-d7/a2-a6,reg_buff3
	bsr	zen_clr
	move.l	a0,a1		*モード名の表示
@@:
	tst.b	(a0)+
	bne	@b
	move.l	a0,d4
	sub.l	a1,d4
	subq.w	#2,d4
	moveq.l	#%1101,d1
	moveq.l	#0,d2
	moveq.l	#0,d3
	IOCS	_B_PUTMES
	bsr	file_sel
	st	use_fm
	movem.l	reg_buff3(pc),d0-d7/a2-a6
	rts

data_clr:			*データのクリア
	movem.l	d0-d7/a0-a6,reg_buff2
	moveq.l	#0,d7
	lea	address1(pc),a0
	tst.b	bank_sel
	beq	dc1
	lea	20(a0),a0
dc1:
	bsr	clr_bottom
	display2	sure??,%0111,0,28,19
	moveq.l	#19,d1
	moveq.l	#28,d2
	IOCS	_B_LOCATE
	IOCS	_OS_CURON
	DOS	_INKEY
	move.b	d0,d1
	bsr	mk_capital1
	cmpi.b	#'Y',d1
	beq	do_clr
	cmpi.b	#$0d,d0
	beq	do_clr
ext_dc:
	IOCS	_OS_CUROF
	bsr	scr_init
	movem.l	reg_buff2(pc),d0-d7/a0-a6
	bra	complete
do_clr:
	tst.b	bank_sel
	bne	clr_b

	bsr	mfree_adpcm_a	*clear bank a
	bsr	mfree_pcm_a
	clr.b	data_a
	bsr	waku1
	bra	clr_wk
clr_b:
	bsr	mfree_adpcm_b	*clear bank b
	bsr	mfree_pcm_b
	clr.b	data_b
	bsr	waku2
clr_wk:
	clr.l	(a0)
	clr.l	4(a0)
	clr.l	8(a0)
	clr.l	12(a0)
	clr.l	16(a0)
	bra	ext_dc

all_end2:			*終了?
	movem.l	d0-d7/a0-a6,reg_buff2
	bsr	clr_bottom
	display2	sure??,%0111,0,28,19
	moveq.l	#19,d1
	moveq.l	#28,d2
	IOCS	_B_LOCATE
	IOCS	_OS_CURON
	DOS	_INKEY
	move.b	d0,d1
	bsr	mk_capital1
	cmpi.b	#'Y',d1
	beq	all_end_
	cmpi.b	#$0d,d0
	beq	all_end_
non_ope:
	IOCS	_OS_CUROF
	bsr	scr_init
	movem.l	reg_buff2(pc),d0-d7/a0-a6
	bra	complete

all_end:			*全ての作業を終了??
	tst.b	ESC_ON
	beq	m_lp
all_end_:
*	画面関係初期化プログラム by Y.SUZUKI
	bsr	zen_clr
	move.w	brk_wk(pc),-(sp)
	DOS	_BREAKCK
	addq.w	#2,sp
	move.w	#$0010,d1
	IOCS	_CRTMOD
	move.b	#-$02,d1
	IOCS	_CONTRAST
	IOCS	_B_CURON
	clr.l	d1
	move.l	#$005f001e,d2
	IOCS	_B_CONSOL
	IOCS	_MS_CUROF
	clr.l	d1
	move.l	#$02ff01ff,d2
	IOCS	_MS_LIMIT
	moveq.l	#$ff,d1
	IOCS	_SKEY_MOD
	pea.l	_L00009b(pc)
	DOS	_PRINT
	addq.w	#4,a7
	pea.l	_L0000a1(pc)
	DOS	_PRINT
	addq.w	#4,a7

	lea	ent_path(pc),a0
	move.w	(a0)+,-(sp)
	DOS	_CHGDRV
	addq.w	#2,sp

	pea	(a0)
	DOS	_CHDIR
	addq.w	#4,sp

	bsr	init_kbuf
bye_bye:
	tst.b	monitor_mode
	beq	goto_bv
	move.b	#$08,$e92003
	move.b	#%0000_0001,$e92001	*adpcm all end
goto_bv:
	bsr	back_vect

	pea	0.w
	DOS	_MFREE
	addq.w	#4,sp

	IOCS	_OS_CURON

	movea.l	ssp(pc),a1
	IOCS	_B_SUPER

	move.l	org_sp(pc),sp	*スタックを戻す

	DOS	_EXIT		*終了…

use_gram??:			*グラフィックは使用可能？
	moveq.l	#0,d1
	moveq.l	#-1,d2
	IOCS	_TGUSEMD	*グラフィックの使用状況
	tst.l   d0
	rts

write_data:			*ディスクへの書き込み
	* < d1.l=size
	* < a1.l=file name
	* < a2.l=data address
	* > minus=error
	* - all
	clr.l	rd_error
	movem.l	d0/d5,-(sp)

	bsr	check_pcm?
	bne	create

	move.w	#%0_000_01,-(sp)
	pea	(a1)
	DOS	_OPEN
	addq.w	#6,sp
	tst.l	d0
	bpl	copy_fh
create:
	move.w	#32,-(sp)
	pea	(a1)
	DOS	_CREATE
	addq.w	#6,sp
	move.l	d0,fo_error
	bmi	wr_exit2
copy_fh:
	move.w	d0,d5		*d5.w=file handle

	clr.l	disk_full
	move.l	d1,-(sp)
	pea   	(a2)
	move.w	d5,-(sp)
	DOS	_WRITE
	lea	10(sp),sp
	move.l	d0,wr_error
	bmi	go_close
	cmp.l	d0,d1
	beq	go_close
	move.l	#-1,disk_full
go_close:
	move.w	d5,-(sp)
	DOS	_CLOSE
	addq.w	#2,sp

	tst.l	wr_error
	bmi	do_del
	tst.l	disk_full
	beq	wr_exit2
do_del:
	pea	(a1)		*書き出しエラーの場合は消去
	DOS	_DELETE
	addq.w	#4,sp
wr_exit2:
	movem.l	(sp)+,d0/d5
	rts

check_pcm?:			*ファイルネームがpcmかどうか
	* yes=eq / no=ne
	* - all
	movem.l	d0/a1,-(sp)

	move.b	(a1)+,d0
	bsr	mk_capital0
	cmpi.b	#'P',d0
	bne	bye_cp?
	move.b	(a1)+,d0
	bsr	mk_capital0
	cmpi.b	#'C',d0
	bne	bye_cp?
	move.b	(a1)+,d0
	bsr	mk_capital0
	cmpi.b	#'M',d0
	bne	bye_cp?
	moveq.l	#0,d0
bye_cp?:
	movem.l	(sp)+,d0/a1
	rts

open_check:			*ファイルネームが存在するかチェック
	* < (a1)=file name
	* > minus=error
	clr.l	wr_error
	movem.l	d0/d5/a0-a1,-(sp)
	clr.w	-(sp)
	pea     (a1)
	DOS	_OPEN
	addq.w	#6,sp
	move.l	d0,fo_error
	bmi	rd_exit2
	move.w	d0,d5		*d5.w=file handle
	bsr	get_fsize
	move.l	d0,d1		*d1.l=size
	seq	rd_error
	bra	rd_exit		*illegal file size

read_data:			*ディスクからの読み込み
	* < (a1)=file name
	* > d1.l=size
	* > d2.l=data address
	* > minus=error
	clr.l	wr_error
	movem.l	d0/d5/a0-a1,-(sp)
	lea	rd_fn(pc),a0
	tst.b	drive_name
	beq	rdt1
	move.b	drive_name(pc),(a0)+
	move.b	drive_name+1(pc),(a0)+
rdt1:
	move.b	(a1)+,(a0)+
	bne	rdt1

	clr.w	-(sp)
	pea     rd_fn(pc)
	DOS	_OPEN
	addq.w	#6,sp
	move.l	d0,fo_error
	bmi	rd_exit2
	move.w	d0,d5		*d5.w=file handle
	bsr	get_fsize
	move.l	d0,d1		*d1.l=size
	seq	rd_error
	beq	rd_exit		*illegal file size

	move.l	d0,-(sp)
	DOS	_MALLOC
	addq.w	#4,sp		*d2.l=address
	move.l	d0,mm_error
	bmi	rd_exit
	move.l	d0,d2

	move.l	d1,-(sp)
	move.l	d2,-(sp)
	move.w	d5,-(sp)
	DOS	_READ
	lea	10(sp),sp
	move.l	d0,rd_error
rd_exit:
	move.w	d5,-(sp)
	DOS	_CLOSE
	addq.w	#2,sp
	tst.l	rd_error
	bpl	rd_exit2
	move.l	d2,-(sp)	*読み込みエラーならばメモリを解放して終了
	DOS	_MFREE
	addq.w	#4,sp
	moveq.l	#-1,d0		*minus
rd_exit2:
	movem.l	(sp)+,d0/d5/a0-a1
	rts

get_fsize:
	* < d5.w=file handle
	* > d0.l=file size
	move.w	#2,-(sp)	*ファィルの長さを調べる
	pea	0.w
	move.w	d5,-(sp)
	DOS	_SEEK
 	addq.w	#8,sp		*d0.l=file length

	move.l	d0,-(sp)

	clr.w	-(sp)		*ファイルポインタを元に戻す
	pea	0.w
	move.w	d5,-(sp)
	DOS	_SEEK
	addq.w	#8,sp

	move.l	(sp)+,d0
	rts

g_init:
	moveq.l	#16,d1
	IOCS	_CRTMOD		*768*512/16colors mode
	move.b	author(pc),-(sp)
	clr.b	author
	moveq.l	#38,d1
	moveq.l	#14,d2
	IOCS	_B_LOCATE
	move.w	#2,-(sp)
	pea	title(pc)
	DOS	_FPUTS
	addq.w	#6,sp
	moveq.l	#33,d1
	moveq.l	#16,d2
	IOCS	_B_LOCATE
	move.b	(sp)+,author
	move.w	#2,-(sp)
	pea	author(pc)
	DOS	_FPUTS
	addq.w	#6,sp
	IOCS	_G_CLR_ON
	moveq.l	#15,d1
	IOCS	_APAGE
	IOCS	_VPAGE
	moveq.l	#1,d1
	move.l	#29596,d2
	IOCS	_GPALET
	bsr	g_giji_clr
	bsr	waku1_
	bra	waku2_
window_init:
	moveq.l	#0,d1
	moveq.l	#0,d2
	move.w	#767,d3
	move.w	#511,d4
	IOCS	_WINDOW
	rts
clr_waku1:
	moveq.l	#0,d1
	moveq.l	#0,d2
	move.w	#767,d3
	move.w	#scr_wy*2+1,d4
	IOCS	_WINDOW
	IOCS	_WIPE
*	bsr	window_init
	rts
clr_waku2:
	moveq.l	#0,d1
	move.w	#plus_,d2
	move.w	#767,d3
	move.w	#scr_wy*2+plus_+1,d4
	IOCS	_WINDOW
	IOCS	_WIPE
*	bsr	window_init
	rts
waku1:
	bsr	clr_waku1
	pea	(a0)
	movea.l	last_txt_adas(pc),a0
	bsr	clr_ttsn
	movea.l	last_txt_adae(pc),a0
	bsr	clr_ttsn
	move.l	(sp)+,a0
waku1_:
	lea	kakomi1(pc),a1
	IOCS	_BOX
	lea	sen_1(pc),a1
	IOCS	_LINE
	rts
waku2:
	bsr	clr_waku2
	pea	(a0)
	movea.l	last_txt_adbs(pc),a0
	bsr	clr_ttsn
	movea.l	last_txt_adbe(pc),a0
	bsr	clr_ttsn
	move.l	(sp)+,a0
waku2_:
	lea	kakomi2(pc),a1
	IOCS	_BOX
	lea	sen_2(pc),a1
	IOCS	_LINE
	rts

other_init:
	bsr	zen_clr

	move.w	#-1,-(sp)
	DOS	_BREAKCK
	addq.w	#2,sp
	move.w	d0,brk_wk

	move.w	#2,-(sp)
	DOS	_BREAKCK
	addq.w	#2,sp

	moveq.l	#0,d1
	moveq.l	#0,d2
	IOCS	_SKEY_MOD
	IOCS	_MS_CUROF
	move.b	#-$02,d1
	IOCS	_CONTRAST
	clr.l	d1
	move.l	#$005f001e,d2
	IOCS	_B_CONSOL
	clr.l	d1
	move.l	#$02ff01ff,d2
	IOCS	_MS_LIMIT
	rts
zen_clr:				*テキスト画面の全プレーンクリア
	movem.l	d0-d1/a0,-(sp)
	move.w	$e8002a,-(sp)
	move.w	#%00000001_11110000,$e8002a	*同時アクセス

	lea	$e00000,a0
	move.w	#32*16-1,d0
clr_loop0:
	moveq.l	#24-1,d1
clr_loop1:
	clr.l	(a0)+
	dbra	d1,clr_loop1
	lea	32(a0),a0
	dbra	d0,clr_loop0

	move.w	(sp)+,$e8002a			*同時アクセス解除
	movem.l	(sp)+,d0-d1/a0
	rts
zen_clr2:				*テキスト画面の全プレーンクリア
	movem.l	d0-d1/a0,-(sp)
	move.w	$e8002a,-(sp)
	move.w	#%00000001_11110000,$e8002a	*同時アクセス

	lea	$e00000+$80*16,a0
	move.w	#31*16-1,d0
	bra	clr_loop0

zen_clr3:				*テキスト画面の全プレーンクリア
	movem.l	d0-d1/a0,-(sp)
	move.w	$e8002a,-(sp)
	move.w	#%00000001_11110000,$e8002a	*同時アクセス

	lea	$e00000+$80*32,a0
	move.w	#30*16-1,d0
	bra	clr_loop0

dsp_wv:				*波形の表示(CASE:PCM)
	* < a6=pcm data address
	* < d6=(adpcm) data size
	* < genten_y
	* > d1=steps,d5=_steps
	* X d0-d7,a1
	* - a0,a2-a7
				*screen initialize
	add.l	d6,d6		*pcm count=size*2

	move.w	genten_y(pc),last_y	*init. work area
	move.w	#start_x,d4
	move.w	d4,last_x
	cmp.l	#scr_wx,d6
	bcs	dsp_wv2
				*calc steps
	move.l	d6,d1
	move.l	#scr_wx,d2
	bsr	wari		*d1/d2=d1...d2
	move.l	d1,-(sp)
	move.l	d2,d5
	swap	d5
	clr.w	d5		*=andi.l #$ffff0000,d5(amari*65536)
	move.l	#scr_wx,d2
	move.l	d5,d1
	bsr	wari
	move.l	d1,d5
	tst.l	d2		*余りあるかな?
	beq	calc_steps
	addq.w	#2,d5		*d5=_steps
calc_steps:
	move.l	(sp)+,d1
	bne	make_steps
	moveq.l	#1,d1
make_steps:
	add.l	d1,d1		*d1=steps
	add.l	d6,d6

	moveq.l	#0,d3		*offset
	moveq.l	#0,d7
dsp_wv_lp:
	move.l	d3,d0		*src
	add.w	d5,d7
	bcc	@f
	addq.l	#2,d3
@@:
	addq.w	#1,d4		*inc x
	add.l	d1,d3		*inc steps
	cmp.l	d3,d6
	bhi	@f
	move.l	d6,d3
@@:
	lea	line_buf+8(pc),a1
	clr.w	-(a1)
	move.w	d4,-(a1)
	move.l	(a1),-(a1)
dspwvlp00:
	move.w	(a6,d0.l),d2
	bpl	@f
	cmp.w	2(a1),d2
	bgt	next_dspwvlp00
	move.w	d2,2(a1)
	bra	next_dspwvlp00
@@:
	cmp.w	6(a1),d2
	blt	next_dspwvlp00
	move.w	d2,6(a1)
next_dspwvlp00:
	addq.w	#2,d0
	cmp.l	d0,d3
	bne	dspwvlp00		*区間内値合計
*	bsr	chk_ovf

	moveq.l	#max_b,d0
	move.w	2(a1),d2
	muls	#scr_wy,d2
	asr.l	d0,d2
	neg.w	d2
	add.w	genten_y(pc),d2
	move.w	d2,2(a1)

	move.w	6(a1),d2
	muls	#scr_wy,d2
	asr.l	d0,d2
	neg.w	d2
	add.w	genten_y(pc),d2
	move.w	d2,6(a1)
	IOCS	_LINE

	cmp.l	d3,d6
	bhi	dsp_wv_lp
	rts

dsp_wv2:			*calc steps
	move.l	d6,d2
	move.l	#scr_wx,d1
	bsr	wari		*d1/d2=d1...d2
	move.l	d1,-(sp)
	move.l	d2,d5
	swap	d5
	clr.w	d5		*=andi.l #$ffff0000,d5(amari*65536)
	move.l	d6,d2
	move.l	d5,d1
	bsr	wari
	move.l	d1,d5
	tst.l	d2		*余りあるかな?
	beq	calc_steps_
	addq.w	#1,d5		*d5=_steps
calc_steps_:
	move.l	(sp)+,d1

	add.l	d6,d6

	moveq.l	#0,d3		*offset
	moveq.l	#0,d7
	lea	line_buf(pc),a1
	move.w	genten_y(pc),2(a1)	*y0
dsp_wv_lp_:
	move.w	d4,-(sp)
	add.w	d5,d7
	bcc	@f
	addq.w	#1,d4
@@:
	add.w	d1,d4		*inc x
	move.w	(a6,d3.l),d2	*get pcm val
*	bsr	chk_ovf
	muls	#scr_wy,d2
	moveq.l	#max_b,d0
	asr.l	d0,d2
	neg.w	d2
	add.w	genten_y(pc),d2
	move.w	d2,6(a1)	*y1
	move.w	(sp)+,d2
@@:
	move.w	d2,(a1)		*x0
	move.w	d2,4(a1)	*x1
	IOCS	_LINE
	addq.w	#1,d2
	cmp.w	d2,d4
	bne	@b
	addq.l	#2,d3		*inc step
	cmp.l	d3,d6
	bhi	dsp_wv_lp_
@@:
	rts

dsp_wv_2:			*波形の表示#2(CASE:ADPCM)
	* < a6=adpcm data address
	* < d6=(adpcm) data size
	* < genten_y
	* > d1=steps,d5=_steps
	* X d0-d7,a1
	* - a0,a2-a7
	movem.l	d0-d7/a0-a6,-(sp)
	move.b	hk_mode(pc),d0
	beq	bye_dsp
				*screen initialize
	add.l	d6,d6		*pcm count=size*2

	move.w	genten_y(pc),last_y	*init. work area
	move.w	#start_x,d4
	move.w	d4,last_x
	cmp.l	#scr_wx,d6
	bcs	_dsp_wv2
				*calc steps
	move.l	d6,d1
	move.l	#scr_wx,d2
	bsr	wari		*d1/d2=d1...d2
	move.l	d1,-(sp)
	move.l	d2,d5
	swap	d5
	clr.w	d5		*=andi.l #$ffff0000,d5(amari*65536)
	move.l	#scr_wx,d2
	move.l	d5,d1
	bsr	wari
	move.l	d1,d5
	tst.l	d2		*余りあるかな?
	beq	_calc_steps
	addq.w	#2,d5		*d5=_steps
_calc_steps:
	move.l	(sp)+,d1
	bne	_make_steps
	moveq.l	#1,d1
_make_steps:
	move.l	a6,a2
	lea	line_buf(pc),a1
	lea	scaleval(pc),a5
	lea	levelchg(pc),a6
	moveq.l	#0,d3
	moveq.l	#0,d7
	move.w	d3,last_val-line_buf(a1)
	move.w	#$0f,work3-line_buf(a1)

	move.w	d3,work1-line_buf(a1)
	move.l	#1,work2-line_buf(a1)
_dsp_wv_lp00:
	move.w	d4,(a1)
	clr.w	2(a1)
	move.l	(a1),4(a1)		*init
_dsp_wv_lp01:
	subq.l	#1,d6
	bcs	bye_dsp

	move.b	(a2),d0
	move.w	work3(pc),d2
	and.w	d2,d0
	tst.b	d2
	bpl	@f
	lsr.b	#4,d0
	addq.w	#1,a2
@@:
	not.b	d2
	move.w	d2,work3-line_buf(a1)
	exg	d0,d1
	bsr	calc_pcm_val
	exg	d0,d1
	tst.w	d2
	bpl	@f
	cmp.w	2(a1),d2
	bgt	_next_dspwv2lp
	move.w	d2,2(a1)
	bra	_next_dspwv2lp
@@:
	cmp.w	6(a1),d2
	blt	_next_dspwv2lp
	move.w	d2,6(a1)
_next_dspwv2lp:
	subq.l	#1,work2-line_buf(a1)
	bne	_dsp_wv_lp01
*	bsr	chk_ovf
	moveq.l	#max_b,d0
	move.w	2(a1),d2
	muls	#scr_wy,d2
	asr.l	d0,d2
	neg.w	d2
	add.w	genten_y(pc),d2
	move.w	d2,2(a1)
	move.w	d4,4(a1)

	move.w	6(a1),d2
	muls	#scr_wy,d2
	asr.l	d0,d2
	neg.w	d2
	add.w	genten_y(pc),d2
	move.w	d2,6(a1)
	IOCS	_LINE
	addq.w	#1,d4		*inc x

	add.w	d5,work1-line_buf(a1)
	bcc	@f
	addq.l	#1,work2-line_buf(a1)
@@:
	add.l	d1,work2-line_buf(a1)	*inc steps
	bra	_dsp_wv_lp00
bye_dsp:
	movem.l	(sp)+,d0-d7/a0-a6
	rts

_dsp_wv2:
	move.l	d6,d2
	move.l	#scr_wx,d1
	bsr	wari		*d1/d2=d1...d2
	move.l	d1,-(sp)
	move.l	d2,d5
	swap	d5
	clr.w	d5		*=andi.l #$ffff0000,d5(amari*65536)
	move.l	d6,d2
	move.l	d5,d1
	bsr	wari
	move.l	d1,d5
	tst.l	d2		*余りあるかな?
	beq	_calc_steps_
	addq.w	#2,d5		*d5=_steps
_calc_steps_:
	move.l	(sp)+,d1

	move.l	a6,a2
	lea	line_buf(pc),a1
	lea	scaleval(pc),a5
	lea	levelchg(pc),a6
	moveq.l	#0,d3
	moveq.l	#0,d7
	move.w	d3,last_val-line_buf(a1)
	move.w	#$0f,work3-line_buf(a1)

	move.w	d3,work1-line_buf(a1)
	move.w	genten_y(pc),2(a1)		*y0
_dsp_wv_lp_:
	subq.l	#1,d6
	bcs	bye_dsp
	move.b	(a2),d0
	move.w	work3(pc),d2
	and.w	d2,d0
	tst.b	d2
	bpl	@f
	lsr.b	#4,d0
	addq.w	#1,a2
@@:
	not.b	d2
	move.w	d2,work3-line_buf(a1)
	exg	d0,d1
	bsr	calc_pcm_val
	exg	d0,d1
*	bsr	chk_ovf
	muls	#scr_wy,d2
	moveq.l	#max_b,d0
	asr.l	d0,d2
	neg.w	d2
	add.w	genten_y(pc),d2
	move.w	d2,6(a1)		*y1
	move.w	d4,d2
	add.w	d5,work1-line_buf(a1)
	bcc	@f
	addq.w	#1,d4
@@:
	add.w	d1,d4
@@:
	move.w	d2,(a1)			*x0
	move.w	d2,4(a1)		*x1
	IOCS	_LINE
	addq.w	#1,d2
	cmp.w	d2,d4
	bne	@b
	bra	_dsp_wv_lp_

*chk_ovf:
*	cmp.w	#max/2-1,d2
*	ble	minus_ovf?
*	move.w	#max/2-1,d2
*	bra	ext_chk_ovf
*minus_ovf?:
*	cmp.w	#-max/2,d2
*	bge	ext_chk_ovf
*	move.w	#-max/2,d2
*ext_chk_ovf:
*	rts

mix_pcm:			*２つのＰＣＭデータをミキシングする。
	* < d1=size1,d2=size2
	* < a1=buffer1,a2=buffer2
	* < offset
	* > buffer3,dest_size
	movem.l	d0-d7/a0-a6,-(sp)
	move.l	d1,d3
	add.l	offset(pc),d3
	cmp.l	d2,d3
	bhi	@f
	move.l	d2,d3		*d2の方が大きかったら
@@:
	move.l	d3,dest_size
	lsl.l	#2,d3		*4倍のワークエリアが必要
	move.l	d3,-(sp)
	DOS 	_MALLOC
	addq.w	#4,sp
	move.l	d0,mm_error
	bsr	print_em
	bmi	ext_do_mix
	move.l	d0,buffer3

	movem.l	d1-d2/a1-a2,-(sp)
	move.l	d3,d2		*size
	moveq.l	#%00000101,d1	*mode
	movea.l	a2,a1		*source
	movea.l	buffer3(pc),a2	*dest
	bsr	trans_dma
	movem.l	(sp)+,d1-d2/a1-a2

	move.l	dest_size(pc),d3
	sub.l	d2,d3		*お尻の部分をクリア
	beq	go_mix
	move.l	buffer3(pc),a0
	add.l	d2,d2
	add.l	d2,d2
	add.l	d2,a0
	moveq.l	#0,d0
@@:
	move.l	d0,(a0)+	*いっぺんに２つクリア
	subq.l	#1,d3
	bne	@b
go_mix:
	*a1=use parameter
	movea.l	buffer3(pc),a2
	move.l	offset(pc),d2
	add.l	d2,d2
	add.l	d2,d2
	adda.l	d2,a2
	move.l	d1,d0
	bsr	do_mix
ext_do_mix:
	movem.l	(sp)+,d0-d7/a0-a6
	rts

do_mix:
	* < a1=source pcm data
	* < a2=dest.  pcm data
	* < d0.l=data size
	add.l	d0,d0
	tst.b	mix_mode
	bmi	ins_lp
mix_lp:
	move.w	(a1)+,d2
	add.w	(a2),d2
*	bsr	chk_ovf
	move.w	d2,(a2)+
	subq.l	#1,d0
	bne	mix_lp
	rts
ins_lp:
	move.w	(a1)+,(a2)+
	subq.l	#1,d0
	bne	ins_lp
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

just_adpcm_to_pcm:		*ピッチチェンジやレベルチェンジを
				*行わない単なるADPCM→PCM変換
	* < a0=adpcm data buffer
	* < a1=pcm data buffer
	* < d0.l=data size
	lea	scaleval(pc),a5
	lea	levelchg(pc),a6
	moveq.l	#0,d3
	moveq.l	#0,d7
	moveq.l	#$0f,d4
	add.l	d0,d0
	clr.w	last_val-levelchg(a6)
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

adpcm_to_pcm:				*ＡＤＰＣＭをＰＣＭデータへ変換する
	* < a0=adpcm data buffer
	* < a1=pcm data buffer
	* < d0.l=data size
	* < d1.w=note shift(0～b～c(non shift)～d～18)
	* < d6.l=volume value(0%～200%)
	* > a1=data size
				*パーセンテージを１６進変換
	lea	levelchg(pc),a6
	clr.b	up_down-levelchg(a6)

	move.l	a1,-(sp)
	lsl.l	#7,d6
	divu	#100,d6
	andi.l	#$ffff,d6	*d6=0～200→0～256

	add.w	d1,d1		*d1=d1*2
	cmpi.b	#$18,d1
	beq	est		*ピッチシフトは無しのケース
	bcs	shift_down	*ピッチを下げるケース
*shift_up				*ピッチを上げるケース
	lea	frq_tbl(pc),a5
	move.w	-26(a5,d1.w),frq_flg-frq_tbl(a5)
	move.b	#1,up_down-frq_tbl(a5)	*set switch
	bra	init_wk
shift_down:
	lea	frq_tbl2(pc),a5
	move.w	(a5,d1.w),frq_flg-frq_tbl2(a5)
	st	up_down-frq_tbl2(a5)	*set switch
init_wk:
	clr.w	last_val2-levelchg(a6)
	clr.w	frq_wk-levelchg(a6)
est:
	lea	scaleval(pc),a5
	moveq.l	#0,d3
	moveq.l	#0,d7
	moveq.l	#$0f,d4
	add.l	d0,d0
	clr.w	last_val-levelchg(a6)
	clr.b	hajimete-levelchg(a6)
atp_lp:
	move.b	(a0),d1
	and.l	d4,d1
	tst.b	d4
	bpl	neg_d4
	lsr.b	#4,d1		*get 4bit data
	addq.w	#1,a0
neg_d4:
	not.b	d4
	bsr	calc_pcm_val	*実際の計算

	andi.l	#$ffff,d2	*ボリュームの変更
	muls	d6,d2		*d6にはパーセントが
	asr.l	#7,d2		*入っている

	tst.b	hajimete-levelchg(a6)	*初めて
	bne	up_or_down
	seq.b	hajimete-levelchg(a6)
	bra	wrt_pcm_dt
up_or_down:
	tst.b	up_down-levelchg(a6)
	beq	wrt_pcm_dt	*non pitch shift
	bmi	do_shift_down

	move.w	frq_flg(pc),d1
	add.w	d1,frq_wk-levelchg(a6)
	bcc	check_dsz
	bra	wrt_pcm_dt
do_shift_down:
	move.w	frq_flg(pc),d1
	beq	@f
	add.w	d1,frq_wk-levelchg(a6)
	bcc	wrt_pcm_dt
@@:
	move.w	d2,d1
	add.w	last_val2(pc),d1
	asr.w	d1		*線形補間
	move.w	d1,(a1)+
wrt_pcm_dt:
	move.w	d2,(a1)+	*store pcm data to buffer
check_dsz:
	move.w	d2,last_val2-levelchg(a6)
	subq.l	#1,d0
	bne	atp_lp
	sub.l	(sp)+,a1	*data size
	rts

adpcm_to_8pcm:				*ＡＤＰＣＭを8bitＰＣＭデータへ変換する
	* < a0=adpcm data buffer
	* < a1=pcm data buffer
	* < d0.l=data size
	* < d1.w=note shift(0～b～c(non shift)～d～18)
	* < d6.l=volume value(0%～200%)
	* > a1=data size
				*パーセンテージを１６進変換
	lea	levelchg(pc),a6
	clr.b	up_down-levelchg(a6)

	move.l	a1,-(sp)
	lsl.l	#7,d6
	divu	#100,d6
	andi.l	#$ffff,d6	*d6=0～200→0～256

	add.w	d1,d1		*d1=d1*2
	cmpi.b	#$18,d1
	beq	_8est		*ピッチシフトは無しのケース
	bcs	_8shift_down	*ピッチを下げるケース
*_8shift_up:				*ピッチを上げるケース
	lea	frq_tbl(pc),a5
	move.w	-26(a5,d1.w),frq_flg-frq_tbl(a5)
	move.b	#1,up_down-frq_tbl(a5)	*set switch
	bra	_8init_wk
_8shift_down:
	lea	frq_tbl2(pc),a5
	move.w	(a5,d1.w),frq_flg-frq_tbl2(a5)
	st	up_down-frq_tbl2(a5)	*set switch
_8init_wk:
	clr.w	last_val2-levelchg(a6)
	clr.w	frq_wk-levelchg(a6)
_8est:
	lea	scaleval(pc),a5
	moveq.l	#0,d3
	moveq.l	#0,d7
	moveq.l	#$0f,d4
	add.l	d0,d0
	clr.w	last_val-levelchg(a6)
	clr.b	hajimete-levelchg(a6)
_8atp_lp:
	move.b	(a0),d1
	and.l	d4,d1
	tst.b	d4
	bpl	@f
	lsr.b	#4,d1		*get 4bit data
	addq.w	#1,a0
@@:
	not.b	d4
	bsr	calc_pcm_val	*実際の計算

	andi.l	#$ffff,d2	*ボリュームの変更
	muls	d6,d2		*d6にはパーセントが
	asr.l	#7,d2		*入っている

	tst.b	hajimete-levelchg(a6)	*初めて
	bne	@f
	seq.b	hajimete-levelchg(a6)
	bra	_8wrt_pcm_dt
@@:
	tst.b	up_down-levelchg(a6)
	beq	_8wrt_pcm_dt	*non pitch shift
	bmi	_8do_shift_down

	move.w	frq_flg(pc),d1
	add.w	d1,frq_wk-levelchg(a6)
	bcc	_8check_dsz
	bra	_8wrt_pcm_dt
_8do_shift_down:
	move.w	frq_flg(pc),d1
	beq	@f
	add.w	d1,frq_wk-levelchg(a6)
	bcc	_8wrt_pcm_dt
@@:
	move.w	d2,d1
	add.w	last_val2(pc),d1
	asr.w	d1		*線形補間
	bsr	reduce_8bit
	move.b	d1,(a1)+
_8wrt_pcm_dt:
	move.w	d2,d1
	bsr	reduce_8bit
	move.b	d1,(a1)+	*store pcm data to buffer
_8check_dsz:
	move.w	d2,last_val2-levelchg(a6)
	subq.l	#1,d0
	bne	_8atp_lp
	sub.l	(sp)+,a1	*data size
	rts

reduce_8bit:
	cmpi.w	#-128,d1
	bge	@f
	moveq.l	#-128,d1
	rts
@@:
	cmpi.w	#127,d1
	ble	@f
	moveq.l	#127,d1
@@:
	rts

calc_pcm_val:
	* < d1.b=adpcm value
	* < d7.w=scale level
	* > d2.w=pcm value
	* > d7.w=next scale level
	* > d1.b=adpcm*2
	* X d3 d2

	add.b	d1,d1
calc_pcm_val_:
	add.b	d7,d7
	move.w	(a5,d7.w),d3	*=d
	lsr.b	d7

	move.w	cpv(pc,d1.w),d2
	jmp	cpv(pc,d2.w)
abc:
	add.w	last_val(pc),d2
*	bsr	chk_ovf
	move.w	d2,last_val-levelchg(a6)	*d2=pcmdata

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
	* < d0.l=data size
	* X d0-d5/a1,a2,a5,a6

	lea	scaleval(pc),a5
	lea	levelchg(pc),a6

	moveq.l	#0,d6		*scalelevel=0
	moveq.l	#0,d7
	moveq.l	#0,d4
	add.l	d0,d0
	move.w	d4,last_val-levelchg(a6)	*last_val=0
pta_lp:
	move.w	(a1)+,d3	*d3=pcm data
	bsr	calc_adpcm_val
	not.b	d4
	bne.s	set_lower
				*case upper 4bits
	lsl.b	#4,d1
	or.b	d1,d5
	move.b	d5,(a0)+
	bra	check_cnt
set_lower:
	move.b	d1,d5
check_cnt:
	subq.l	#1,d0
	bne	pta_lp
	rts

calc_adpcm_val:
	* < d3.w=pcm value
	* < d7.w=scale level
	* > d1.b=adpcm value
	* > d7.w=next scale level
	* X
	sub.w	last_val(pc),d3		*d3=diff
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
	lsr.b	d1
	rts

line_ln:	equ	168		*縦線の長さ
txt_ln:				*テキストの(赤い)縦ライン
	* < d0.w=x
	* < d1.w=y
	* < a0.l=del.addr.
	* < a1.l=write plane addr.
	* > a0.l=del.addr.
	bsr	clr_ttsn
	move.l	#$ffff,d2
	and.l	d2,d1
	and.l	d2,d0
	lsl.l	#7,d1		*y=y*$80
	move.w	d0,d2
	andi.b	#$0f,d2
	eori.b	#$0f,d2		*どのビット?
	moveq.l	#0,d3
	bset	d2,d3		*wrt data
	lsr.w	#4,d0		*d0=true x
	lsl.w	d0
	add.l	d1,d0
	adda.l	d0,a1
	pea	(a1)
	move.w	#line_ln,d1
txt_ln_lp:
	move.w	d3,(a1)
	lea	$80(a1),a1
	dbra	d1,txt_ln_lp
	move.l	(sp)+,a0
	rts

clr_ttsn:
	cmpa.l	#0,a0
	beq	@f
	move.w	#line_ln,d2	*ラインを消去
del_lnlp:
	clr.w	(a0)
	lea	$80(a0),a0
	dbra	d2,del_lnlp
@@:
	rts

scr_init:	*コンソール下部の初期化
	bsr	clr_em_area
	bsr	clr_bottom	*画面下部の消去
	moveq.l	#0,d7
	bsr	_bank_a
	bsr	_bank_b
	bsr	_stpt1
	bsr	_edpt1
	bsr	_stpt2
	bsr	_edpt2
	bsr	_mon
	bsr	_dt_tp
	bsr	_smp_tm
	bsr	_sec
	bsr	_smp_sz
	bsr	_bytes
	bsr	_smp_rt
	bsr	_khz
	bsr	_out_as
	bsr	_at_md
	bsr	_quit
	bsr	_play
	bsr	_rec
	bsr	_files
	bsr	_pitch
	bsr	_level
	bsr	_mix

	move.b	monitor_mode(pc),d0
	bsr	mt_on_off
	move.b	data_type(pc),d0
	bsr	dt_type
	move.b	sampling_rate(pc),d0
	bsr	smp_rate
	move.b	out_assign(pc),d0
	bsr	pan_pot
	move.b	trigger(pc),d0
	bsr	trg_on_off

	moveq.l	#0,d0
	move.b	sampling_rate(pc),d0
	lsl.w	d0
	lea	rate_tbl(pc),a0
	move.w	(a0,d0.w),d0		*freq
	move.l	rec_data_size(pc),d1
	add.l	d1,d1			*d1=d1*2(sz=sz*2)
	divu	d0,d1			*d1=time
	bne	disp_st__
	moveq.l	#1,d1			*１秒以下は皆１秒とする
disp_st__:
	moveq.l	#0,d0
	move.w	d1,d0
	move.w	d0,recording_time
	bsr	rec_time
	move.l	rec_data_size(pc),d0
	bsr	data_size
	bsr	init_hex_area_a
	bsr	init_hex_area_b
	rts
init_hex_area_a:			*１６進数表示エリアを初期化
	movem.l	d0-d7/a0-a6,-(sp)

	moveq.l	#12,d1		*x
	moveq.l	#11,d2		*y
	IOCS	_B_LOCATE
	moveq.l	#0,d1
	IOCS	_B_ERA_ST

	moveq.l	#12,d1		*x
	moveq.l	#12,d2		*y
	IOCS	_B_LOCATE
	moveq.l	#0,d1
	IOCS	_B_ERA_ST

	moveq.l	#0,d6
	moveq.l	#'5',d0
	bsr	data_move_

	addq.b	#1,d6
	moveq.l	#'5',d0
	tst.l	end_point1
	bne	do_dm_a
	moveq.l	#'.',d0
do_dm_a:
	bsr	data_move_
	bsr	ofsst1
	bsr	ofsed1

	move.w	$e8002a,d6
	move.w	#%00000001_1100_0000,$e8002a	*同時アクセス

	move.w	#52*8+4,d1
	move.w	#13*16,d2
	lea	sankaku(pc),a1
	IOCS	_TEXTPUT

 	move.w	d6,$e8002a			*同時アクセス解除
	movem.l	(sp)+,d0-d7/a0-a6
	rts

init_hex_area_b:			*１６進数表示エリアを初期化
	movem.l	d0-d7/a0-a6,-(sp)

	moveq.l	#12,d1		*x
	moveq.l	#25,d2		*y
	IOCS	_B_LOCATE
	moveq.l	#0,d1
	IOCS	_B_ERA_ST

	moveq.l	#12,d1		*x
	moveq.l	#26,d2		*y
	IOCS	_B_LOCATE
	moveq.l	#0,d1
	IOCS	_B_ERA_ST

	moveq.l	#2,d6
	moveq.l	#'5',d0
	bsr	data_move_

	addq.b	#1,d6
	moveq.l	#'5',d0
	tst.l	end_point2
	bne	do_dm_b
	moveq.l	#'.',d0
do_dm_b:
	bsr	data_move_
	bsr	ofsst2
	bsr	ofsed2

	move.w	$e8002a,d6
	move.w	#%00000001_1100_0000,$e8002a	*同時アクセス

	move.w	#52*8+4,d1
	move.w	#27*16,d2
	lea	sankaku(pc),a1
	IOCS	_TEXTPUT

 	move.w	d6,$e8002a			*同時アクセス解除
	movem.l	(sp)+,d0-d7/a0-a6
	rts

clr_bottom:
	movem.l	d0-d2,-(sp)
	bsr	init_kbuf
	moveq.l	#0,d1
	moveq.l	#28,d2
	IOCS	_B_LOCATE
	moveq.l	#0,d1
	IOCS	_B_CLR_ST
	movem.l	(sp)+,d0-d2
	rts
val_init:
	movem.l	d0/a0,-(sp)
	moveq.l	#0,d0
	lea	monitor_mode(pc),a0			*on
	move.b	d0,(a0)					*on
	st	data_type-monitor_mode(a0)		*adpcm
	move.b	#4,sampling_rate-monitor_mode(a0)	*15kHz
	move.b	#3,out_assign-monitor_mode(a0)		*middle
	move.b	d0,trigger-monitor_mode(a0)		*off
	move.w	#8/2,recording_time-monitor_mode(a0)	*8seconds
	move.l	#62400/2,rec_data_size-monitor_mode(a0)
	move.l	d0,file_list-monitor_mode(a0)

	move.l	d0,address1-monitor_mode(a0)
	move.l	d0,address2-monitor_mode(a0)
	move.l	d0,start_point1-monitor_mode(a0)
	move.l	d0,start_point2-monitor_mode(a0)
	move.l	d0,end_point1-monitor_mode(a0)
	move.l	d0,end_point2-monitor_mode(a0)
	move.l	d0,size1-monitor_mode(a0)
	move.l	d0,size2-monitor_mode(a0)
	move.l	d0,buffer1-monitor_mode(a0)
	move.l	d0,buffer2-monitor_mode(a0)
	move.b	#':',cdr_str+1-monitor_mode(a0)
	move.b	#'\',cdr_str+2-monitor_mode(a0)
	movem.l	(sp)+,d0/a0
	rts

	*メッセージの表示部
	* < d7=8:reverse / 0=normal
_bank_a:
	move.b	d7,-(sp)
	tst.b	bank_sel
	bne	not_a_
	eori.b	#8,d7
not_a_:
	display	bank_a,%0110,0,0,8
	tst.b	data_a
	beq	erasebnka
	display3	bank_a_fn,%0110,9,0
	bra	dobnka
erasebnka:
	moveq.l	#9,d1
	moveq.l	#0,d2
	IOCS	_B_LOCATE
	moveq.l	#0,d1
	IOCS	_B_ERA_ST
dobnka:
	bsr	ofsttl_a
	bsr	ofsst1
	bsr	ofsed1
	move.b	(sp)+,d7
	rts

_bank_b:
	move.b	d7,-(sp)
	tst.b	bank_sel
	beq	not_b_
	eori.b	#8,d7
not_b_:
	display	bank_b,%0110,0,14,8
	tst.b	data_b
	beq	erasebnkb
	display3	bank_b_fn,%0110,9,14
	bra	dobnkb
erasebnkb:
	moveq.l	#9,d1
	moveq.l	#14,d2
	IOCS	_B_LOCATE
	moveq.l	#0,d1
	IOCS	_B_ERA_ST
dobnkb:
	bsr	ofsttl_b
	bsr	ofsst2
	bsr	ofsed2
	move.b	(sp)+,d7
	rts

_stpt1:
	display	stpt,%0111,0,11,11
	rts
_edpt1:
	display	edpt,%0111,0,12,11
	rts
_stpt2:
	display	stpt,%0111,0,25,11
	rts
_edpt2:
	display	edpt,%0111,0,26,11
	rts
_mon:
	display	mt_on,%0111,0,28,8
	rts
_dt_tp:
	display	dt_tp,%0111,15,28,10
	rts
_smp_tm:
	display	smp_tm,%0111,32,28,14
	rts
_sec:
	display	sec,%0111,50,28,6
	rts
_smp_rt:
	display	smp_rt,%0111,56,28,14
	rts
_khz:
	display	khz,%0111,74,28,6
	rts
_out_as:
	display	out_as,%0111,0,30,7
	rts
_at_md:
	display	at_md,%0111,15,30,8
	rts
_smp_sz:
	display	smp_sz,%0111,28,30,14
	rts
_bytes:
	display	bytes,%0111,50,30,10
_quit:
	display	quit,%0101,89,30,4
	rts
_play:
	display	play,%0110,81,30,4
	rts
_rec:
	display	rec,%0110,81,28,6
	rts
_files:
	display	files,%0101,89,28,4
	rts
_level:
	display	level,%0110,56,30,6
	rts
_pitch:
	display	pitch,%0110,64,30,5
	rts
_mix:
	display	mix,%0110,71,30,8
	rts

	*値の表示
	* < d0=data,d7=attribute
mt_on_off:
	move.b	d0,monitor_mode
	beq	case_m_of
	display	on_msg,%0011,8,28,3
	rts
case_m_of:
	display	of_msg,%0011,8,28,3
	rts
dt_type:
	move.b	d0,data_type
	beq	case_pcm
	display	tp_adp,%0011,25,28,5
	rts
case_pcm:
	display	tp_pcm,%0011,25,28,5
	rts
smp_rate:
	lea	_39(pc),a1
	move.b	d0,sampling_rate-_39(a1)
	ext.w	d0
	lsl.w	#2,d0
	adda.w	d0,a1
	moveq.l	#%0011,d1
	or.b	d7,d1
	moveq.l	#70,d2
	moveq.l	#28,d3
	moveq.l	#3,d4
	IOCS	_B_PUTMES
	rts
pan_pot:
	lea	mute(pc),a1
	move.b	d0,out_assign-mute(a1)
	ext.w	d0
	lsl.w	#3,d0
	adda.w	d0,a1
	moveq.l	#%0011,d1
	or.b	d7,d1
	moveq.l	#7,d2
	moveq.l	#30,d3
	moveq.l	#5,d4
	IOCS	_B_PUTMES
	rts

trg_on_off:
	move.b	d0,trigger
	beq	case_t_off
	display	on_msg,%0011,23,30,3
	rts
case_t_off:
	display	of_msg,%0011,23,30,3
	rts
rec_time:
	move.w	d0,recording_time
	bsr	num_to_str
	display	suji+6,%0011,46,28,4
	rts
data_size:
	move.l	d0,rec_data_size
	bsr	num_to_str
	display	suji+2,%0011,42,30,8
	rts
_num_to_str:	*レジスタの値を文字数列にする
	movem.l	d1-d4/a0-a1,-(sp)
	moveq.l	#'0',d4
	bra	@f
num_to_str:	*レジスタの値を文字数列にする
	* < d0.l=value
	* > suji
	* - all
	movem.l	d1-d4/a0-a1,-(sp)
	moveq.l	#' ',d4
@@:
	lea	suji(pc),a0
	clr.b	hajimete-suji(a0)
	moveq.l	#9,d1
	lea	exp_tbl(pc),a1
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
	tst.b	d2
	bne	nml_ktset
	tst.b	hajimete
	bne	nml_ktset
	move.b	d4,(a0)+	*dummy
	bra	nml_lp_ope

nml_ktset:
	st	hajimete
	add.b	#'0',d2
	move.b	d2,(a0)+
nml_lp_ope:
	dbra	d1,ex_loop0
	tst.b	hajimete
	bne	@f
	move.b	#'0',-1(a0)
@@:
	clr.b	(a0)		*end flg
	movem.l	(sp)+,d1-d4/a0-a1
	rts

test_mode:			*PCM/ADPCMデータのテスト再生
	bsr	clr_bottom
	move.b	data_type(pc),dt_mem
	lea	pcm_test(pc),a0	*use later
	display2_	1(a0),%1111,0,28,9
	moveq.l	#0,d1
	bsr	input_fn
test_play_:
	move.l	a1,d0
	beq	exit_ld_md
	bsr	ext_chk		*拡張子のチェック
	bsr	saisei		*ADPCM再生
	tst.b	use_fm
	beq	exit_ld_md
	move.l	#nameptr,work3
	movem.l	reg_buff(pc),d0-d7/a0-a6
	pea	test_play_(pc)	*push return addr.
	bra	hot_start

saisei:
	* < d1.l=size
	* < a1.l=filename pointer
	* - all
	movem.l	d0-d7/a0-a6,-(sp)
	moveq.l	#0,d1
	IOCS	_ADPCMMOD	*adpcm stop

	bsr	read_data	*d1=size,d2=address
	movea.l	d2,a1		*data address
	move.l	d1,d2		*size
	bsr	print_em
	bmi	naranai_
	tst.b	data_type
	bne	_saisei
				*PCM dataの再生のケース
	bsr	get_pt1
	bsr	dsp_cv_mes
	move.l	d2,d1
	lsr.l	#2,d1
	move.l	d1,-(sp)	*adpcm data分確保
	DOS	_MALLOC
	addq.w	#4,sp
	move.l	d0,mm_error
	bsr	print_em
	bmi	naranai		*case:error
	move.l	d0,a0		*for adpcm_data
	move.l	d1,d0		*size
	movem.l	d1/a0-a1,-(sp)
	bsr	pcm_to_adpcm
	movem.l	(sp)+,d1/a0-a1
	pea	(a1)
	DOS	_MFREE		*mfree pcm data
	addq.w	#4,sp
	move.l	a0,a1		*a1=adpcm data adr
	move.l	d1,d2		*d2=size of adpcm data
	bsr	put_pt1
_saisei:
	move.b	sampling_rate(pc),d1
	lsl.w	#8,d1
	move.b	out_assign(pc),d1
	IOCS	_ADPCMOUT
naranai:
	pea	(a1)
	DOS	_MFREE
	addq.w	#4,sp
naranai_:
	movem.l	(sp)+,d0-d7/a0-a6
	rts

ofsttl_a:
	movem.l	d0-d4/a0-a1,-(sp)
	display2	ofsbar,%0011,34,0,60
	lea	$e00000+35,a1
	lea	stp0(pc),a0
	bsr	cgwr
	lea	$e00000+35+$80*8,a1
	lea	stp1(pc),a0
	bsr	cgwr
	lea	$e00000+54,a1
	lea	edp0(pc),a0
	bsr	cgwr
	lea	$e00000+54+$80*8,a1
	lea	edp1(pc),a0
	bsr	cgwr
	lea	$e00000+73,a1
	lea	efsz0(pc),a0
	bsr	cgwr
	lea	$e00000+73+$80*8,a1
	lea	efsz1(pc),a0
	bsr	cgwr
	movem.l	(sp)+,d0-d4/a0-a1
	rts

ofsttl_b:
	movem.l	d0-d4/a0-a1,-(sp)
	display2	ofsbar,%0011,34,14,60
	lea	$e00000+35+$80*14*16,a1
	lea	stp0(pc),a0
	bsr	cgwr
	lea	$e00000+35+$80*8+$80*14*16,a1
	lea	stp1(pc),a0
	bsr	cgwr
	lea	$e00000+54+$80*14*16,a1
	lea	edp0(pc),a0
	bsr	cgwr
	lea	$e00000+54+$80*8+$80*14*16,a1
	lea	edp1(pc),a0
	bsr	cgwr
	lea	$e00000+73+$80*14*16,a1
	lea	efsz0(pc),a0
	bsr	cgwr
	lea	$e00000+73+$80*8+$80*14*16,a1
	lea	efsz1(pc),a0
	bsr	cgwr
	movem.l	(sp)+,d0-d4/a0-a1
	rts

cgwr:
	* < a1.l=write address
	movem.l	d0-d1/a0/a2,-(sp)
	move.w	sr,-(sp)
	ori.w	#$0700,sr
	move.w	$e8002a,-(sp)
	move.w	#%00000001_00110000,$e8002a	*同時アクセス
cgwrlp:
	moveq.l	#0,d0
	move.b	(a0)+,d0
	beq	exit_cgwr
	pea	(a1)
	lsl.w	#3,d0
	lea	$f3a000,a2	*CG ADR
	add.w	d0,a2
	moveq.l	#8-1,d1
@@:
	move.b	(a2)+,(a1)
	lea	$80(a1),a1
	dbra	d1,@b
	move.l	(sp)+,a1
	addq.w	#1,a1
	bra	cgwrlp
exit_cgwr:
	move.w	(sp)+,$e8002a
	move.w	(sp)+,sr
	movem.l	(sp)+,d0-d1/a0/a2
	rts

_data:		equ	36
_disp_max:	equ	60
fsy:		equ	2		*メニューの開始y座標
fey:		equ	31		*メニューの終了y座標
num_f:		equ	30		*1列のファイル数
file_sel:		*ファイルのセレクト
	* < work3 = 検索ファイル名(default='*.*')
	* X d0-d7,a0,a1,a2,a6

	IOCS	_OS_CUROF
	bsr	g_giji_clr
make_file_list:
	bsr	zen_clr2	*画面消去2

	moveq.l	#0,d6		*number of exist files
	moveq.l	#0,d2		*x
	moveq.l	#fsy,d3		*y
	move.l	#256,fn_buf
	move.l	#_data*256,fn_buf_size

	move.l	file_list(pc),-(sp)	*古いのは消去
	beq	@f
	DOS	_MFREE
@@:
	addq.w	#4,sp

	move.l	fn_buf_size(pc),-(sp)	*256 file 分のバッファ
	DOS	_MALLOC
	addq.w	#4,sp
	tst.l	d0
	bmi	mem_error
	move.l	d0,file_list
	movea.l	d0,a6
					*カレントの変更
	movem.l	d0/a0-a1,-(sp)
	move.l	work3(pc),a0
	move.l	a0,a1
	tst.b	(a0)
	beq	end_chgcur
	cmpi.b	#':',1(a0)
	bne	@f
	move.b	(a0),d0
	andi.w	#$df,d0
	subi.b	#'A',d0
	move.w	d0,-(sp)
	DOS	_CHGDRV
	addq.w	#2,sp
@@:
	tst.b	(a0)+
	bne	@b
@@:
	move.b	-(a0),d0
	cmpi.b	#'\',d0
	beq	@f
	cmp.l	a1,a0
	bne	@b
	bra	end_chgcur
@@:
	move.b	1(a0),-(sp)
	clr.b	1(a0)
	pea	(a1)
	DOS	_CHDIR
	addq.w	#4,sp
	move.b	(sp)+,1(a0)
end_chgcur:
	movem.l	(sp)+,d0/a0-a1

	move.w	#%110000,-(sp)
	move.l	work3(pc),-(sp)
	pea	filbuf(pc)
	DOS	_FILES
	tst.l	d0
	bmi	exit_nf
	move.b	filbuf+30(pc),d0
	cmpi.b	#'.',d0
	bne	nm_trns
nfiles:
	DOS	_NFILES
	tst.l	d0
	bmi	exit_nf
nm_trns:
	lea	filbuf+30(pc),a1
	move.b	#$20,(a6)+	*0
	moveq.l	#18-1,d4	*18-1 filename(18)
nm_trns_lp:			*save file_name
	move.b	(a1)+,d0
	cmpi.b	#'.',d0
	beq	dmy_spc
	move.b	d0,(a6)+
	dbra	d4,nm_trns_lp
_get_peri:
	move.b	(a1)+,d0
	beq	kk_trns		*case:end
	cmpi.b	#'.',d0
	bne	_get_peri
	bra	kk_trns
dmy_spc:
	move.b	#$20,(a6)+
	dbra	d4,dmy_spc
kk_trns:
	move.b	d0,(a6)+	* '.'
	moveq.l	#3-1,d4		*拡張子(3)
kk_trns_lp:			*save 拡張子
	move.b	(a1)+,d0
	beq	dmy_spc2
	move.b	d0,(a6)+
	dbra	d4,kk_trns_lp
	move.b	(a1)+,d0
	bra	go_nf
dmy_spc2:
	move.b	#$20,(A6)+
	dbra	d4,dmy_spc2
go_nf:
	move.b	#$20,(a6)+		*dmy spc 	23
	move.b	d0,(a6)+		*0		24
	move.b	filbuf+21(pc),(a6)+	*属性		25
	move.l	filbuf+26(pc),(a6)+	*save size	26,27,28,29
	move.b	d2,(a6)+		*x		30
	move.b	d3,(a6)+		*y		31
	move.l	filbuf+22(pc),(a6)+	*date		32,33,34,35

	addq.l	#1,d6			*num of files

	cmp.l	fn_buf(pc),d6
	bls	@f
	add.l	#256,fn_buf
	add.l	#_data*256,fn_buf_size
	move.l	fn_buf_size(pc),-(sp)
	move.l	file_list(pc),-(sp)
	DOS	_SETBLOCK
	addq.w	#8,sp
	tst.l	d0
	bmi	mem_error
@@:
	addq.b	#1,d3		*y=y+1
	cmpi.b	#fey+1,d3
	bne	go_nf1
	moveq.l	#fsy,d3
	addi.b	#48,d2		*x=x+29
	cmpi.b	#96,d2
	bcs	go_nf1
	moveq.l	#0,d2		*x=0
go_nf1:
	bra	nfiles		*go back to loop start
exit_nf:
	lea	10(sp),sp	*stack came back
	subq.l	#1,d6		*nf=nf-1
	scs	no_files
	move.l	d6,num_of_files

	moveq.l	#0,d7
	bsr	disp_cd			*カレントディレクトリ等の表示

	tst.b	no_files		*ファイル個数=0
	bne	_klp
	*select部
	movea.l	file_list(pc),a0	*file names base address
	move.l	sel_pnt(pc),d6		*select pointer
	moveq.l	#0,d0
	move.l	num_of_files(pc),d0
	cmp.l	d0,d6
	bls	@f
	move.l	d0,d6
@@:
	move.l	d6,d0
	divu	#_disp_max,d0
	bsr	prt_all_files		*ファイル初期表示
	move.l	d6,d5			*last select pointer
	bsr	disp_cur
_klp:
	move.l	d6,sel_pnt
	IOCS	_B_KEYINP

	cmpi.b	#'?',d0
	beq	input_fn2

	cmpi.b	#' ',d0
	beq	kiku

	move.b	d0,d1		*alphabet ?
	bsr	mk_capital1
	subi.b	#$41,d1
	bmi	number?
	cmpi.b	#26,d1
	bcs	chg_drv
number?:
	move.b	d0,d1		*number key?
	subi.b	#$30,d1
	bmi	not_number
	cmpi.b	#4,d1
	bls	chg_frq
not_number:
	cmpi.b	#$0d,d0
	beq	ret_ope		*CR!

	cmpi.b	#$1b,d0		*esc
	beq	quit_this

	cmpi.b	#3,d0		*break
	beq	quit_this

	tst.b	no_files	*ファイル個数=0
	bne	_klp
				*cursor 移動チェック
	move.w	d0,d1
	lsr.w	#8,d1
*rllup??:			*roll up
	cmpi.w	#7*8+1,d1
	bne	rlldwn??
	move.l	d6,d0
	subi.l	#_disp_max,d0
	bmi	@f
	moveq.l	#-_disp_max,d0
	bra	move_cur
@@:
	move.l	d6,d0
	neg.l	d0
	bra	move_cur
rlldwn??:			*roll down
	cmpi.w	#7*8+0,d1
	bne	up??
	move.l	d6,d0
	addi.l	#_disp_max,d0
	cmp.l	num_of_files(pc),d0
	bhi	@f
	moveq.l	#_disp_max,d0
	bra	move_cur
@@:
	moveq.l	#0,d0
	move.l	num_of_files(pc),d0
	sub.l	d6,d0
	bra	move_cur
up??:
	cmpi.w	#7*8+4,d1
	bne	down??
_up?:
	tst.l	d6
	beq	_nxt_k
	moveq.l	#-1,d0
	bra	move_cur
down??:
	cmpi.b	#7*8+6,d1
	bne	left??
_dwn?:
	cmp.l	num_of_files(pc),d6
	beq	_nxt_k
	moveq.l	#1,d0
	bra	move_cur
left??:
	cmpi.b	#7*8+3,d1
	bne	right??
	move.l	d6,d0
	subi.l	#num_f,d0
	bmi	@f
	moveq.l	#-num_f,d0
	bra	move_cur
@@:
	move.l	d6,d0
	neg.l	d0
	bra	move_cur
right??:
	cmpi.b	#7*8+5,d1
	bne	_nxt_k
	move.l	d6,d0
	addi.l	#num_f,d0
	cmp.l	num_of_files(pc),d0
	bhi	@f
	moveq.l	#num_f,d0
	bra	move_cur
@@:
	moveq.l	#0,d0
	move.l	num_of_files(pc),d0
	sub.l	d6,d0
	bra	move_cur
_nxt_k:
	cmpi.b	#2*8+0,d1	*tab
	beq	tab_ope

	IOCS	_B_SFTSNS
	btst.l	#0,d0
	bne	mute_pcm

	bra	_klp

move_cur:
	add.l	d0,d6
hot_start:
do_cur:
	bsr	del_cur
	bsr	disp_cur
	move.l	d6,d5
	bra	_klp
del_cur:
	move.w	d5,d0
	mulu	#_data,d0
	lea	(a0,d0.l),a2	*last
	moveq.l	#0,d7
	bra	prt_file
disp_cur:
	move.l	d6,d0
	divu	#_disp_max,d0
	move.l	d5,d1
	divu	#_disp_max,d1
	cmp.w	d0,d1
	beq	@f
	bsr	prt_all_files
@@:
	move.w	d6,d0
	mulu	#_data,d0
	lea	(a0,d0.l),a2	*new
	moveq.l	#%1000,d7
	bra	prt_file

quit_this:	*ファイルセレクト終了
	suba.l	a1,a1		*a1=0
	rts
mute_pcm:
	moveq.l	#0,d1
	IOCS	_ADPCMMOD	*adpcm stop
	bra	_klp
chg_frq:
	move.b	d1,sampling_rate
	bra	_klp
chg_drv:
	andi.w	#$00ff,d1
	addq.w	#1,d1
	move.w	d1,-(sp)
	DOS	_DRVCTRL
	addq.w	#2,sp
	btst.l	#1,d0
	beq	_klp		*not ready

	subq.w	#1,d1
	move.w	d1,-(sp)
	DOS	_CHGDRV
	addq.w	#2,sp

	clr.l	oya_kai
	clr.l	sel_pnt

	move.l	#nameptr,work3

	bra	make_file_list

dmy_lp:
	subq.l	#1,d0
	bne	dmy_lp
	rts

ret_ope:	*return keyが押された
	* > a1=file name pointer
	* > d1=size
	bsr	del_cur		*erase cursor
	move.w	d6,d0
	mulu	#_data,d0
	lea	(a0,d0.l),a2
	btst.b	#4,25(a2)
	bne	dir_chg
	lea	1(a2),a1
	move.l	26(a2),d1
	movem.l	d0-d7/a0-a6,reg_buff
	rts
dir_chg:
	pea	1(a2)
	DOS	_CHDIR
	addq.w	#4,sp
	cmpi.b	#'.',19(a2)
	bne	@f
	cmpi.b	#'.',20(a2)
	bne	@f
	move.l	oya_kai(pc),sel_pnt
	clr.l	oya_kai
	bra	make_file_list
@@:
	move.l	d6,oya_kai
	clr.l	sel_pnt
	bra	make_file_list
tab_ope:	*tab keyが押された
	bsr	del_cur		*erase cursor
	move.w	d6,d0
	mulu	#_data,d0
	lea	(a0,d0.l),a2
*	btst.b	#4,25(a2)
*	bne	dir_chg
	cmpi.b	#'.',19(a2)	*!!!
	bne	@f		*!!!
	cmpi.b	#'.',20(a2)	*!!!
	beq	dir_chg		*!!!
@@:
	moveq.l	#0,d6
	bra	do_cur

kiku:		*SPACE keyが押された
	* > a1=file name pointer
	* > d1=size
	bsr	del_cur		*erase cursor
	move.w	d6,d0
	mulu	#_data,d0
	lea	(a0,d0.l),a2
	btst.b	#4,25(a2)
	bne	do_cur		*ディレクトリ名は駄目
	lea	1(a2),a1
	move.l	26(a2),d1
	bsr	ext_chk
	bsr	saisei
	bra	do_cur

input_fn2:
	* > use_fm=1 file menuを使った
	* > a1=file name addr.
	* > a1=0 then cansel
	moveq.l	#0,d1
	moveq.l	#1,d2
	IOCS	_B_LOCATE
	IOCS	_B_ERA_ST
	display2	input_fn2_mes,%0111,0,1,16
	moveq.l	#16,d1
	moveq.l	#1,d2
	IOCS	_B_LOCATE
	IOCS	_OS_CURON

	lea	input_buffer(pc),a1
	move.b	#91,(a1)	*文字数
	pea	(a1)
	DOS	_GETS		*一行入力
	addq.w	#4,sp

	addq.w	#2,a1		*chr data buffer
	IOCS	_OS_CUROF

	move.l	#nameptr,work3
	cmpi.b	#$20,(a1)
	bls	make_file_list
	move.l	a1,work3
	bsr	check_wild
	bmi	make_file_list
	movem.l	d0-d7/a0-a6,reg_buff
	rts

prt_all_files:
	* < d0.w=page
	movem.l	d0-d7/a0-a2,-(sp)
	bsr	zen_clr3		*画面消去2
	mulu	#_disp_max,d0
	move.l	d0,d1			*page*60
	mulu	#_data,d0
	movea.l	file_list(pc),a2	*file names
	add.l	d0,a2
	move.l	num_of_files(pc),d6
	tst.b	no_files		*file個数=0?
	bne	@f
	sub.l	d1,d6
	cmpi.l	#_disp_max-1,d6
	bls	print_fn_lp
	moveq.l	#_disp_max-1,d6
print_fn_lp:
	bsr	prt_file
	lea	_data(a2),a2
	dbra	d6,print_fn_lp
@@:
	movem.l	(sp)+,d0-d7/a0-a2
	rts

prt_file:
	* < a2=file pointer
	* < d7=attribute
	* < d0=offset
	* X d0,d1,d2,d3,d4 a1
	btst.b	#4,25(a2)	*atr
	beq	set_white
	moveq.l	#%0010,d1
	bra	or_rev
set_white:
	moveq.l	#%0011,d1	*atr
or_rev:
	or.b	d7,d1
	lea	(a2),a1
	moveq.l	#23,d4		*length
	move.b	30(a2),d2
	move.b	31(a2),d3	*get x&y
	IOCS	_B_PUTMES

	btst.b	#4,25(a2)	*atr
	bne	case_dir	*ディレクトリ

	move.l	26(a2),d0	*size
	bsr	num_to_str
	move.w	#$2000,suji+10
	lea	suji+2(pc),a1
	moveq.l	#9-1,d4
	IOCS	_B_PUTMES
	bra	_year
case_dir:
	subq.b	#1,d2
	lea	_dir(pc),a1
	moveq.l	#10-1,d4
	moveq.l	#%0010,d1
	or.b	d7,d1
	IOCS	_B_PUTMES
_year:
	moveq.l	#0,d0
	move.b	34(a2),d0
	lsr.b	d0
	add.b	#80,d0
	bsr	num_to_str
	lea	suji+8(pc),a1
	move.w	#$2e_00,2(a1)
	moveq.l	#3-1,d4
	IOCS	_B_PUTMES	*年

	moveq.l	#0,d0
	move.w	34(a2),d0
	lsr.w	#5,d0
	andi.w	#15,d0
	bsr	_num_to_str
	lea	suji+8(pc),a1
	move.w	#$2e_00,2(a1)
	moveq.l	#3-1,d4
	IOCS	_B_PUTMES	*月

	moveq.l	#0,d0
	move.w	34(a2),d0
	andi.w	#31,d0
	bsr	_num_to_str
	lea	suji+8(pc),a1
	moveq.l	#3-1,d4
	IOCS	_B_PUTMES	*日

	moveq.l	#0,d0
	move.b	32(a2),d0
	lsr.b	#3,d0
	andi.w	#31,d0
	bsr	_num_to_str
	lea	suji+8(pc),a1
	move.w	#$3a_00,2(a1)
	moveq.l	#3-1,d4
	IOCS	_B_PUTMES	*時

	moveq.l	#0,d0
	move.w	32(a2),d0
	lsr.w	#5,d0
	andi.w	#63,d0
	bsr	_num_to_str
	lea	suji+8(pc),a1
*	move.w	#$27_00,2(a1)
	moveq.l	#3-1,d4
	IOCS	_B_PUTMES	*分

*	moveq.l	#0,d0
*	move.b	33(a2),d0
*	andi.w	#31,d0
*	add.b	d0,d0
*	bsr	num_to_str
*	lea	suji+8(pc),a1
*	moveq.l	#3-1,d4
*	IOCS	_B_PUTMES	*秒
ext_prt_fl:
	rts

disp_cd:
	DOS	_CURDRV
	move.w	d0,d1
	add.b	#$41,d0
	move.b	d0,cdr_str

	lea	pathbf(pc),a1
	moveq.l	#91,d0
spc_lpp:
	move.b	#$20,(a1)+
	dbra	d0,spc_lpp

	addq.w	#1,d1
	pea	pathbf(pc)
	move.w	d1,-(sp)
	DOS	_CURDIR
	addq.w	#6,sp

	display	pathbf-3,%0110,0,1,67
	rts
loop:
	bra	loop

clr_em_area:
	movem.l	d0-d2,-(sp)
	moveq.l	#0,d1
	moveq.l	#15,d2
	IOCS	_B_LOCATE
	moveq.l	#2,d1
	IOCS	_B_ERA_ST	*メッセージエリアの消去
	movem.l	(sp)+,d0-d2
	rts
print_em:
	* > d1=minus then error
	movem.l	d0-d4/a0-a1,-(sp)
	lea	work(pc),a1
	tst.l	mm_error-work(a1)
	bmi	mem_error
	tst.l	fo_error-work(a1)
	bmi	file_error
	tst.l	rd_error-work(a1)
	bmi	_rd_error
	tst.l	wr_error-work(a1)
	bmi	_wr_error
	tst.l	disk_full-work(a1)
	bmi	_disk_full
	moveq.l	#0,d0
	movem.l	(sp)+,d0-d4/a0-a1
	rts
err_exit:
	moveq.l	#0,d0
	lea	work(pc),a1
	move.l	d0,mm_error-work(a1)
	move.l	d0,fo_error-work(a1)
	move.l	d0,rd_error-work(a1)
	move.l	d0,wr_error-work(a1)
	move.l	d0,disk_full-work(a1)
	IOCS	_OS_CUROF
	bsr	get_spc
	bsr	put_pt
	moveq.l	#-1,d0
	movem.l	(sp)+,d0-d4/a0-a1
	rts
get_spc:
	DOS	_INKEY
	cmpi.b	#$20,d0
	beq	bye_gs
	cmpi.b	#$0d,d0
	beq	bye_gs
	bra	get_spc
bye_gs:
	rts

mk_capital0:			*小文字→大文字(英字以外の場合はそのままthrough out)
	* < d0.b=letter chr
	cmpi.b	#'a',d0
	bcs	exit_mkcptl
	cmpi.b	#'z',d0
	bhi	exit_mkcptl
	andi.w	#$df,d0		*わざと.w
exit_mkcptl:
	rts

mk_capital1:			*小文字→大文字(英字以外の場合はそのままthrough out)
	* < d1.b=letter chr
	cmpi.b	#'a',d1
	bcs	exit_mkcptl
	cmpi.b	#'z',d1
	bhi	exit_mkcptl
	andi.w	#$df,d1		*わざと.w
	rts

init_kbuf:
	move.l	d0,-(sp)
	clr.w	-(sp)
	DOS	_KFLUSH
	addq.w	#2,sp
	move.l	(sp)+,d0
	rts

erase_line	macro	y
	moveq.l	#0,d1
	moveq.l	#y,d2
	IOCS	_B_LOCATE
	moveq.l	#2,d1
	IOCS	_B_ERA_ST
	endm

mem_error:
	bsr	get_pt
	erase_line	15
	display2	mem_er_mes,%1111,41,15,13
	clr.l	mm_error
	bra	err_exit
file_error:
	bsr	get_pt
	erase_line	15
	display2	fop_er_mes,%1111,40,15,15
	bra	err_exit
_rd_error:
	bsr	get_pt
	erase_line	15
	display2	rd_er_mes,%1111,43,15,10
	bra	err_exit
_wr_error:
	bsr	get_pt
	erase_line	15
	display2	wr_er_mes,%1111,43,15,11
	bra	err_exit
_disk_full:
	bsr	get_pt
	erase_line	15
	display2	dsk_f_mes,%1111,44,15,9
	bra	err_exit
get_pt:				*エラーメッセージの背景を待避
	movem.l	d0-d1/a0-a1,-(sp)
	pea	96*16*2.w
	DOS	_MALLOC
	addq.w	#4,sp
	move.l	d0,a0
	move.l	d0,get_pt_buf
	lea	$e00000+$80*16*15,a1
	moveq.l	#15,d1
get_pt_lp:
	moveq.l	#24-1,d0
get_pt_lp_:
	move.l	(a1)+,(a0)+
	dbra	d0,get_pt_lp_
	lea	32(a1),a1
	dbra	d1,get_pt_lp
	lea	$e20000+$80*16*15,a1
	moveq.l	#15,d1
get_pt_lp2:
	moveq.l	#24-1,d0
get_pt_lp2_:
	move.l	(a1)+,(a0)+
	dbra	d0,get_pt_lp2_
	lea	32(a1),a1
	dbra	d1,get_pt_lp2
	movem.l	(sp)+,d0-d1/a0-a1
	rts
put_pt:				*エラーメッセージの背景を復活
	movem.l	d0-d1/a0-a1,-(sp)
	movea.l	get_pt_buf(pc),a0
	lea	$e00000+$80*16*15,a1
	moveq.l	#15,d1
put_pt_lp:
	moveq.l	#24-1,d0
put_pt_lp_:
	move.l	(a0)+,(a1)+
	dbra	d0,put_pt_lp_
	lea	32(a1),a1
	dbra	d1,put_pt_lp
	lea	$e20000+$80*16*15,a1
	moveq.l	#15,d1
put_pt_lp2:
	moveq.l	#24-1,d0
put_pt_lp2_:
	move.l	(a0)+,(a1)+
	dbra	d0,put_pt_lp2_
	lea	32(a1),a1
	dbra	d1,put_pt_lp2

	move.l	get_pt_buf(pc),-(sp)
	DOS	_MFREE
	addq.w	#4,sp
	movem.l	(sp)+,d0-d1/a0-a1
	rts
get_pt1:
	movem.l	d0-d1/a0-a1,-(sp)
	pea	96*16*2.w
	DOS	_MALLOC
	addq.w	#4,sp
	move.l	d0,a0
	move.l	d0,get_pt_buf1
	lea	$e00000+$80*16*15,a1
	moveq.l	#15,d1
get_pt_lp1:
	moveq.l	#24-1,d0
get_pt_lp_1:
	move.l	(a1)+,(a0)+
	dbra	d0,get_pt_lp_1
	lea	32(a1),a1
	dbra	d1,get_pt_lp1
	lea	$e20000+$80*16*15,a1
	moveq.l	#15,d1
get_pt_lp21:
	moveq.l	#24-1,d0
get_pt_lp2_1:
	move.l	(a1)+,(a0)+
	dbra	d0,get_pt_lp2_1
	lea	32(a1),a1
	dbra	d1,get_pt_lp21
	movem.l	(sp)+,d0-d1/a0-a1
	rts
put_pt1:
	movem.l	d0-d1/a0-a1,-(sp)
	movea.l	get_pt_buf1(pc),a0
	lea	$e00000+$80*16*15,a1
	moveq.l	#15,d1
put_pt_lp1:
	moveq.l	#24-1,d0
put_pt_lp_1:
	move.l	(a0)+,(a1)+
	dbra	d0,put_pt_lp_1
	lea	32(a1),a1
	dbra	d1,put_pt_lp1
	lea	$e20000+$80*16*15,a1
	moveq.l	#15,d1
put_pt_lp21:
	moveq.l	#24-1,d0
put_pt_lp2_1:
	move.l	(a0)+,(a1)+
	dbra	d0,put_pt_lp2_1
	lea	32(a1),a1
	dbra	d1,put_pt_lp21

	move.l	get_pt_buf1(pc),-(sp)
	DOS	_MFREE
	addq.w	#4,sp
	movem.l	(sp)+,d0-d1/a0-a1
	rts

get_name:			*get file name or switches
	*< a2=command line
	*> a2=point next
	movem.l	d0/a3,-(sp)
	lea	input_buffer(pc),a3
loop1:
	move.b	(a2)+,d0
	beq	ext_nm
	cmpi.b	#' ',d0
	bls	loop1		*skip space

	subq.w	#1,a2
loop2:
	move.b	(a2)+,d0
	cmpi.b	#' ',d0
	bls	ext_nm
	move.b	d0,(a3)+
	bra	loop2
ext_nm:
	subq.w	#1,a2
	clr.b   (a3)
	movem.l	(sp)+,d0/a3
	rts
sv_fn:				*ファイルネームを定位置へ
	movem.l	d0/a0-a1,-(sp)
	lea	input_buffer(pc),a0
	tst.b	fn1
	bne	chk_fn2?
	lea	fn1(pc),a1
	bsr	check_wild2
	bpl	do_sv_fn
	lea	wfn1(pc),a1
	move.b	#1,wild_mode
	st	fn1
	bra	do_sv_fn
chk_fn2?:
	tst.b	fn2
	bne	chk_fn3?
	lea	fn2(pc),a1
	bsr	check_wild2
	bpl	do_sv_fn
	ori.b	#2,wild_mode
	lea	wfn2(pc),a1
	st	fn2
	bra	do_sv_fn
chk_fn3?:
	tst.b	fn3
	bne	bye_sv_fn
	lea	fn3(pc),a1
	bsr	check_wild2
	bpl	do_sv_fn
	ori.b	#4,wild_mode
	lea	wfn3(pc),a1
	st	fn3
do_sv_fn:
	move.b	(a0)+,(a1)+
	bne	do_sv_fn
	moveq.l	#0,d0
bye_sv_fn:
	movem.l	(sp)+,d0/a0-a1
	rts

check_wild2:			*ワイルドカードかどうか
	* > minus = wild / zero = not wild
	* - ALL
	movem.l	d0/a0,-(sp)
cw_lp2:
	move.b	(a0)+,d0
	beq	bye_cw_no2	*file name end
	cmpi.b	#'*',d0
	beq	bye_cw_yes2
	cmpi.b	#'?',d0
	bne	cw_lp2
bye_cw_yes2:
	moveq.l	#-1,d0
	movem.l	(sp)+,d0/a0
	rts
bye_cw_no2:
	moveq.l	#0,d0
	movem.l	(sp)+,d0/a0
	rts

fn_chk_p16:
	lea	fn1(pc),a0
	lea	fn1_p16(pc),a1
	bsr	do_fcp16
	lea	fn2(pc),a0
	lea	fn2_p16(pc),a1
	bsr	do_fcp16
	lea	fn3(pc),a0
	lea	fn3_p16(pc),a1
do_fcp16:
	move.b	(a0)+,d0
	beq	err_exit_fcp16
	cmpi.b	#'.',d0
	bne	do_fcp16
	move.b	(a0)+,d0
	bsr	mk_capital0
	cmpi.b	#'P',d0
	bne	err_exit_fcp16
	move.b	(a0)+,d0
	cmpi.b	#'1',d0
	bne	err_exit_fcp16
	move.b	(a0)+,d0
	cmpi.b	#'6',d0
	bne	err_exit_fcp16
	st.b	(a1)
	bra	@f
err_exit_fcp16:
	clr.b	(a1)
@@:
	rts

go_command:			*フィルター機能の分岐
	bsr	val_init	*数値の初期化
	bsr	fn_chk_p16	*fnが.p16かどうかチェック
	move.w	#RTS,print_em	*rtsをエラーメッセージ表示ルーチンに書きこんじゃう
	lea	pitch_cmd(pc),a5
	lea	volume_cmd(pc),a6

	tst.l	truncate_cmd-pitch_cmd(a5)	*切り出し(hidden)
	bne	_truncate
	tst.b	_4_cmd-pitch_cmd(a5)		*4段階セーブ
	bne	_4_com
	tst.b	conv_atop_cmd-pitch_cmd(a5)	*adpcm to pcm
	bne	conv_atop
	tst.b	conv_ptoa_cmd-pitch_cmd(a5)	*pcm to adpcm
	bne	conv_ptoa

	tst.b	(a5)
	bne	pv_com
	tst.w	(a6)
	bne	pv_com
	tst.b	1(a5)
	bne	pv_com
	tst.w	2(a6)
	bne	pv_com		*２セットチェックする
	tst.b	mix_cmd-pitch_cmd(a5)
	bne	pv_com

	bra	print_hlp

print_em2:
	* > d1=minus then error
	move.l	d0,-(sp)
	pea	back_print_em2(pc)
	tst.l	mm_error
	bmi	outm_error
	tst.l	fo_error
	bmi	fop_er
	tst.l	rd_error
	bmi	read_error
	tst.l	wr_error
	bmi	write_error
	tst.l	disk_full
	bmi	disk_full_error
	addq.w	#4,sp
	move.l	(sp)+,d0
	move	#0,ccr
	rts
back_print_em2:
	moveq.l	#-1,d0
	move.l	(sp)+,d0
	ori.b	#%01000,ccr
	rts

conv_atop:			*convert adpcm to pcm	!!!
	bsr	disp_proc
	move.b	fn1(pc),d0
	and.b	fn2(pc),d0
	beq	fn_error	*ファイルネームが揃ってない…

	lea	fn1(pc),a1
	bsr	read_data
	bsr	print_em2
	bmi	bye_bye
	move.l	d1,size1
	move.l	d2,address1

	lsl.l	#3,d1		*adpcm sizeの8倍のエリアを確保(for pcm data)
	move.l	d1,-(sp)
	DOS	_MALLOC
	addq.w	#4,sp
	move.l	d0,mm_error
	bsr	print_em2
	bmi	bye_bye
	move.l	d0,buffer1
	move.l	d0,a1		*pcm buffer
	move.l	d2,a0		*adpcm buffer
	move.l	size1(pc),d0	*adpcm size
	moveq.l	#0,d1
	move.b	(a5),d1
	add.b	#12,d1
	moveq.l	#0,d6
	move.w	(a6),d6
	bne	catop1
	moveq.l	#100,d6		*0は100にする
catop1:
	tst.b	_8bit_pcm
	beq	@f
	bsr	adpcm_to_8pcm
	bra	catop2
@@:
	bsr	adpcm_to_pcm	*pcm変換
catop2:
	move.l	a1,d1		*save size
	lea	fn2(pc),a1		*destination file name
	movea.l	buffer1(pc),a2
	bsr	write_data
	bsr	print_em2
	bmi	bye_bye
	bra	mission_complete

conv_ptoa:			*convert pcm to adpcm
	bsr	disp_proc
	move.b	fn1(pc),d0
	and.b	fn2(pc),d0
	beq	fn_error	*ファイルネームがない
	lea	fn1(pc),a1
	bsr	read_data
	bsr	print_em2
	bmi	bye_bye
	lsr.l	#2,d1
	move.l	d1,size1	*adpcm size
	move.l	d2,buffer1

	move.l	size1(pc),d0
	add.l	d0,d0		*d0=data count
	move.l	buffer1(pc),a1
	moveq.l	#0,d6
	move.w	(a6),d6
	beq     cpta1		*0は100なので無視
	bsr	do_level
cpta1:
	tst.b	(a5)		*pitch shiftはないケース
	beq	cpta2

	move.l	size1(pc),d0
	lsl.l	#3,d0		*一応８倍の大きさを取る
	move.l	d0,-(sp)
	DOS	_MALLOC
	addq.w	#4,sp
	move.l	d0,mm_error
	bsr	print_em2
	bmi	bye_bye
	move.l	d0,buffer3
	move.l	d0,a1		*destination
	move.l	buffer1(pc),a2	*source
	move.l	size1(pc),d0
	add.l	d0,d0		*d0=data count
	move.b	(a5),d6
	ext.w	d6
	add.w	#12,d6
	add.w	d6,d6
	cmpi.b	#$18,d6
	bcs	cpta_shift_down	*ピッチを下げるケース
				*ピッチを上げるケース
	lea	frq_tbl(pc),a5
	move.w	-26(a5,d6.w),d4
	move.b	#1,d6		*set switch
	bra	cpta_bsr_do_pitch
cpta_shift_down:
	lea	frq_tbl2(pc),a5
	move.w	(a5,d6.w),d4
	st	d6		*set switch
cpta_bsr_do_pitch:
	bsr	do_pitch	*ピッチチェンジ
	bsr	mfree_pcm_a	*もういらない
	move.l	a1,d0
	lsr.l	#2,d0
	move.l	d0,size1
	move.l	buffer3(pc),buffer1
cpta2:
	bsr	make_adpcm_a
	bsr	print_em2
	bmi	bye_bye
	lea	fn2(pc),a1
	move.l	size1(pc),d1
	move.l	address1(pc),a2
	bsr	write_data
	bsr	print_em2
	bmi	bye_bye
	bra	mission_complete

_truncate:			*波形切り出し
	bsr	disp_proc
	move.b	fn1(pc),d0
	and.b	fn2(pc),d0
	beq	fn_error	*ファイルネームが揃ってない…

	lea	fn1(pc),a1
	bsr	read_data
	bsr	print_em2
	bmi	bye_bye
	move.l	d1,size1
	tst.b	fn1_p16
	bpl	@f
	move.l	d2,buffer1
	bra	go__tr__
@@:
	move.l	d2,address1

	bsr	make_pcm_a
	bsr	mfree_adpcm_a
go__tr__:
	move.l	size1(pc),d2	*d2=size
	lsl.l	#2,d2
	movea.l	buffer1(pc),a1	*base data addr.
	move.l	a1,buffer2	*非常用
	lea	-2(a1,d2.l),a2	*a2=end addr
	move.l	a2,size2	*一時的にこうしておく
get_hd_lp_2:
	move.w	(a1)+,d0
	bpl	@f
	neg.w	d0
@@:
	cmpa.l	a1,a2
	bls	exit_qt2	*最後まで来てしまった…
	cmp.w	truncate_cmd(pc),d0
	bls	get_hd_lp_2
	subq.w	#2,a1
	move.l	a1,buffer2	*save start
	addq.w	#2,a1
get_tl_2:
	moveq.l	#0,d1		*found counter
get_tl_lp_2:
	move.w	(a1)+,d0
	bpl	@f
	neg.w	d0
@@:
	cmpa.l	a1,a2
	bls	exit_qt2
	cmp.w	truncate_cmd(pc),d0
	bhi	get_tl_lp_2
	move.l	a1,work1
get_tl_lp1_2:
	addq.l	#1,d1		*find=find+1
	move.w	(a1)+,d0
	bpl	@f
	neg.w	d0
@@:
	cmpa.l	a1,a2
	bls	exit_qt2
	cmp.w	truncate_cmd(pc),d0
	bhi	get_tl_2
	cmpi.b	#meyasu*2,d1
	bcs	get_tl_lp1_2
	move.l	work1(pc),d0
	subq.l	#2,d0
	move.l	d0,size2
exit_qt2:
	move.l	size2(pc),d0	*サイズの計算
	addq.l	#2,d0
	movea.l	buffer2(pc),a2
	sub.l	a2,d0		*d0=pcm data size
	tst.b	fn2_p16
	bpl	@f
	move.l	d0,d1		*size
	bra	do_wrt__tr
@@:
	lsr.l	#2,d0		*d0=adpcm data size
	move.l	d0,size2
	bsr	make_adpcm_b
	move.l	size2(pc),d1	*save size
	movea.l	address2(pc),a2
do_wrt__tr:
	lea	fn2(pc),a1	*destination file name
	clr.l	buffer2		*dummy
	bsr	write_data
	bsr	print_em2
	bmi	bye_bye
	bra	mission_complete

pv_com:				*ピッチチェンジやボリュームチェンジ
	bsr	disp_proc
	move.b	fn1(pc),d0
	and.b	fn2(pc),d0
	tst.b	mix_cmd
	beq	pv0
	and.b	fn3(pc),d0
pv0:
	tst.b	d0
	beq	fn_error	*ファイルネームが揃ってない…

	lea	fn1(pc),a1
	bsr	read_data
	bsr	print_em2
	bmi	bye_bye
	tst.b	fn1_p16		*ファイルネームの拡張子チェック
	bpl	case_pv_adp
	move.l	d1,d3
	add.l	d1,d1		*2倍のエリアを確保(変換したPCMデータを格納するための)
	move.l	d1,-(sp)
	DOS	_MALLOC
	addq.w	#4,sp
	move.l	d0,mm_error
	bsr	print_em2
	bmi	bye_bye
	move.l	d0,buffer1
	move.l	d0,a1		*destinaton pcm buffer
	move.l	d2,a0		*source pcm buffer
	move.l	d3,d0
	lsr.l	d0		*get pcm count
	moveq.l	#0,d1
	move.b	(a5),d1		*pitch
	add.b	#12,d1
	moveq.l	#0,d6
	move.w	(a6),d6		*level
	bne	@f
	moveq.l	#100,d6		*0は100にする
@@:
	pea	(a0)
	bsr	do_pitch_vol	*>a1.l=new size
	DOS	_MFREE		*読み込んだPCMデータを破棄
	addq.w	#4,sp
	bra	pv_00
case_pv_adp:
	move.l	d1,size1
	move.l	d2,address1
	lsl.l	#3,d1		*adpcm sizeの8倍のエリアを確保(for transfomed pcm data)
	move.l	d1,-(sp)
	DOS	_MALLOC
	addq.w	#4,sp
	move.l	d0,mm_error
	bsr	print_em2
	bmi	bye_bye
	move.l	d0,buffer1
	move.l	d0,a1		*pcm buffer
	move.l	d2,a0		*adpcm buffer
	move.l	size1(pc),d0	*adpcm size
	moveq.l	#0,d1
	move.b	(a5),d1		*pitch
	add.b	#12,d1
	moveq.l	#0,d6
	move.w	(a6),d6		*level
	bne	@f
	moveq.l	#100,d6		*0は100にする
@@:
	bsr	adpcm_to_pcm	*pcm変換
	bsr	mfree_adpcm_a
pv_00:
	pea	(a1)		*new size
	move.l	buffer1(pc),-(sp)
	DOS	_SETBLOCK
	addq.w	#8,sp		*縮小
	move.l	a1,d0
	lsr.l	#2,d0
	move.l	d0,size1	*new adpcm size

	tst.b	mix_cmd
	beq	pv_sv		*mixの時はもう１つのファイルを読む

	lea	fn2(pc),a1
	bsr	read_data
	bsr	print_em2
	bmi	bye_bye
	tst.b	fn2_p16
	bpl	case_pv_adp2
	move.l	d1,d3
	add.l	d1,d1		*2倍のエリアを確保(変換したPCMデータを格納するため)
	move.l	d1,-(sp)
	DOS	_MALLOC
	addq.w	#4,sp
	move.l	d0,mm_error
	bsr	print_em2
	bmi	bye_bye
	move.l	d0,buffer2
	move.l	d0,a1		*destination pcm buffer
	move.l	d2,a0		*source pcm buffer
	move.l	d3,d0
	lsr.l	d0		*get pcm count
	moveq.l	#0,d1
	move.b	pitch_cmd+1(pc),d1
	add.b	#12,d1
	moveq.l	#0,d6
	move.w	volume_cmd+2(pc),d6
	bne	@f
	moveq.l	#100,d6		*0は100にする
@@:
	pea	(a0)
	bsr	do_pitch_vol	*>a1.l=new size
	DOS	_MFREE		*読み込んだPCMデータを破棄
	addq.w	#4,sp
	bra	pv_01
case_pv_adp2:
	move.l	d1,size2
	move.l	d2,address2
	lsl.l	#3,d1		*adpcm sizeの8倍のエリアを確保(for pcm data)
	move.l	d1,-(sp)
	DOS	_MALLOC
	addq.w	#4,sp
	move.l	d0,mm_error
	bsr	print_em2
	bmi	bye_bye
	move.l	d0,buffer2
	move.l	d0,a1		*pcm buffer
	move.l	d2,a0		*adpcm buffer
	move.l	size2(pc),d0	*adpcm size
	moveq.l	#0,d1
	move.b	pitch_cmd+1(pc),d1
	add.b	#12,d1
	moveq.l	#0,d6
	move.w	volume_cmd+2(pc),d6
	bne	@f
	moveq.l	#100,d6		*0は100にする
@@:
	bsr	adpcm_to_pcm	*pcm変換
	bsr	mfree_adpcm_b
pv_01:
	pea	(a1)		*new size
	move.l	buffer2(pc),-(sp)
	DOS	_SETBLOCK
	addq.w	#8,sp		*縮小
	move.l	a1,d0
	lsr.l	#2,d0
	move.l	d0,size2	*new adpcm size
*do_mix_fn1_fn2:
	move.l	size1(pc),d1
	move.l	size2(pc),d2
	move.l	buffer1(pc),a1
	move.l	buffer2(pc),a2
	bsr	mix_pcm		*destinationは自動確保 > addr.=buffer3, size=dest_size
	bsr	print_em2
	bmi	bye_bye

	tst.b	fn3_p16
	bpl	@f
	move.l	dest_size(pc),d1	*size
	lsl.l	#2,d1			*PCMサイズへ
	move.l	buffer3(pc),a2
	bra	do_mixwrt
@@:
	move.l	dest_size(pc),-(sp)
	DOS	_MALLOC
	addq.w	#4,sp
	move.l	d0,mm_error
	bsr	print_em2
	bmi	bye_bye
	move.l	d0,a0			*destination
	move.l	dest_size(pc),d0	*size
	move.l	buffer3(pc),a1		*source
	movem.l	d0/a0,-(sp)
	pea	(a1)
	bsr	pcm_to_adpcm
	DOS	_MFREE
	addq.w	#4,sp
	movem.l	(sp)+,d1/a2
do_mixwrt:
	pea	(a2)
	lea	fn3(pc),a1		*ミックス指定がある時は第三のファイルネームで
	bsr	write_data
	DOS	_MFREE
	addq.w	#4,sp
	bsr	print_em2
	bmi	bye_bye
	bra	mission_complete

pv_sv:					*ミックスでないケース
	tst.b	fn2_p16
	bpl	@f
	move.l	size1(pc),d1		*PCMデータをセーブ
	lsl.l	#2,d1
	move.l	buffer1(pc),a2
	bra	do_pvwrt
@@:
	move.l	size1(pc),-(sp)
	DOS	_MALLOC
	addq.w	#4,sp
	move.l	d0,mm_error
	bsr	print_em2
	bmi	bye_bye
	move.l	d0,a0
	move.l	buffer1(pc),a1
	move.l	size1(pc),d0
	pea	(a0)
	pea	(a1)
	bsr	pcm_to_adpcm
	DOS	_MFREE
	addq.w	#4,sp
	move.l	size1(pc),d1		*size1
	move.l	(sp)+,a2		*address1
do_pvwrt:
	lea	fn2(pc),a1
	pea	(a2)
	bsr	write_data
	DOS	_MFREE
	addq.w	#4,sp
	bsr	print_em2
	bmi	bye_bye
	bra	mission_complete

do_pitch_vol:
	* < a0=source pcm data buffer
	* < a1=destination pcm data buffer
	* < d0.l=pcm data count
	* < d1.w=note shift(0～b～c(non shift)～d～18)
	* < d6.l=volume value(0%～200%)
	* > a1=data size
				*パーセンテージを１６進変換
	lea	levelchg(pc),a6
	clr.b	up_down-levelchg(a6)

	move.l	a1,-(sp)
	lsl.l	#7,d6
	divu	#100,d6
	andi.l	#$ffff,d6	*d6=0～200→0～256

	add.w	d1,d1		*d1=d1*2
	cmpi.b	#$18,d1
	beq	~~est		*ピッチシフトは無しのケース
	bcs	~~shift_down	*ピッチを下げるケース
*~~shift_up				*ピッチを上げるケース
	lea	frq_tbl(pc),a5
	move.w	-26(a5,d1.w),frq_flg-frq_tbl(a5)
	move.b	#1,up_down-frq_tbl(a5)	*set switch
	bra	~~init_wk
~~shift_down:
	lea	frq_tbl2(pc),a5
	move.w	(a5,d1.w),frq_flg-frq_tbl2(a5)
	st	up_down-frq_tbl2(a5)	*set switch
~~init_wk:
	clr.w	last_val2-levelchg(a6)
	clr.w	frq_wk-levelchg(a6)
~~est:
	lea	scaleval(pc),a5
	moveq.l	#0,d3
	moveq.l	#0,d7
	moveq.l	#$0f,d4
	clr.w	last_val-levelchg(a6)
	clr.b	hajimete-levelchg(a6)
~~atp_lp:
	move.w	(a0)+,d2
	muls	d6,d2		*d6にはパーセントが
	asr.l	#7,d2		*入っている

	tst.b	hajimete-levelchg(a6)	*初めて
	bne	~~up_or_down
	seq.b	hajimete-levelchg(a6)
	bra	~~wrt_pcm_dt
~~up_or_down:
	tst.b	up_down-levelchg(a6)
	beq	~~wrt_pcm_dt	*non pitch shift
	bmi	~~do_shift_down

	move.w	frq_flg(pc),d1
	add.w	d1,frq_wk-levelchg(a6)
	bcc	~~check_dsz
	bra	~~wrt_pcm_dt
~~do_shift_down:
	move.w	frq_flg(pc),d1
	beq	@f
	add.w	d1,frq_wk-levelchg(a6)
	bcc	~~wrt_pcm_dt
@@:
	move.w	d2,d1
	add.w	last_val2(pc),d1
	asr.w	d1		*線形補間
	move.w	d1,(a1)+
~~wrt_pcm_dt:
	move.w	d2,(a1)+	*store pcm data to buffer
~~check_dsz:
	move.w	d2,last_val2-levelchg(a6)
	subq.l	#1,d0
	bne	~~atp_lp
	sub.l	(sp)+,a1	*data size
	rts

_4_com:				*４段階のデータを生成
	bsr	disp_proc
	tst.b	fn1
	beq	fn_error	*ファイルネームが揃ってない…

	lea	fn1(pc),a1
	bsr	read_data
	bsr	print_em2
	bmi	bye_bye

	tst.b	fn1_p16		*ファイルネームチェック
	bpl	_4c_00
	move.l	d1,d3
	add.l	d1,d1		*2倍のエリアを確保(変換したPCMデータを格納するため)
	move.l	d1,-(sp)
	DOS	_MALLOC
	addq.w	#4,sp
	move.l	d0,mm_error
	bsr	print_em2
	bmi	bye_bye
	move.l	d0,buffer1
	move.l	d0,a1		*destination pcm buffer
	move.l	d2,a0		*source pcm buffer
	move.l	d3,d0
	lsr.l	d0		*get pcm count
	moveq.l	#0,d1
	move.b	(a5),d1		*pitch
	add.b	#12,d1
	moveq.l	#0,d6
	move.w	(a6),d6		*level
	bne	@f
	moveq.l	#100,d6		*0は100にする
@@:
	pea	(a0)
	bsr	do_pitch_vol	*>a1.l=new size
	DOS	_MFREE
	addq.w	#4,sp
	bra	_4c_01
_4c_00:
	move.l	d1,size1
	move.l	d2,address1
	lsl.l	#3,d1		*adpcm sizeの8倍のエリアを確保(for pcm data)
	move.l	d1,-(sp)
	DOS	_MALLOC
	addq.w	#4,sp
	move.l	d0,mm_error
	bsr	print_em2
	bmi	bye_bye
	move.l	d0,buffer1
	move.l	d0,a1		*pcm buffer
	move.l	d2,a0		*adpcm buffer
	move.l	size1(pc),d0	*adpcm size
	moveq.l	#0,d1
	move.b	(a5),d1
	add.b	#12,d1
	moveq.l	#0,d6
	move.w	(a6),d6
	bne	@f
	moveq.l	#100,d6		*0は100にする
@@:
	bsr	adpcm_to_pcm	*pcm変換
	bsr	mfree_adpcm_a	*もういらない
_4c_01:
	pea	(a1)		*new size
	move.l	buffer1(pc),-(sp)
	DOS	_SETBLOCK
	addq.w	#8,sp		*縮小
	move.l	a1,d0
	lsr.l	#2,d0
	move.l	d0,size1	*new adpcm size

	move.l	#fn2,fn_adr	*base file name
	tst.b	fn2
	bne	@f
	move.l	#fn1,fn_adr
@@:
	move.l	buffer1(pc),buffer3
	move.l	size1(pc),size3
	pea	_4_back(pc)	*push return addr.
	cmpi.b	#'V',_4_cmd
	beq	by4levels
	cmpi.b	#'P',_4_cmd
	beq	by4scales
	bra	fn_error
_4_back:
	bsr	print_em2
	bmi	bye_bye
*	bra	mission_complete

mission_complete:
	pea	done_it(pc)
	DOS	_PRINT
	addq.w	#4,sp
	bra	bye_bye
disp_proc:			*now processing
	IOCS	_OS_CUROF
	pea	proc_mes(pc)
	DOS	_PRINT
	addq.w	#4,sp
	IOCS	_B_UP_S
	rts
fn_error:			*パラメータの間違いなら
	pea	fn_er_mes(pc)
	DOS	_PRINT
	addq.w	#4,sp
	bra	bye_bye
outm_error
	pea	outm_er_mes(pc)
	DOS	_PRINT
	addq.w	#4,sp
	clr.l	mm_error
	rts
read_error:
	pea	rer_mes(pc)
	DOS	_PRINT
	addq.w	#4,sp
	rts
write_error:
	pea	wer_mes(pc)
	DOS	_PRINT
	addq.w	#4,sp
	rts
disk_full_error:
	pea	dkf_mes(pc)
	DOS	_PRINT
	addq.w	#4,sp
	rts
fop_er:
	pea	fop_mes(pc)
	DOS	_PRINT
	addq.w	#4,sp
	rts
gram_error:
	lea	title(pc),a1
	bsr	ppp

	pea	ger_mes(pc)
	DOS	_PRINT
	addq.w	#4,sp
print_hlp:
	pea	hlp_mes(pc)
	DOS	_PRINT
	addq.w	#4,sp
	bra	bye_bye

ppp:
	move.w	#$0002,-(a7)
	pea.l	(a1)
	DOS	_FPUTS
	addq.l	#6,a7
	rts

*	以下'XAPNEL.X'のルーチンの流用です
*----------------------------------------
*    X68k ADPCM PUTI NOISE ELIMINATOR
*
*		ＸＡＰＮＥＬ
*
*	 Programmed by Z.Nishikawa
*----------------------------------------
fm_addr_port:	equ	$e90001
fm_data_port:	equ	$e90003
DMADSTAT:	equ	$c32
OPMCTRL:	equ	$9da
num_of_80:	equ	32

adpcmout:			*IOCS	$60
	movem.l	d1-d2/a0-a2,-(sp)
	bsr	adpcm_end
	lea	OCR3,a0
	lea	last_param(pc),a2
	move.w	d1,(a2)+
	move.l	a1,(a2)+
	move.l	d2,(a2)+
	clr.w	(a2)
*@@:
*	tst.b	DMADSTAT.w
*	bne	@b

	moveq.l	#0,d0

	cmpi.l	#$feff,d2
	bls	adpcm_sgl	*$feff以下なら単回処理(<d0.l=0)

	move.l	d2,d0
	move.l	#$feff,d2
	divu	d2,d0
	swap	d0
	tst.w	d0
	beq	@f
	swap	d0
	bra	store_lpc
@@:
	swap	d0
	subq.w	#1,d0
store_lpc:
	move.w	d0,(a2)		*d0回数
	sub.l	d2,-4(a2)	*size補正
	moveq.l	#$80,d0		*ループ指定
adpcm_sgl:
	* a0=OCR3,a2=last_feff
*	tst.b	DMADSTAT.w
*	bne	adpcm_sgl	*待ちループ

	addq.b	#%0000_0010,d0	*play指定
	move.b	d0,DMADSTAT.w
	move.b	#%0111_1010,(a0)	*aray mode
	st	CSR3-OCR3(a0)		*status set
	move.w	#2,BTC3-OCR3(a0)	*アレイテーブルの個数
	lea	aray_tbl(pc),a2
	move.l	a2,BAR3-OCR3(a0)	*アレイテーブルのアドレス
	move.l	a1,next_at-aray_tbl(a2)
	move.w	d2,next_at+4-aray_tbl(a2)
	bsr	adpcm_dtst
	move.b	#$02,$e92001
	movem.l	(sp)+,d1-d2/a0-a2
	rts

adpcm_dtst:
	move.b	OPMCTRL.w,d0	*音声合成クロック読みだし
	tas.b	d0
	cmpi.w	#$02_00,d1
	bcs	adpcm_clkst	*5.2kHzならばクロック4MHz

	sub.w	#$02_00,d1
	andi.b	#$7f,d0		*7.8kHzならばクロック8MHz

adpcm_clkst:
	move.b	d0,OPMCTRL.w
	opmset	#$1b,d0		*クロックを書き込む
	move.w	d1,d0
	andi.b	#%0000_0011,d0
	beq	adpcm_pan_not

	cmpi.b	#3,d0		*出力チェック
	bne	adpcm_panst
adpcm_pan_not:
	eori.b	#%0000_0011,d0	*ビット反転
adpcm_panst:
	lsr.w	#6,d1		*サンプリング周波数を下位バイトへ
	andi.b	#%0000_1100,d1
	or.b	d0,d1		*出力モードを重ね合わせる
	move.b	$e9a005,d0	*周波数やパンをゲット
	andi.b	#%1111_0000,d0
	or.b	d1,d0		*設定値を作成
	move.b	#$88,CCR3-OCR3(a0)	*dma start
	move.b	d0,$e9a005	*コントロールデータ送信
	rts

adpcmmod:			*IOCS	$67
	tst.b	d1
	beq	adpcm_end
	cmpi.b	#1,d1
	beq	adpcm_stop
	cmpi.b	#2,d1
	beq	adpcm_cnt
	moveq.l	#-1,d0	*エラーコード
	rts
adpcm_end:
	moveq.l	#0,d0		*終了コード
	move.w	sr,-(sp)
	ori.w	#$0700,sr	*最上位割り込みマスク
	move.b	#$10,CCR3
	move.b	#$88,$e92003
	move.w	d0,DMADSTAT.w

	move.w	(sp)+,sr
	rts
adpcm_stop:
	moveq.l	#0,d0
	move.w	sr,-(sp)
	ori.w	#$0700,sr

	move.b	#$20,CCR3	*動作中断
				*move.b	#$88,$e92003をしないのは再開した時に音が変に鳴るから
	move.w	(sp)+,sr
	rts
adpcm_cnt:
	move.b	#$08,CCR3	*動作継続
	moveq.l	#0,d0
	rts

int_adpcm_stop:			*ＡＤＰＣＭデータの再生が終了するとここへくる
	movem.l	d0-d2/a0-a1,-(sp)
	lea	last_feff(pc),a0
	tst.w	(a0)
	beq	stop_quit
	subq.w	#1,(a0)
	move.l	#$feff,d1
	move.l	-(a0),d0	*get size
	move.l	d0,d2
	sub.l	d1,d0
	bcs	@f
	sub.l	d1,(a0)		*size=size-$feff
	move.l	d1,d2
@@:
	add.l	d1,-(a0)
	move.l	(a0),a1		*get address
	move.w	-(a0),d1	*get param

	lea	OCR3,a0
	move.b	#%0000_0010,DMADSTAT.w
	move.b	#%0111_0010,(a0)	*no aray
	st	CSR3-OCR3(a0)		*status set
	move.l	a1,MAR3-OCR3(a0)
	move.w	d2,MTC3-OCR3(a0)
	bsr	adpcm_dtst
	move.b	#$02,$e92001

	movem.l	(sp)+,d0-d2/a0-a1
	rte
stop_quit:
	move.b	#$10,CCR3	*dma stop
	st	CSR3		*status clear
	move.b	#$88,$e92003
	clr.b	DMADSTAT.w
	movem.l	(sp)+,d0-d2/a0-a1
break:				*ブレークキーが押されるとここへ
	rte

adpcm_stop_v:	ds.l	1
adpcmout_v:	ds.l	1
adpcmmod_v:	ds.l	1
aray_tbl:
	dc.l	dummy_data
	dc.w	num_of_80
next_at:
	dc.l	0
	dc.w	0
dummy_data:	dcb.b	num_of_80,$80
	.even
last_param:	ds.w	1
last_address:	ds.l	1
last_size:	ds.l	1
last_feff:	dc.w	0

set_pan_frq:
	move.w	last_param(pc),d1
	move.b	OPMCTRL.w,d0	*音声合成クロック読みだし
	tas.b	d0
	cmpi.w	#$02_00,d1
	bcs	@f		*5.2kHzならばクロック4MHz

	sub.w	#$02_00,d1
	andi.b	#$7f,d0		*7.8kHzならばクロック8MHz

@@:
	move.b	d0,OPMCTRL.w
	opmset	#$1b,d0		*クロックを書き込む
	move.w	d1,d0
	andi.b	#%0000_0011,d0
	beq	@f

	cmpi.b	#3,d0		*出力チェック
	bne	spf1
@@:
	eori.b	#%0000_0011,d0	*ビット反転
spf1:
	lsr.w	#6,d1		*サンプリング周波数を下位バイトへ
	andi.b	#%0000_1100,d1
	or.b	d0,d1		*出力モードを重ね合わせる
	move.b	$e9a005,d0	*周波数やパンをゲット
	andi.b	#%1111_0000,d0
	or.b	d1,d0		*設定値を作成
	move.b	d0,$e9a005	*コントロールデータ送信
	rts

set_vect:				*ADPCMの停止処理の書き換え
	movem.l	d0-d1/a1,-(sp)

	lea	break(pc),a1		*ブレークキーベクタの乗っ取り
	move.l	$ac.w,break_org
	move.l	a1,$ac.w

	lea	abort(pc),a1
	move.w	#$01fd,d1	*abort
	IOCS	_B_INTVCS
	move.l	d0,last_iocs-abort(a1)

	moveq.l	#$6a,d1
	lea	int_adpcm_stop(pc),a1
	IOCS	_B_INTVCS
	move.l	d0,adpcm_stop_v-int_adpcm_stop(a1)

	move.w	#$160,d1	*IOCS	_ADPCMOUTを乗っ取る
	lea	adpcmout(pc),a1
	IOCS	_B_INTVCS
	move.l	d0,adpcmout_v-adpcmout(a1)

	move.w	#$167,d1	*IOCS	_ADPCMMODを乗っ取る
	lea	adpcmmod(pc),a1
	IOCS	_B_INTVCS
	move.l	d0,adpcmmod_v-adpcmmod(a1)

	move.w	#$0403,d1		*frq/pan
	moveq.l	#num_of_80,d2		*length
	lea	dummy_data(pc),a1
	IOCS	_ADPCMOUT		*ダミー再生

*	move.b	#%0001_0000,$e840c7	*DMA_CCR SAB=1 強制停止
*	st.b	$e840c0			*$FF -> $e840c0
*	move.b	#%0000_0100,$e92001	*adpcm rec start
*	move.b	#%0000_0001,$e92001	*adpcm all end

	move.w	#NOP,back_vect		*ベクタ復元必要

	movem.l	(sp)+,d0-d1/a1
	rts

back_vect:			*ベクタの復元
	rts
	movem.l	d0-d1/a1,-(sp)

	moveq.l	#0,d1
	IOCS	_ADPCMMOD	*まず停止

	move.l	break_org(pc),$ac.w	*ブレークキーベクタの復元

	movea.l	last_iocs(pc),a1
	move.w	#$01fd,d1	*abort
	IOCS	_B_INTVCS

	movea.l	adpcm_stop_v(pc),a1
	moveq.l	#$6a,d1
	IOCS	_B_INTVCS	*get back int adpcm stop vector

	movea.l	adpcmout_v(pc),a1
	move.w	#$160,d1
	IOCS	_B_INTVCS	*get back int adpcmout vector

	movea.l	adpcmmod_v(pc),a1
	move.w	#$167,d1
	IOCS	_B_INTVCS	*get back int adpcmmod vector
	movem.l	(sp)+,d0-d1/a1

	rts

abort:				*ABORT時の処理
	bsr	back_vect
	move.l	last_iocs(pc),-(sp)
	rts

	.bss
_8bit_pcm:	ds.b	1
	.even
num_of_files:	ds.l	1
fn_buf:		ds.l	1
fn_buf_size:	ds.l	1

filbuf:		ds.b	21
ATR:		ds.b	1
		ds.w	2
FILE_SIZE:	ds.l	1
FILE_NAME:	ds.b	23
	.even
suji:		ds.b	16
fn1:		DS.B	92
fn2:		DS.B	92
fn3:
wfile_name:	DS.B	92
cdr_str:	ds.b	1
		ds.b	2	*':\'
pathbf:		ds.b	92
wfn1:		ds.b	92
wfn2:		ds.b	92
	.even
param_mem:
wfn3:		ds.b	92	*別目的にも使用される
mix_mode:	ds.b	1
played:		ds.b	1
input_buffer:	ds.b	1
		ds.b	1
		ds.b	256
prt_md:		ds.b	1
	.even
recording_time:	ds.w	1
bank_sel:	ds.b	1
ESC_ON:		ds.b	1
use_fm:		ds.b	1
data_a:		ds.b	1
data_b:		ds.b	1
pl_mi:		ds.b	1
up_down:	ds.b	1
hajimete:	ds.b	1
fn_sj:		ds.b	1
last_point:	ds.b	1
dirofchg:	ds.b	1
dt_mem:		ds.b	1
fn1_p16:	ds.b	1
fn2_p16:	ds.b	1
fn3_p16:	ds.b	1
	.even
ssp:		ds.l	1
rec_data_size:	ds.l	1
file_list:	ds.l	1
reg_buff:	ds.l	15
reg_buff2:	ds.l	15
reg_buff3:	ds.l	15
current_drive:	ds.w	1
mem_max:	ds.l	1
fo_error:	ds.l	1
mm_error:	ds.l	1
rd_error:	ds.l	1
wr_error:	ds.l	1
disk_full:	ds.l	1
get_pt_buf:	ds.l	1
get_pt_buf1:	ds.l	1

org_sp:		ds.l	1	*元々のスタックを格納する所
last_val:	ds.w	1
last_val2:	ds.w	1
last_iocs:	ds.l	1	*abort vector

address1:	ds.l	1	*0
buffer1:	ds.l	1	*4
size1:		ds.l	1	*8
start_point1:	ds.l	1	*12
end_point1:	ds.l	1	*16

address2:	ds.l	1	*20
buffer2:	ds.l	1	*24
size2:		ds.l	1	*28
start_point2:	ds.l	1	*32
end_point2:	ds.l	1	*36

address3:	ds.l	1
buffer3:	ds.l	1
size3:		ds.l	1

brk_wk:		ds.w	1
work1:		ds.l	1	*汎用
work2:		ds.l	1
fn_adr:		ds.l	1
work3:		ds.l	1
offset:		ds.l	1
dest_size:	ds.l	1
add_frq:	ds.l	1
add_times:	ds.w	1
frq_flg:	ds.w	1
frq_wk:		ds.w	1
genten_y:	ds.w	1
ent_path:	ds.b	70
prt_bf:		ds.b	96
patch1:		ds.w	1
fn_buffer1:	ds.l	1
fn_point1:	ds.l	1
rd_fn:		ds.b	92
bank_a_fn:	ds.b	92
bank_b_fn:	ds.b	92
trn_rvs:	ds.l	1
break_org:	ds.l	1
atb_step:	ds.l	1	*autobend work
atb_rvs:	ds.w	1	*autobend work
atb_rvswk:	ds.w	1	*autobend work
atb_frqsrc:	ds.l	1	*autobend work
atb_frqnow:	ds.l	1	*autobend work
atb_sgn:	ds.l	1	*autobend work
		ds.b	stack
user_sp:
	.end
