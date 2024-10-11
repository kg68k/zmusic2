*-----------------------------------------------
*	      ＰＣＭファイルリンカ
*
*		  ＺＰＬＫ．Ｒ
*
*		  VERSION 1.00
*
*		 BY Z.NISHIKAWA
*-----------------------------------------------

	.include	iocscall.mac
	.include	doscall.mac
	.text

smax:		equ	32		*記述できるソースファイルの最大数
vmax:		equ	300
lmax:		equ	3		*ループの種類(0,1,2,3)
rmax:		equ	65535		*繰り返し数最大値
cmax:		equ	2		*サブ・コンバージョンタイプ(0,1,2)
tbl_size:	equ	128
loop_type:	equ	0	*loop type.w
rept_time:	equ	2	*repeat time.w
conv_type:	equ	4	*conv type.w
fsize:		equ	8	*file size.l
fname:		equ	32	*filename.b

print	macro	mes
	move.w	#2,-(sp)
	pea	mes
	DOS	_FPUTS
	addq.w	#6,sp
	endm

print2	macro	mes
	pea	mes
	DOS	_PRINT
	addq.w	#4,sp
	endm

*--------------- PROGRAM START ---------------

	lea	$10(a0),a0		*メモリブロックの変更
	lea.l	user_sp(pc),a1
	suba.l	a0,a1
	pea	(a1)
	pea	(a0)
	DOS	_SETBLOCK
	addq.w	#8,sp
	tst.l	d0
	bmi	out_of_mem

	lea	work(pc),a6
	lea	user_sp(pc),sp		*スタック設定

	move.l	a3,env_bak-work(a6)

	print	title(pc)

	tst.b	(a2)+			*パラメータある?
	beq	print_hlp

	clr.b	d_name-work(a6)
	lea	s_name_tbl(pc),a0
	move.l	a0,a4
	move.l	a0,a5
	move.w	#((smax+1)*tbl_size)/4-1,d0
@@:					*ワークの初期化
	clr.l	(a0)+
	dbra	d0,@b

	moveq.l	#smax,d6
chk_coml:
	bsr	skip_spc
	move.b	(a2)+,d0
	beq	go_ope
	cmpi.b	#'/',d0
	beq	scan_switch
	cmpi.b	#'-',d0
	beq	scan_switch
get_fname:				*ファイルネームの取得
	subq.w	#1,a2
	move.l	a5,a0
	adda.w	#fname,a5
gflp0:
	move.b	(a2)+,d0
	move.b	d0,(a5)+
	bpl	@f
	move.b	(a2)+,(a5)+		*case kanji
	bra	gflp0
@@:
	bsr	fnstr_chk
	bne	gflp0
	clr.b	-(a5)
	subq.w	#1,a2
	lea.l	tbl_size(a0),a5
	dbra	d6,chk_coml
	bra	too_many_s
scan_switch:			*最終出力形式の設定
	move.b	(a2)+,d0	*get sw name
	beq	print_hlp
	bsr	mk_capital
	cmpi.b	#'A',d0
	bne	@f
	st.b	adpout_mode-work(a6)
	moveq.l	#-1,d0
	and.b	p16out_mode(pc),d0	*両方のスイッチ・オンは矛盾
	bne	not_support
	bra	chk_coml
@@:				*ピッチベンド
	cmpi.b	#'B',d0
	bne	cutting
	bsr	chk_num
	bpl	@f
	move.b	(a2),d0
	bsr	mk_capital
	cmpi.b	#'P',d0
	bne	unex_error
	addq.w	#1,a2
	bsr	get_frq_rng
	move.l	d1,s_frq_b-work(a6)
	move.l	d2,d_frq_b-work(a6)
	bra	get_ofb
@@:
	bsr	asc_to_n
	cmpi.l	#65535,d1
	bhi	s_illegal
	move.l	d1,s_frq_b-work(a6)
	beq	s_illegal
	bsr	skip_sep	*skip ','
	bsr	chk_num
	bmi	no_d_error
	bsr	asc_to_n
	cmpi.l	#65535,d1
	bhi	d_illegal
	move.l	d1,d_frq_b-work(a6)
	beq	d_illegal
	cmp.l	s_frq_b(pc),d1	*同じでは意味がない。
	beq	d_illegal
get_ofb:			*オフセットポイントの設定
	bsr	skip_sep	*skip ','
	bsr	chk_num
	bpl	@f
	cmpi.b	#',',(a2)
	bne	chk_coml
	bra	get_b_size
@@:
	bsr	asc_to_n
	move.l	d1,b_offset-work(a6)
	bmi	b_offset_err
get_b_size:
	bsr	skip_sep
	bsr	chk_num
	bmi	chk_coml
	bsr	asc_to_n
	move.l	d1,b_size-work(a6)
	beq	b_size_err
	bmi	b_size_err
	bra	chk_coml
cutting:			*切り出しオプション
	cmpi.b	#'C',d0
	bne	envchg?
				*オフセット値
	bsr	chk_num
	bpl	@f
	cmpi.b	#',',(a2)
	bne	unex_error	*意味不明
	bra	get_cutsize
@@:
	bsr	asc_to_n
	move.l	d1,cut_offset-work(a6)
	bmi	cut_offset_illegal
get_cutsize:			*切り出しサイズ
	bsr	skip_sep
	bsr	chk_num
	bmi	chk_coml
	bsr	asc_to_n
	move.l	d1,cut_size-work(a6)
	bmi	cut_size_illegal
	or.l	cut_offset(pc),d1
	beq	not_support	*両方のパラメータを省略はできない。
	bra	chk_coml
envchg?:			*エンベロープ変更設定
	cmpi.b	#'F',d0
	bne	impulse?
	st.b	fade_mode-work(a6)
				*オフセット値
	bsr	chk_num
	bpl	@f
	cmpi.b	#',',(a2)
	bne	chk_coml	*意味不明
	bra	get_envlvl
@@:
	bsr	asc_to_n
	move.l	d1,env_offset-work(a6)
	bmi	env_offset_illegal
get_envlvl:			*エンベロープ初期／最終音量
	bsr	skip_sep
	bsr	chk_num
	bpl	@f
	cmpi.b	#',',(a2)
	bne	chk_coml	*意味不明
	bra	get_envtype
@@:
	bsr	asc_to_n
	cmpi.l	#127,d1
	bhi	envlvl_illegal
	move.b	d1,env_lvl-work(a6)
get_envtype:			*エンベロープ変更種類
	bsr	skip_sep
	bsr	chk_num
	bmi	chk_coml
	bsr	asc_to_n
	cmpi.l	#1,d1
	bhi	envtype_illegal
	move.b	d1,env_type-work(a6)
	bne	chk_coml
	tst.l	env_offset-work(a6)
	beq	env_offset_illegal	*フェードインでオフセット０はあり得ない
	bra	chk_coml
impulse?:				*たたみ込みを行なうか
	cmpi.b	#'I',d0
	bne	p16otmd?
	st.b	impulse_mode-work(a6)
	bsr	skip_spc
	lea	impulse_name(pc),a1
implp0:
	move.b	(a2)+,d0
	move.b	d0,(a1)+
	bpl	@f
	move.b	(a2)+,(a1)+
	bra	implp0
@@:
	bsr	fnstr_chk
	bne	implp0
	clr.b	-(a1)
	subq.w	#3,a1
	move.b	(a1)+,d0
	bsr	mk_capital
	cmpi.b	#'P',d0
	bne	unid_err
	move.b	(a1)+,d0
	bsr	mk_capital
	cmpi.b	#'C',d0
	bne	impulse_p16?
	move.b	(a1)+,d0
	bsr	mk_capital
	cmpi.b	#'M',d0
	bne	unid_err
	lea	impulse_name(pc),a1
	bsr	read_data2
	pea	(a0)
	move.l	impulse_buff(pc),a0
	move.l	impulse_size(pc),d1
	move.l	d1,d0
	lsl.l	#2,d0
	move.l	d0,impulse_size-work(a6)
	move.l	d0,-(sp)
	DOS	_MALLOC
	addq.l	#4,sp
	move.l	d0,impulse_buff-work(a6)
	bmi	out_of_mem
	move.l	d0,a1
	move.l	d1,d0
	bsr	just_adpcm_to_pcm
	pea	(a0)
	DOS	_MFREE
	addq.w	#4,sp
	move.l	(sp)+,a0
	bra	chk_coml
impulse_p16?:
	cmpi.b	#'1',d0
	bne	unid_err
	cmpi.b	#'6',(a1)+
	bne	unid_err
	lea	impulse_name(pc),a1
	bsr	read_data2
	bra	chk_coml
p16otmd?:
	cmpi.b	#'P',d0
	bne	reverse?
	st.b	p16out_mode-work(a6)
	moveq.l	#-1,d0
	and.b	adpout_mode(pc),d0	*両方のスイッチ・オンは矛盾
	bne	not_support
	bra	chk_coml
reverse?:			*最終逆転
	cmpi.b	#'R',d0
	bne	@f
	st.b	revout_mode-work(a6)
	bra	chk_coml
@@:				*最終音量設定
	cmpi.b	#'V',d0
	bne	@f
	bsr	chk_num
	bmi	no_v_param
	bsr	asc_to_n
	cmpi.l	#vmax,d1
	bhi	v_illegal
	lsl.l	#8,d1
	divu	#100,d1
	move.w	d1,vol_val-work(a6)
	bra	chk_coml
@@:				*最終的に周波数を変換する
	cmpi.b	#'T',d0
	bne	sub_con
	bsr	chk_num
	bpl	@f
	move.b	(a2),d0
	bsr	mk_capital
	cmpi.b	#'P',d0
	bne	unex_error
	addq.w	#1,a2
	bsr	get_frq_rng
	move.l	d1,s_frq-work(a6)
	move.l	d2,d_frq-work(a6)
	bra	get_oft
@@:
	bsr	asc_to_n
	cmpi.l	#65535,d1
	bhi	s_illegal
	move.l	d1,s_frq-work(a6)
	beq	s_illegal
	bsr	skip_sep	*skip ','
	bsr	chk_num
	bmi	no_d_error
	bsr	asc_to_n
	cmpi.l	#65535,d1
	bhi	d_illegal
	move.l	d1,d_frq-work(a6)
	beq	d_illegal
	cmp.l	s_frq(pc),d1	*同じでは意味がない。
	beq	d_illegal
get_oft:			*オフセットポイントの設定
	bsr	skip_sep	*skip ','
	bsr	chk_num
	bpl	@f
	cmpi.b	#',',(a2)
	bne	chk_coml
	bra	get_t_size
@@:
	bsr	asc_to_n
	move.l	d1,t_offset-work(a6)
	bmi	t_offset_err
get_t_size:
	bsr	skip_sep
	bsr	chk_num
	bmi	chk_coml
	bsr	asc_to_n
	move.l	d1,t_size-work(a6)
	beq	t_size_err
	bmi	t_size_err
	bra	chk_coml
sub_con:			*変換パラメータの設定
	cmpi.b	#'X',d0
	bne	print_hlp
				*ループタイプ取り出し
	bsr	chk_num
	bpl	@f
	cmpi.b	#',',(a2)
	bne	unex_error	*意味不明
	bra	get_rept
@@:
	bsr	asc_to_n
	cmpi.l	#lmax,d1
	bhi	l_illegal
	bne	@f
	subq.w	#4,d1		*3 -> -1
@@:
	move.w	d1,loop_type(a5)
get_rept:			*ループ回数の取り出し
	bsr	skip_sep
	bsr	chk_num
	bpl	@f
	cmpi.b	#',',(a2)
	bne	unex_error	*意味不明
	bra	get_srccnv
@@:
	bsr	asc_to_n
	cmpi.l	#rmax,d1
	bhi	r_illegal
	move.w	d1,rept_time(a5)
get_srccnv:			*ソースファイルのサブ・コンバージョン形式
	bsr	skip_sep
	bsr	chk_num
	bmi	chk_coml
	bsr	asc_to_n
	cmpi.l	#cmax,d1
	bhi	r_illegal
	bne	@f
	subq.w	#3,d1		*2 -> -1
@@:
	move.w	d1,conv_type(a5)
	beq	chk_coml	*non touch
	bmi	chk_coml	*to pcm
	tst.w	loop_type(a5)
	beq	chk_coml	*forward
	bra	not_support	*ADPCMのリバースやオルタネートは出来ない

get_frq_rng:
	bsr	skip_spc
	moveq.l	#0,d0
	cmpi.b	#'-',(a2)
	bne	@f
	addq.w	#1,a2
	moveq.l	#-1,d0
@@:
	bsr	asc_to_n
	subq.l	#1,d1
	bcs	unex_error	*±0では無意味
	cmpi.l	#143,d1
	bhi	illegal_frq
	add.l	d1,d1
	moveq.l	#0,d2
	move.w	frq_tbl(pc,d1.l),d2
	move.l	#65280,d1
	tst.l	d0
	bmi	@f
	exg.l	d1,d2
@@:
	rts

frq_tbl:
	* for i=1 to 144
	*  frq_tbl=(2^-(i/12))*65280
	* next
	dc.w	61616,58158,54894,51813,48905,46160,43569,41124,38816,36637,34581,32640
	dc.w	30808,29079,27447,25906,24452,23080,21785,20562,19408,18319,17290,16320
	dc.w	15404,14539,13723,12953,12226,11540,10892,10281,9704,9159,8645,8160
	dc.w	7702,7270,6862,6477,6113,5770,5446,5140,4852,4580,4323,4080
	dc.w	3851,3635,3431,3238,3057,2885,2723,2570,2426,2290,2161,2040
	dc.w	1926,1817,1715,1619,1528,1442,1362,1285,1213,1145,1081,1020
	dc.w	963,909,858,810,764,721,681,643,606,572,540,510
	dc.w	481,454,429,405,382,361,340,321,303,286,270,255
	dc.w	241,227,214,202,191,180,170,161,152,143,135,128
	dc.w	120,114,107,101,96,90,85,80,76,72,68,64
	dc.w	60,57,54,51,48,45,43,40,38,36,34,32
	dc.w	30,28,27,25,24,23,21,20,19,18,17,16

go_ope:				*書き出しファイル名の取り出し
	lea	s_name_tbl+tbl_size(pc),a0
	cmpa.l	a0,a5		*ファイル名パラメータが少なすぎる
	bls	syntax_error
	suba.w	#tbl_size-fname,a5
	move.l	a5,a4
	lea	d_name(pc),a0
@@:
	move.b	(a5)+,(a0)+
	bne	@b
	clr.b	(a4)		*終了コードとする

	lea	s_name_tbl-tbl_size(pc),a5
	moveq.l	#0,d3		*読み込みバッファ最大値
	moveq.l	#0,d4		*サブ・コンバッファ最大値
fsmx_lp:			*最大のファイルサイズはいくつ？
	lea	tbl_size(a5),a5
	tst.b	fname(a5)	*最後かどうか
	beq	get_bff
	lea	fname(a5),a1
	bsr	check_fopen	*>d1.l=fsize
	cmp.l	d3,d1
	bls	@f
	move.l	d1,d3
@@:
	tst.w	conv_type(a5)
	beq	fsmx_lp			*no touch
	bpl	@f
	lsl.l	#2,d1
	bra	fsmx0
@@:
	lsr.l	#2,d1
fsmx0:
	cmp.l	d4,d1
	bls	fsmx_lp
	move.l	d1,d4
	bra	fsmx_lp
get_bff:			*目的のＰＣＭデータを作成する。
	move.l	d3,-(sp)
	DOS	_MALLOC
	addq.l	#4,sp
	move.l	d0,src_temp1-work(a6)
	bmi	out_of_mem

	tst.l	d4		*サブ・コンがあるか
	bne	@f		*ある
	move.l	d3,d4		*便宜上取る
@@:
	move.l	d4,-(sp)	*サブ・コンバッファ確保
	DOS	_MALLOC
	addq.l	#4,sp
	move.l	d0,src_temp2-work(a6)
	bmi	out_of_mem
				*テンポラリエリア確保
	move.l	#-1,-(sp)
	DOS	_MALLOC
	addq.w	#4,sp
	andi.l	#$00ffffff,d0
	move.l	d0,a3
	move.l	d0,-(sp)	*取れる限り確保
	DOS	_MALLOC
	addq.w	#4,sp
	move.l	d0,pcm_temp-work(a6)
	bmi	out_of_mem
	move.l	d0,a4		*destination buffer address
	add.l	d0,a3		*mem. block last address

	* < a3.l=mem. block last address
	* < a4.l=mem. destination buffer address
	lea	s_name_tbl(pc),a5
mkpcm_lp01:
	tst.b	fname(a5)		*最後かどうか
	beq	temp_clr		*最後なら次の処理へ
	lea	fname(a5),a1
	bsr	read_data		*d1:size d2:address
	tst.w	conv_type(a5)
	beq	non_tch
	bpl	to_adp
*to_p16:				*16bit pcmへ
	move.l	d1,d0			*size
	move.l	d2,a0			*adpcm source
	move.l	src_temp2(pc),a1	*pcm buffer
	lsl.l	#2,d1
	move.l	d1,fsize(a5)		*save size
	move.w	vol_val(pc),d6
	bsr	adpcm_to_pcm
	bra	link_it
to_adp:
	move.l	d1,d0			*size
	move.l	d2,a1			*pcm source
	move.l	src_temp2(pc),a0	*adpcm buffer
	lsr.l	#2,d1
	move.l	d1,fsize(a5)		*save size
	move.l	d0,d1
	btst.l	#0,d1
	bne	size_error		*ＰＣＭデータが奇数サイズということはあり得ない。
	lsr.l	d1
	movem.l	d0/a0/a1,-(sp)
	move.w	vol_val(pc),d2
@@:
	move.w	(a1),d0
	muls	d2,d0			*音量変換
	asr.l	#8,d0
	move.w	d0,(a1)+
	subq.l	#1,d1
	bne	@b
	movem.l	(sp)+,d0/a0/a1
	bsr	pcm_to_adpcm
	bra	link_it
non_tch:
	move.l	d2,a1			*source
	move.l	src_temp2(pc),a2	*destination
	move.l	d1,d2			*size
	move.l	d1,fsize(a5)		*save size
	lsr.l	#2,d1
	andi.l	#$0000_0003,d2
@@:
	tst.l	d1
	beq	@f
	move.l	(a1)+,(a2)+
	subq.l	#1,d1
	bra	@b
@@:
	tst.b	d2
	beq	@f
	move.b	(a1)+,(a2)+
	subq.b	#1,d2
	bra	@b
@@:
link_it:
	move.w	loop_type(a5),d0
	beq	loop_fwd
	bmi	loop_rev
	subq.w	#1,d0
	beq	loop_alt1
*loop_alt2:				*→←
	move.w	rept_time(a5),d2
	bne	@f			*0は１回と見なす
	addq.w	#1,d2
@@:
	move.l	src_temp2(pc),a1	*source
	move.l	a1,d4
	add.l	fsize(a5),d4		*edge address
**	addq.w	#2,a1
	bsr	do_cnv_fwd
	subq.w	#1,d2
	beq	exit_al2

	move.l	src_temp2(pc),d4	*edge address
	move.l	d4,a1
	add.l	fsize(a5),a1		*source
**	subq.w	#2,a1
	bsr	do_cnv_rev
	subq.w	#1,d2
	beq	exit_al2

	dbra	d2,@b
exit_al2:
	bra	next_mkpcm

loop_fwd:
	move.l	src_temp2(pc),a1	*source
	move.l	a1,d4
	add.l	fsize(a5),d4		*edge address
	move.w	rept_time(a5),d2
	beq	@f			*0は１回と見なす
	subq.w	#1,d2
@@:
	bsr	do_cnv_fwd
	dbra	d2,@b
	bra	next_mkpcm

loop_rev:
	move.l	src_temp2(pc),d4	*edge address
	move.l	d4,a1
	add.l	fsize(a5),a1		*source
	move.w	rept_time(a5),d2
	beq	@f			*0は１回と見なす
	subq.w	#1,d2
@@:
	bsr	do_cnv_rev
	dbra	d2,@b
	bra	next_mkpcm

loop_alt1:				*←→
	move.w	rept_time(a5),d2
	bne	@f			*0は１回と見なす
	addq.w	#1,d2
@@:
	move.l	src_temp2(pc),d4	*edge address
	move.l	d4,a1
	add.l	fsize(a5),a1		*source
**	subq.w	#2,a1
	bsr	do_cnv_rev
	subq.w	#1,d2
	beq	exit_al1

	move.l	src_temp2(pc),a1	*source
	move.l	a1,d4
	add.l	fsize(a5),d4		*edge address
**	addq.w	#2,a1
	bsr	do_cnv_fwd
	subq.w	#1,d2
	beq	exit_al1

	dbra	d2,@b
exit_al1:
next_mkpcm:
	lea	tbl_size(a5),a5
	bra	mkpcm_lp01

temp_clr:
	move.l	a4,d1
	move.l	pcm_temp(pc),d4
	sub.l	d4,d1
	move.l	d1,-(sp)		*size
	move.l	d4,-(sp)		*address
	DOS	_SETBLOCK
	addq.w	#8,sp
	tst.l	d0
	bmi	out_of_mem

	move.l	src_temp1(pc),-(sp)	*テンポラリエリア解放
	DOS	_MFREE
	addq.w	#4,sp

	move.l	src_temp2(pc),-(sp)
	DOS	_MFREE
	addq.w	#4,sp
frq_chg?:				*周波数変換
	tst.l	s_frq-work(a6)
	beq	portament?
					*テンポラリエリア確保
	move.l	pcm_temp(pc),a2
	move.l	a2,src_temp1-work(a6)	*交換

	move.l	#-1,-(sp)
	DOS	_MALLOC
	addq.w	#4,sp
	andi.l	#$00ffffff,d0
	move.l	d0,d5
	move.l	d0,-(sp)	*取れる限り確保
	DOS	_MALLOC
	addq.w	#4,sp
	move.l	d0,pcm_temp-work(a6)
	bmi	out_of_mem
	move.l	d0,a1		*a1.l=destination buffer address
	add.l	d0,d5		*d5.l=mem. block end address
				*a2.l=source pcm data address
				*パラメータチェック
	move.l	a2,d0		*push a2
	move.l	t_offset(pc),d1
	add.l	a2,d1		*オフセット値が巨大過ぎないか
	bcs	t_offset_err
	move.l	a4,d4
	sub.l	d1,d4		*オフセットが大きすぎないか
	bcs	t_offset_err
	beq	t_offset_err
	move.l	d0,a2		*pop a2

	move.l	t_offset(pc),d1
	lsr.l	d1
	beq	frq_chg_ope	*オフセットなし
@@:				*元データ転送
	move.w	(a2)+,(a1)+
	cmp.l	a1,d5
	bls	out_of_mem
	subq.l	#1,d1
	bne	@b
frq_chg_ope:
	move.l	t_size(pc),d4
	bne	@f
				*サイズ指定がない場合
	move.l	a4,d4
	sub.l	a2,d4
	bra	do_frqchg
@@:				*サイズ指定がある場合
	move.l	d4,d0
	add.l	a2,d0
	cmp.l	a4,d0		*サイズが大きすぎないか
	bhi	t_size_err
do_frqchg:
	lsr.l	d4		*d4.l=data count
	beq	cant_t_err
	move.l	s_frq(pc),d6	*d6.l=source frq
	move.l	d_frq(pc),d7	*d7.l=destination frq
	bsr	do_ajfr		*周波数変換	> a1.l=destination end address
	tst.l	t_size-work(a6)
	beq	mfreetmp1	*サイズ指定がなかった場合はTAIL処理省略
set_tail_ope:			*元データ転送
	move.l	a4,d1
	sub.l	a2,d1
	lsr.l	d1
	beq	mfreetmp1
@@:
	move.w	(a2)+,(a1)+
	cmp.l	a1,d5
	bls	out_of_mem
	subq.l	#1,d1
	bne	@b
mfreetmp1:
	move.l	a1,a4			*辻褄合わせ
	move.l	pcm_temp(pc),d1
	sub.l	d1,a1

	move.l	src_temp1(pc),-(sp)
	DOS	_MFREE
	addq.w	#4,sp

	pea.l	(a1)			*size
	move.l	d1,-(sp)		*address
	DOS	_SETBLOCK
	addq.w	#8,sp
	tst.l	d0
	bmi	out_of_mem
portament?:				*ポルタメント(オートベンド)
	tst.l	s_frq_b-work(a6)
	beq	truncate?
					*テンポラリエリア確保
	move.l	pcm_temp(pc),a2
	move.l	a2,src_temp1-work(a6)	*交換

	move.l	#-1,-(sp)
	DOS	_MALLOC
	addq.w	#4,sp
	andi.l	#$00ffffff,d0
	move.l	d0,d5
	move.l	d0,-(sp)	*取れる限り確保
	DOS	_MALLOC
	addq.w	#4,sp
	move.l	d0,pcm_temp-work(a6)
	bmi	out_of_mem
	move.l	d0,a1		*a1.l=destination buffer address
	add.l	d0,d5		*d5.l=mem. block end address
				*a2.l=source pcm data address
				*パラメータチェック
	move.l	a2,d0		*push a2
	move.l	b_offset(pc),d1
	add.l	a2,d1		*オフセット値が巨大過ぎないか
	bcs	b_offset_err
	move.l	a4,d4
	sub.l	d1,d4		*オフセットが大きすぎないか
	bcs	b_offset_err
	beq	b_offset_err
	move.l	d0,a2		*pop a2

	move.l	b_offset(pc),d1
	lsr.l	d1
	beq	portament_ope	*オフセットなし
@@:				*元データ転送
	move.w	(a2)+,(a1)+
	cmp.l	a1,d5
	bls	out_of_mem
	subq.l	#1,d1
	bne	@b
portament_ope:
	move.l	b_size(pc),d4
	bne	@f
				*サイズ指定がない場合
	move.l	a4,d4
	sub.l	a2,d4
	bra	do_portament
@@:				*サイズ指定がある場合
	move.l	d4,d0
	add.l	a2,d0
	cmp.l	a4,d0		*サイズが大きすぎないか
	bhi	b_size_err
do_portament:
	lsr.l	d4		*d4.l=data count
	beq	cant_b_err
	move.l	s_frq_b(pc),d6	*d6.l=source frq
	move.l	d_frq_b(pc),d7	*d7.l=destination frq
	bsr	do_autobend	*周波数連続変換	> a1.l=destination end address
	tst.l	b_size-work(a6)
	beq	mfreetmp1_b	*サイズ指定がなかった場合はTAIL処理省略
set_tail_ope_b:			*元データ転送
	move.l	a4,d1
	sub.l	a2,d1
	lsr.l	d1
	beq	mfreetmp1_b
@@:
	move.w	(a2)+,(a1)+
	cmp.l	a1,d5
	bls	out_of_mem
	subq.l	#1,d1
	bne	@b
mfreetmp1_b:
	move.l	a1,a4			*辻褄合わせ
	move.l	pcm_temp(pc),d1
	sub.l	d1,a1

	move.l	src_temp1(pc),-(sp)
	DOS	_MFREE
	addq.w	#4,sp

	pea.l	(a1)			*size
	move.l	d1,-(sp)		*address
	DOS	_SETBLOCK
	addq.w	#8,sp
	tst.l	d0
	bmi	out_of_mem
truncate?:				*切り出し
	move.l	cut_offset(pc),d0
	or.l	cut_size(pc),d0
	beq	rev_ope?

	move.l	pcm_temp(pc),a1
	move.l	a1,a2			*destination
	suba.l	a2,a4
	move.l	cut_offset(pc),d2
	cmp.l	a4,d2
	bhi	cut_offset_illegal
	add.l	d2,a1			*source
	move.l	cut_size(pc),d1
	bne	@f
	move.l	a4,d1
	sub.l	d2,d1
@@:
	cmp.l	a4,d1
	bhi	cut_size_illegal
	lea	(a2,d1.l),a4		*辻褄合わせ
	move.l	d1,-(sp)		*size
@@:
	move.b	(a1)+,(a2)+
	subq.l	#1,d1
	bne	@b
	move.l	pcm_temp(pc),-(sp)	*address
	DOS	_SETBLOCK
	addq.w	#8,sp
	tst.l	d0
	bmi	out_of_mem
rev_ope?:				*逆転再生モード
	tst.b	revout_mode-work(a6)
	beq	dis_ope?

	move.l	pcm_temp(pc),a1
	move.l	a1,src_temp1-work(a6)	*交換
	move.l	a4,d4
	sub.l	a1,d4

	move.l	d4,-(sp)		*size
	DOS	_MALLOC
	addq.w	#4,sp
	move.l	d0,pcm_temp-work(a6)
	bmi	out_of_mem
	move.l	d0,a4
	add.l	d4,a1
	lsr.l	d4
@@:
	move.w	-(a1),(a4)+
	subq.l	#1,d4
	bne	@b

	move.l	src_temp1(pc),-(sp)
	DOS	_MFREE
	addq.w	#4,sp
dis_ope?:				*ディストーションモード
*	tst.b	disout_mode-work(a6)
*	beq	env_chg?
*
*	move.l	a4,d4
*	move.l	pcm_temp(pc),a4
*	sub.l	a4,d4
*	lsr.l	d4
*	move.l	#-512,d1
*	move.l	#512,d2
*dislp:
*	move.w	(a4),d0
*	ext.l	d0
*	asl.l	#5,d0
*	cmp.l	d1,d0
*	bge	@f
*	move.w	d1,(a4)+
*	bra	next_dis
*@@:
*	cmp.l	d2,d0
*	ble	@f
*	move.w	d2,(a4)+
*	bra	next_dis
*@@:
*	move.w	d0,(a4)+
*next_dis:
*	subq.l	#1,d4
*	bne	dislp
env_chg?:
	tst.b	fade_mode-work(a6)
	beq	impulse_compound?

	pea	(a4)
	move.l	pcm_temp(pc),a1
	suba.l	a1,a4		*source data size

	move.l	env_offset(pc),d0
	cmp.l	a4,d0
	bcc	env_offset_illegal
	moveq.l	#0,d5
	tst.b	env_type-work(a6)	*check mode
	bne	@f
				*case:fade in
	move.l	d0,d1		*fade count*2
	move.b	env_lvl(pc),d5		*get in level
	moveq.l	#1,d7
	bra	calc_fio
@@:				*case:fade out
	add.l	d0,a1		*start point
	move.l	a4,d1
	sub.l	d0,d1		*fade count*2
	move.b	#128,d5
	moveq.l	#-1,d7
calc_fio:
	lsr.l	d1		*fade count
	move.l	#128,d0
	sub.b	env_lvl(pc),d0	*get in/end level
	move.l	d1,d3
	bsr	wari		d0.l/d1.l=d0.l...d1.l
	move.w	d0,d2		*step
	move.l	d1,d0
	swap	d0
	clr.w	d0		*d0=あまり*65536
	move.l	d3,d1
	bsr	wari		d0.l/d1.l=d0.l...d1.l
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

	move.l	(sp)+,a4		*辻褄合わせ
impulse_compound?:			*たたみ込み
	tst.b	impulse_mode-work(a6)
	beq	p16out?

	move.l	pcm_temp(pc),a1
	move.l	a1,src_temp1-work(a6)	*交換
	move.l	a4,d4
	sub.l	a1,d4			*d4=input data size

	move.l	impulse_size(pc),d5
	add.l	d4,d5
	lsl.l	d5
	subq.l	#4,d5
	move.l	d5,-(sp)		*size
	DOS	_MALLOC
	addq.w	#4,sp
	move.l	d0,pcm_temp-work(a6)
	bmi	out_of_mem

	move.l	d0,a4
	move.l	d0,a0			*あとで使用
	move.l	d5,d0			*get size again
	lsr.l	#2,d0
@@:
	clr.l	(a4)+			*zero clear
	subq.l	#1,d0
	bne	@b

	lsr.l	d4			*input data count
imp_lp0:
	move.w	(a1)+,d1
	beq	next_a1_
	move.l	impulse_buff(pc),a2	*impulse data
	move.l	impulse_size(pc),d2	*impulse size
	lsr.l	d2
	move.l	a0,a4
imp_lp1:
	move.w	(a2)+,d0
	muls.w	d1,d0
	add.l	d0,(a4)+
	subq.l	#1,d2
	bne	imp_lp1
next_a1_:
	addq.w	#4,a0
	subq.l	#1,d4
	bne	imp_lp0

	move.l	pcm_temp(pc),a1
	move.l	a1,a4
	move.l	a1,d2
	lsr.l	#2,d5
@@:
	move.l	(a1)+,d0
	swap	d0			*/65536
	move.w	d0,(a4)+
	subq.l	#1,d5
	bne	@b
	move.l	a4,d0
	sub.l	d2,d0

	move.l	d0,-(sp)		*size
	move.l	d2,-(sp)		*address
	DOS	_SETBLOCK
	addq.w	#8,sp
	tst.l	d0
	bmi	out_of_mem

	move.l	impulse_buff(pc),-(sp)
	DOS	_MFREE
	addq.w	#4,sp

	move.l	src_temp1(pc),-(sp)
	DOS	_MFREE
	addq.w	#4,sp
p16out?:				*ＰＣＭコンバート?
	tst.b	p16out_mode-work(a6)
	beq	adpout?

	move.l	pcm_temp(pc),a1
	move.l	a1,src_temp1-work(a6)	*交換

	suba.l	a1,a4
	move.l	a4,d1
	lsl.l	#2,d1
	move.l	d1,-(sp)
	DOS	_MALLOC
	addq.w	#4,sp
	move.l	d0,pcm_temp-work(a6)
	bmi	out_of_mem
	move.l	d0,a0			*destination
	move.l	a4,d0			*adpcm data size
	lea	(a0,d1.l),a4		*辻褄合わせ
	exg.l	a0,a1
	bsr	just_adpcm_to_pcm

	move.l	src_temp1(pc),-(sp)	*ADPCMデータの解放
	DOS	_MFREE
	addq.w	#4,sp
	bra	generate
adpout?:
	tst.b	adpout_mode-work(a6)
	beq	generate

	move.l	pcm_temp(pc),a1
	move.l	a1,src_temp1-work(a6)	*交換

	suba.l	a1,a4
	move.l	a4,d1
	lsr.l	#2,d1
	move.l	d1,-(sp)
	DOS	_MALLOC
	addq.w	#4,sp
	move.l	d0,pcm_temp-work(a6)
	bmi	out_of_mem
	move.l	d0,a0			*destination
	move.l	a4,d0			*pcm data size
	lea	(a0,d1.l),a4		*辻褄合わせ
	bsr	pcm_to_adpcm

	move.l	src_temp1(pc),-(sp)	*PCMデータの解放
	DOS	_MFREE
	addq.w	#4,sp
generate:
	lea	d_name(pc),a0
	move.l	(a0),d0
	andi.l	#$dfdf_dfff,d0
	cmpi.l	#$50434d00,d0
	bne	@f
	move.w	#%0_000_01,-(sp)
	pea	(a0)
	DOS	_OPEN
	addq.w	#6,sp
	tst.l	d0
	bpl	copy_fh
@@:
	move.w	#32,-(sp)
	pea	(a0)
	DOS	_CREATE
	addq.w	#6,sp
	tst.l	d0
	bmi	wopen_error
copy_fh:
	move.w	d0,d5

	move.l	pcm_temp(pc),d1
	suba.l	d1,a4
	pea	(a4)			*size
	move.l	d1,-(sp)		*address
	move.w	d5,-(sp)		*filehandle
	DOS	_WRITE
	lea	10(sp),sp
	tst.l	d0
	bmi	write_error
	cmp.l	a4,d0			*完全にセーブできたか
	bne	device_full

	move.w	d5,-(sp)
	DOS	_CLOSE
	addq.w	#2,sp

	clr.l	-(sp)
	DOS	_MFREE
	addq.w	#4,sp

	print	no_er_mes(pc)

	DOS	_EXIT

do_ajfr:
	* < d4.l=data count
	* < d5.l=mem.block end address
	* < d6.w=source frq
	* < d7.w=destination frq
	* < a1.l=destination pcm data address
	* < a2.l=source pcm data address
	* > a1.l=destination pcm data size
	* X d0-d7
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
	bls	out_of_mem
@@:
	subq.l	#1,d4
	bne	doa_lp00
	bra	exit_doa
nz_d1:
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
doa_lp01:
	move.w	d0,(a1)+
	cmp.l	a1,d5
	bls	out_of_mem
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
	movem.l	(sp)+,d1/d3
	subq.l	#1,d4
	bne	nz_d1
exit_doa:
*	move.l	a1,d1
	rts

do_autobend:
	* < d4.l=data count
	* < d5.l=mem.block end address
	* < d6.w=source frq
	* < d7.w=destination frq
	* < a1.l=destination pcm data address
	* < a2.l=source pcm data address
	* > a1.l=destination pcm data size
	* X d0-d7
	exg.l	d6,d7
	move.l	d6,atb_frqsrc-work(a6)
	move.l	d6,atb_frqnow-work(a6)
				*周波数変化率計算
	move.l	d4,d1
	move.l	d7,d0
	sub.l	d6,d0
	bpl	@f
	neg.l	d0
	bsr	wari		*d0.l/d1.l=d0.l...d1.l
	neg.l	d0
	move.l	d0,atb_step-work(a6)		*周波数変化率
	move.l	#-1,atb_sgn-work(a6)
	bra	atb0
@@:
	bsr	wari		*d0.l/d1.l=d0.l...d1.l
	move.l	d0,atb_step-work(a6)		*周波数変化率
	move.l	#1,atb_sgn-work(a6)
atb0:
	swap	d1
	clr.w	d1
	move.l	d1,d0
	move.l	d4,d1
	bsr	wari		*d0.l/d1.l=d0.l...d1.l
	tst.l	d1
	beq	@f
	addq.l	#1,d0		*revise
@@:
	move.w	d0,atb_rvs-work(a6)

	bsr	calc_frqchgrate

	moveq.l	#0,d3
	move.w	d3,atb_rvswk-work(a6)

	tst.b	atb_sgn-work(a6)
	bpl	nz_d1_b
doa_lp00_b:
	move.w	(a2)+,d0
	add.w	d7,d3
	bcc	@f
	move.w	d0,(a1)+
	cmp.l	a1,d5
	bls	out_of_mem
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
	bls	out_of_mem
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
	movem.l	(sp)+,d1/d3
	bsr	calc_frqchgrate
	subq.l	#1,d4
	bne	nz_d1_b
exit_doa_b:
*	move.l	a1,d1
	rts

calc_frqchgrate:			*変換パラメータの計算
	movem.l	atb_frqsrc(pc),d6-d7
	add.l	atb_step(pc),d7
	move.w	atb_rvs(pc),d1
	add.w	d1,atb_rvswk-work(a6)
	bcc	@f
	add.l	atb_sgn(pc),d7
@@:
	move.l	d7,atb_frqnow-work(a6)

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

do_cnv_fwd:
	* < d4.l=source edge address
	* < a1.l=source pcm data
	* < a3.l=destination buffer end address
	* < a4.l=destination buffer
	* X a4
	tst.w	conv_type(a5)
	beq	@f
	bpl	do_cnv_fwd_
@@:
	movem.l	d0-d1/a1,-(sp)
	move.w	vol_val(pc),d1
@@:
	move.w	(a1)+,d0
	muls	d1,d0		*音量変換
	asr.l	#8,d0
	move.w	d0,(a4)+
	cmp.l	a3,a4
	bcc	out_of_mem
	cmp.l	a1,d4
	bhi	@b
	movem.l	(sp)+,d0-d1/a1
	rts
do_cnv_fwd_:
	movem.l	d0/a1,-(sp)
@@:
	move.w	(a1)+,(a4)+
	cmp.l	a3,a4
	bcc	out_of_mem
	cmp.l	a1,d4
	bhi	@b
	movem.l	(sp)+,d0/a1
	rts

do_cnv_rev:
	* < d4.l=source edge address
	* < a1.l=source pcm data
	* < a3.l=destination buffer end address
	* < a4.l=destination buffer
	* X a4
	tst.w	conv_type(a5)
	beq	@f
	bpl	do_cnv_rev_
@@:
	movem.l	d0-d1/a1,-(sp)
	move.w	vol_val(pc),d1		*音量変換
@@:
	move.w	-(a1),d0
	muls	d1,d0		*音量変換
	asr.l	#8,d0
	move.w	d0,(a4)+
	cmp.l	a3,a4
	bcc	out_of_mem
	cmp.l	d4,a1
	bhi	@b
	movem.l	(sp)+,d0-d1/a1
	rts
do_cnv_rev_:
	movem.l	d0/a1,-(sp)
@@:
	move.w	-(a1),(a4)+
	cmp.l	a3,a4
	bcc	out_of_mem
	cmp.l	d4,a1
	bhi	@b
	movem.l	(sp)+,d0/a1
	rts

just_adpcm_to_pcm:		*ピッチチェンジやレベルチェンジを
				*行わない単なるADPCM→PCM変換
	* < a0=adpcm data buffer
	* < a1=pcm data buffer
	* < d0.l=adpcm data size
	movem.l	d0-d7/a0-a6,-(sp)
	lea	scaleval(pc),a5
	lea	levelchg(pc),a6
	moveq.l	#0,d3
	moveq.l	#0,d7
	moveq.l	#$0f,d4
	add.l	d0,d0
	lea	last_val(pc),a3
	move.w	d3,(a3)
__atp_lp:
	move.b	(a0),d1
	and.w	d4,d1
	tst.b	d4
	bpl	@f
	lsr.b	#4,d1		*get 4bit data
	addq.w	#1,a0
@@:
	not.b	d4
	bsr	calc_pcm_val	*実際の計算
	move.w	d2,(a1)+	*add pcm data to buffer
	subq.l	#1,d0
	bne	__atp_lp
	movem.l	(sp)+,d0-d7/a0-a6
	rts

adpcm_to_pcm:			*レベルチェンジを
				*行うADPCM→PCM変換
	* < a0=adpcm data buffer
	* < a1=pcm data buffer
	* < d0.l=adpcm data size
	* < d6.w=volume value 0～256
	movem.l	d0-d7/a0-a6,-(sp)
	lea	scaleval(pc),a5
	lea	levelchg(pc),a6
	moveq.l	#0,d3
	moveq.l	#0,d7
	moveq.l	#$0f,d4
	add.l	d0,d0
	lea	last_val(pc),a3
	move.w	d3,(a3)
atp_lp:
	move.b	(a0),d1
	and.w	d4,d1
	tst.b	d4
	bpl	@f
	lsr.b	#4,d1		*get 4bit data
	addq.w	#1,a0
@@:
	not.b	d4
	bsr	calc_pcm_val	*実際の計算
	muls	d6,d2
	asr.l	#8,d2
	move.w	d2,(a1)+	*add pcm data to buffer
	subq.l	#1,d0
	bne	atp_lp
	movem.l	(sp)+,d0-d7/a0-a6
	rts

pcm_to_adpcm:			*ＰＣＭデータをＡＤＰＣＭデータへ変換する
	* < a0=adpcm data buffer
	* < a1=pcm data buffer
	* < d0.l=pcm data size
	* - all

	movem.l	d0-d7/a0-a6,-(sp)

	lea	scaleval(pc),a5
	lea	levelchg(pc),a6

	moveq.l	#0,d6		*scalelevel=0
	moveq.l	#0,d7
	moveq.l	#0,d4
	lsr.l	d0
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
	movem.l	(sp)+,d0-d7/a0-a6
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

read_data:			*ディスクからの読み込み
	* < (a1)=file name
	movem.l	d0/d5/a0-a2,-(sp)

	move.l	a1,a2
	bsr	fopen
	tst.l	d5		*d5=file handle
	bmi	ropen_error

	bsr	get_fsize
	move.l	d0,d1		*d1.l=size
	bmi	size_error	*illegal file size
	beq	size_error	*illegal file size

	move.l	d1,-(sp)	*size
	move.l	src_temp1(pc),d2
	move.l	d2,-(sp)	*address
	move.w	d5,-(sp)	*file handle
	DOS	_READ
	lea	10(sp),sp
	tst.l	d0
	bmi	read_error

	move.w	d5,-(sp)
	DOS	_CLOSE
	addq.w	#2,sp

	movem.l	(sp)+,d0/d5/a0-a2
	rts

read_data2:			*インパルスデータのディスクからの読み込み
	* < (a1)=file name
	movem.l	d0/d5/a0-a2,-(sp)

	move.l	a1,a2
	bsr	fopen
	tst.l	d5		*d5=file handle
	bmi	ropen_error_

	bsr	get_fsize
	move.l	d0,d1		*d1.l=size
	bmi	size_error	*illegal file size
	beq	size_error	*illegal file size

	move.l	d1,-(sp)
	DOS	_MALLOC
	addq.l	#4,sp
	move.l	d0,impulse_buff-work(a6)
	bmi	out_of_mem

	move.l	d1,-(sp)		*size
	move.l	impulse_buff(pc),-(sp)	*address
	move.w	d5,-(sp)		*file handle
	DOS	_READ
	lea	10(sp),sp
	move.l	d0,impulse_size-work(a6)
	bmi	read_error

	move.w	d5,-(sp)
	DOS	_CLOSE
	addq.w	#2,sp

	movem.l	(sp)+,d0/d5/a0-a2
	rts

check_fopen:			*ファイルが存在するかのチェック
	* < (a1)=file name
	* > d1.l=size
	movem.l	d0/d5/a0-a2,-(sp)

	move.l	a1,a2
	bsr	fopen
	tst.l	d5		*d5=file handle
	bmi	ropen_error

	bsr	get_fsize
	move.l	d0,d1		*d1.l=size
	bmi	size_error	*illegal file size
	beq	size_error	*illegal file size

	move.w	d5,-(sp)
	DOS	_CLOSE
	addq.w	#2,sp

	movem.l	(sp)+,d0/d5/a0-a2
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

skip_spc:			*スペースを読み飛ばす(case:command line)
	cmpi.b	#' ',(a2)+
	beq	skip_spc
	subq.w	#1,a2
	rts

skip_sep:			*セパレータをスキップする(スペースやタブも)
	move.l	d0,-(sp)
	bsr	skip_spc
@@:
	move.b	(a2)+,d0
	cmpi.b	#',',d0
	beq	@f
	subq.w	#1,a2
@@:
	move.l	(sp)+,d0
	rts

mk_capital:			*小文字→大文字(英字以外の場合はそのままthrough out)
	* < d0.b=letter chr
	cmpi.b	#'a',d0
	bcs	@f
	cmpi.b	#'z',d0
	bhi	@f
	andi.w	#$df,d0		*わざと.w
@@:
	rts

chk_num:			*数字かどうかチェック(for command line)
	* > eq=number
	* > mi=not num
	bsr	skip_spc
	cmpi.b	#'0',(a2)
	bcs	not_num
	cmpi.b	#'9',(a2)
	bhi	not_num
	move.w	#%0000_0100,ccr	*eq
	rts
not_num:
	move.w	#%0000_1000,ccr	*mi
	rts

asc_to_n:			*数字文字列を数値へ
	* < (a2)=number strings
	* > d1.l=value
	* > a2=next
	* x none
	movem.l	d0/d2-d3,-(sp)
	moveq.l	#0,d1
	moveq.l	#0,d0
	cmpi.b	#'+',(a2)
	bne	@f
	addq.w	#1,a2
@@:
num_lp01:
	move.b	(a2),d0
	sub.b	#$30,d0
	bmi	num_exit
	cmp.b	#9,d0
	bhi	num_exit
	addq.w	#1,a2
	add.l	d1,d1
	move.l	d1,d3
	lsl.l	#2,d1
	add.l	d3,d1		*d1=d1*10
	add.l	d0,d1		*d1=d1+d0
	bra	num_lp01
num_exit:
	movem.l	(sp)+,d0/d2-d3
	rts

wari:				*32ﾋﾞｯﾄ/32ﾋﾞｯﾄ=32ﾋﾞｯﾄ...32ﾋﾞｯﾄ
	* < d0.l/d1.l=d0.l ...d1.l
	cmpi.l	#$ffff,d1
	bls	divx		*16ビット以下の数値なら１命令で処理
	cmp.l	d0,d1
	beq	div01		*d0=d1商は１
	bls	div02		*１命令では無理なケース

	move.l	d0,d1		*商は０余りはd0.l
	moveq.l	#0,d0
	rts
div01:				*商は１余り０
	moveq.l	#1,d0
	moveq.l	#0,d1
	rts
div02:
	movem.l	d3-d5,-(sp)
	move.l	d1,d3
	clr.w	d3
	swap	d3
	addq.l	#1,d3
	move.l	d0,d4
	move.l	d1,d5
	move.l	d3,d1
	bsr	divx
	move.l	d5,d1
	divu	d3,d1
	divu	d1,d0
	andi.l	#$ffff,d0
div03:
	move.l	d5,d1
	move.l	d5,d3
	swap	d3
	mulu	d0,d1
	mulu	d0,d3
	swap	d3
	add.l	d3,d1
	sub.l	d4,d1
	bhi	div04
	neg.l	d1
	cmp.l	d1,d5
	bhi	div05
	addq.l	#1,d0
	bra	div03
div04:
	subq.l	#1,d0
	bra	div03
div05:
	movem.l	(sp)+,d3-d5
	rts
divx:
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

fnstr_chk:			*その文字がファイルネームとして使えるか
	* < d0.b=data
	* > eq=can't use
	* > ne=able to use
	* - all
	tst.b	d0
	bmi	@f
	movem.l	d0-d1,-(sp)
	move.l	d0,d1
	lsr.b	#3,d1
	ext.w	d1
	andi.b	#7,d0
	btst.b	d0,fnstr_tbl(pc,d1.w)
	movem.l	(sp)+,d0-d1
@@:
	rts

fnstr_tbl:	dc.b	%00000000,%00000000	*00～0f
		dc.b	%00000000,%00000000	*10～1f
		dc.b	%01111010,%01000011	*20～2f
		dc.b	%11111111,%00000111	*30～3f
		dc.b	%11111111,%11111111	*40～4f
		dc.b	%11111111,%11010111	*50～5f
		dc.b	%11111111,%11111111	*60～6f
		dc.b	%11111111,%11101111	*70～7f

error_exit:
	clr.w	-(sp)
	DOS	_KFLUSH
	addq.w	#2,sp

	move.w	#1,-(sp)
	DOS	_EXIT2

close_kill:			*書き込もうとしたファイルを消す
	DOS	_ALLCLOSE
	pea	d_name(pc)
	DOS	_DELETE
	addq.w	#4,sp
	rts

print_hlp:
	print2	help(pc)
	print	more(pc)
	DOS	_INKEY
	move.l	#$000d_0002,-(sp)
	DOS	_FPUTC
	addq.w	#4,sp
	print2	help2(pc)
	bra	error_exit
out_of_mem:
	bsr	close_kill
	print	out_of_mem_mes(pc)
	bra	error_exit
write_error:
	bsr	close_kill
	print	wrt_er_mes(pc)
	bra	error_exit
device_full:
	bsr	close_kill
	print	devful_mes(pc)
	bra	error_exit
l_illegal:
	print	l_illegal_mes(pc)
	bra	error_exit
r_illegal:
	print	r_illegal_mes(pc)
	bra	error_exit
v_illegal:
	print	v_illegal_mes(pc)
	bra	error_exit
too_many_s:
	print	too_many_s_mes(pc)
	bra	error_exit
no_v_param:
	print	no_v_param_mes(pc)
	bra	error_exit
ropen_error:
	print	open_er_mes(pc)
	print	fname(a5)
	print	close_blnkt(pc)
	bra	error_exit
ropen_error_:
	print	open_er_mes(pc)
	print	(a1)
	print	close_blnkt(pc)
	bra	error_exit
wopen_error:
	print	open_er_mes(pc)
	print	d_name(pc)
	print	close_blnkt(pc)
	bra	error_exit
size_error:
	print	size_er_mes(pc)
	bra	error_exit
read_error:
	print	read_er_mes(pc)
	bra	error_exit
no_s_error:
	print	no_s_mes(pc)
	bra	error_exit
no_d_error:
	print	no_d_mes(pc)
	bra	error_exit
s_illegal:
	print	s_illegal_mes(pc)
	bra	error_exit
d_illegal:
	print	d_illegal_mes(pc)
	bra	error_exit
unex_error:
	print	unex_er_mes(pc)
	bra	error_exit
syntax_error:
	print	syntax_er_mes(pc)
	bra	error_exit
not_support:
	print	not_sup_er_mes(pc)
	bra	error_exit
cut_size_illegal:
	print	cutsize_er_mes(pc)
	bra	error_exit
cut_offset_illegal:
	print	cutoff_er_mes(pc)
	bra	error_exit
env_offset_illegal:
	print	envoff_er_mes(pc)
	bra	error_exit
envlvl_illegal:
	print	envlvl_er_mes(pc)
	bra	error_exit
envtype_illegal:
	print	envtype_er_mes(pc)
	bra	error_exit
t_offset_err:
	print	trnoff_er_mes(pc)
	bra	error_exit
t_size_err:
	print	trnsize_er_mes(pc)
	bra	error_exit
cant_t_err:
	print	cant_t_er_mes(pc)
	bra	error_exit
b_offset_err:
	print	bndoff_er_mes(pc)
	bra	error_exit
b_size_err:
	print	bndsize_er_mes(pc)
	bra	error_exit
cant_b_err:
	print	cant_b_er_mes(pc)
	bra	error_exit
illegal_frq:
	print	illegal_fr_mes(pc)
	bra	error_exit
unid_err:
	print	unid_mes(pc)
	bra	error_exit

	.data
work:
title:		dc.b	$1b,'[36mΖ',$1b,'[35mplk.R '
		dc.b	$1b,'[37m',$f3,'V',$F3,'E',$F3,'R',$F3,'S',$F3,'I',$F3,'O',$F3,'N'
		dc.b	$f3,' ',$f3,'1',$f3,'.',$f3,'0',$f3,'3'
		dc.b	$1b,'[m (C) 1993,1994 '
		dc.b	$1b,'[36mZENJI SOFT',$1b,'[m',13,10,0
help:		dc.b	'FUNCTION:LINK PCM FILES.',13,10
		dc.b	'   USAGE:ZPLK.R <[OPTIONS] SOURCE1> [[OPTIONS] SOURCE2～32] <DESTINATION>',13,10
		dc.b	' OPTIONS:-A            EXECUTE ADPCM CONVERSION BEFORE GENERATE THE DESTINATION.',13,10
		dc.b	'         -B<s,d>[p,s]  BEND THE PITCH FROM s(1～65535)[Hz] TO d(1～65535)[Hz].',13,10
		dc.b	'           p           PITCH-BEND START POINT(DEFAULT=0)',13,10
		dc.b	'           s           PITCH-BEND SIZE',13,10
		dc.b	'         -C[p]<,s>     TRUNCATE THE DESTINATION.',13,10
		dc.b	'           p           TRUNCATE START POINT(DEFAULT=0)',13,10
		dc.b	'           s           TRUNCATE SIZE',13,10
		dc.b	'         -F[s,l,m]     TRANSFORM THE ENVELOPE OF THE DESTINATION.',13,10
		dc.b	'           s           START POINT(DEFAULT=0)',13,10
		dc.b	'           l           IN/END LEVEL(0～127, DEFAULT=0)',13,10
		dc.b	'           m           TRANSFORM MODE(DEFAULT=1)',13,10
		dc.b	'                       0 FADEIN',13,10
		dc.b	'                       1 FADEOUT',13,10
		dc.b	'         -I<filename>  CONVOLUTE THE DESTINATION WITH IMPULSE DATA.',13,10
		dc.b	'           filename    FILENAME OF THE IMPULSE DATA',13,10
		dc.b	0
help2:
		dc.b	'         -P            EXECUTE 16bit PCM CONVERSION BEFORE GENERATE THE DESTINATION.',13,10
		dc.b	'         -R            GENERATE THE DESTINATION IN REVERSIVE SEQUENCE.',13,10
		dc.b	'         -V<n>         SET n(1～300)[%] FOR AN OUTPUT LEVEL.(DEFAULT=100)',13,10
		dc.b	'         -T<i,o>[,p,s] TRANSFORM THE FREQUENCY FROM i(1～65535)[Hz] TO o(1～65535)[Hz].',13,10
		dc.b	'           p           TRANSFORMATION START POINT(DEFAULT=0)',13,10
		dc.b	'           s           TRANSFORMATION SIZE',13,10
		dc.b	'         -X[l,r,t]     DEFINE SUB-CONVERSION PARAMETER',13,10
		dc.b	'           l           LOOP TYPE(DEFAULT=0)',13,10
		dc.b	'                       0 FORWARD',13,10
		dc.b	'                       1 ALTERNATIVE #1(←→)',13,10
		dc.b	'                       2 ALTERNATIVE #2(→←)',13,10
		dc.b	'                       3 REVERSIVE',13,10
		dc.b	'           r           REPEAT r(1～65535) TIMES BY TYPE l SCAN.(DEFAULT=1)',13,10
		dc.b	'           t           TYPE OF THE SOURCE PRIOR CONVERSION(DEFAULT=0)',13,10
		dc.b	'                       0 NO CONVERSION',13,10
		dc.b	'                       1 REGARD THE GIVEN SOURCE AS 16bit PCM DATA,',13,10
		dc.b	'                         CONVERT IT TO ADPCM BEFORE LINK OPERATION.',13,10
		dc.b	'                       2 REGARD THE GIVEN SOURCE AS ADPCM DATA,',13,10
		dc.b	'                         CONVERT IT TO 16bit PCM DATA BEFORE LINK OPERATION.',13,10
		dc.b	0
more:		dc.b	'-more-',0
no_er_mes:	dc.b	$1b,'[mOperations are all set.',13,10
		dc.b	'A ',$1b,'[37m','♪SOUND',$1b,'[m mind in a '
		dc.b	$1b,'[37mSOUND',$1b,'[m',' body.',13,10,0
out_of_mem_mes:	dc.b	'OUT OF MEMORY.',13,10,0
wrt_er_mes:	dc.b	'WRITE ERROR.',13,10,0
devful_mes:	dc.b	'DEVICE FULL.',13,10,0
l_illegal_mes:	dc.b	'ILLEGAL LOOP TYPE PARAMETER.',13,10,0
r_illegal_mes:	dc.b	'ILLEGAL REPEAT TIME PARAMETER.',13,10,0
v_illegal_mes:	dc.b	'ILLEGAL VOLUME PARAMETER.',13,10,0
too_many_s_mes:	dc.b	'TOO MANY SOURCE FILES.',13,10,0
no_v_param_mes:	dc.b	'NO VOLUME PARAMETER.',13,10,0
open_er_mes:	dc.b	'FILE OPEN ERROR. "',0
close_blnkt:	dc.b	'"',13,10,0
read_er_mes:	dc.b	'READ ERROR.',13,10,0
size_er_mes:	dc.b	'ILLEGAL FILE SIZE',13,10,0
no_s_mes:	dc.b	'NO SOURCE FREQUENCY PARAMETER.',13,10,0
no_d_mes:	dc.b	'NO DESTINATION FREQUENCY PARAMETER.',13,10,0
s_illegal_mes:	dc.b	'ILLEGAL SOURCE FREQUENCY PARAMETER.',13,10,0
d_illegal_mes:	dc.b	'ILLEGAL DESTINATION FREQUENCY PARAMETER.',13,10,0
unex_er_mes:	dc.b	'UNEXPECTED PARAMETER HAS ESTABLISHED.',13,10,0
syntax_er_mes:	dc.b	'SYNTAX ERROR.',13,10,0
not_sup_er_mes:	dc.b	"ZPLK.R DOESN'T SUPPORT THE COMBINATION OF THESE PARAMETERS.",13,10,0
cutsize_er_mes:	dc.b	'ILLEGAL TRUNCATE OFFSET VALUE.',13,10,0
cutoff_er_mes:	dc.b	'ILLEGAL TRUNCATE SIZE.',13,10,0
envoff_er_mes:	dc.b	'ILLEGAL ENVELOPE-REFORM OFFSET VALUE.',13,10,0
envlvl_er_mes:	dc.b	'ILLEGAL ENVELOPE-REFORM IN/OUT LEVEL.',13,10,0
envtype_er_mes:	dc.b	'ILLEGAL ENVELOPE-REFORM TYPE',13,10,0
trnoff_er_mes:	dc.b	'ILLEGAL FREQUENCY TRANSFORMATION OFFSET VALUE.',13,10,0
trnsize_er_mes:	dc.b	'ILLEGAL FREQUENCY TRANSFORMATION SIZE.',13,10,0
cant_t_er_mes:	dc.b	'FREQUENCY TRANSFORMATION UNSUCCESSFUL.',13,10,0
bndoff_er_mes:	dc.b	'ILLEGAL PITCH-BEND OFFSET VALUE.',13,10,0
bndsize_er_mes:	dc.b	'ILLEGAL PITCH-BEND SIZE.',13,10,0
cant_b_er_mes:	dc.b	'PITCH-BEND PROCESSING UNSUCCESSFUL.',13,10,0
illegal_fr_mes:	dc.b	'FREQUENCY PARAMETER OUT OF RANGE.',13,10,0
unid_mes:	dc.b	'UNIDENTIFIED FILE.',13,10,0

getname:	dc.b	'zmusic',0,0
p16out_mode:	dc.b	0
adpout_mode:	dc.b	0
revout_mode:	dc.b	0
env_lvl:	dc.b	0	*エンベロープ変更の音量パラメータ
env_type:	dc.b	1	*エンベロープ変更の種類
fade_mode:	dc.b	0
impulse_mode:	dc.b	0
	.even
vol_val:	dc.w	256
s_frq:		dc.l	0	*source frq
d_frq:		dc.l	0	*destination frq
env_offset:	dc.l	0	*エンベロープ変更の開始ポイント
cut_offset:	dc.l	0	*切り出しオフセット
cut_size:	dc.l	0	*切り出しサイズ
t_offset:	dc.l	0	*ピッチ変更オフセット値
t_size:		dc.l	0	*ピッチ変更サイズ
s_frq_b:	dc.l	0	*pitch bend source frq
d_frq_b:	dc.l	0	*pitch bend destination frq
b_offset:	dc.l	0	*ピッチベンド・オフセット値
b_size:		dc.l	0	*ピッチベンド・サイズ

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

	.bss	
	.even
s_name_tbl:	rept	smax+1
		ds.b	tbl_size
		endm
pcm_temp:	ds.l	1	*PCM BUFFER
src_temp1:	ds.l	1	*SOURCE PCM BUFFER
src_temp2:	ds.l	1	*SUB CONVERTED PCM BUFFER
last_val:	ds.w	1	*ADPCM変換ワーク
atb_step:	ds.l	1	*autobend work
atb_rvs:	ds.w	1	*autobend work
atb_rvswk:	ds.w	1	*autobend work
atb_frqsrc:	ds.l	1	*autobend work
atb_frqnow:	ds.l	1	*autobend work
atb_sgn:	ds.l	1	*autobend work
env_bak:	ds.l	1
impulse_buff:	ds.l	1
impulse_size:	ds.l	1
	.even
d_name:		ds.b	91
open_fn:	ds.b	91
impulse_name:	ds.b	91
	.even
		ds.l	1024
user_sp:
