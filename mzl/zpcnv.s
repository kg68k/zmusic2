*-------------------------------------------------------
*	ZMUSIC/OPMD用のADPCMデータCONFIGファイルを
*		ひとまとめにするコンバーター
*
*		      ＺＰＣＮＶ.Ｒ
*
*		PROGRAMMED  BY  Z.NISHIKAWA
*
*-------------------------------------------------------
	.include	doscall.mac
	.include	iocscall.mac

chk_mj	macro	moji		*マクロ
	moveq.l	moji,d1
	bsr	chk_mj
	endm

adpcm_n_max:	equ	128*4

cache_flush_ng	.macro
	movem.l	d0-d1,-(sp)
	moveq.l	#1,d0
	.cpu	68020
	move.b	@skip(pc,d0.w*2),d1
	.cpu	68000
	beq	@skip
	moveq	#3,d1
	IOCS	_SYS_STAT
	bra	@skip
@unknown:
	moveq	#0,d1
	moveq	#-1,d0
@skip:
	movem.l	(sp)+,d0-d1
.endm

*-------- program start --------
	lea	$10(a0),a0	*メモリブロックの変更
	lea.l	user_sp(pc),a1
	suba.l	a0,a1
	pea	(a1)
	pea	(a0)
	DOS	_SETBLOCK
	addq.w	#8,sp
	tst.l	d0
	bmi	mem_error

	lea	work(pc),a6
	lea.l	user_sp(pc),sp	*スタック設定
	move.l	a3,env_bak-work(a6)

	tst.b	(a2)+		*パラメータある?
	beq	print_hlp

	bsr	skip_spc_	*最初のファイルネームまでスキップ

	move.b	(a2),d0
	beq	print_hlp
	cmpi.b	#'/',d0
	beq	@f
	cmpi.b	#'-',d0
	bne	get_src_fn
@@:
	addq.w	#1,a2
	bsr	skip_spc_
	move.b	(a2)+,d0
	beq	print_hlp
	bsr	mk_capital
	cmpi.b	#'D',d0
	bne	print_hlp
	bsr	skip_spc_	*最初のファイルネームまでスキップ
	st.b	display_cmt-work(a6)
get_src_fn:
	lea	s_name(pc),a3
gets_lp:			*ソースファイルネームのゲット
	move.b	(a2)+,d0
	cmpi.b	#' ',d0
	bls	@f
	move.b	d0,(a3)+
	bra	gets_lp
@@:
	subq.w	#1,a2
	clr.b	(a3)		*end code
				*拡張子'CNF'セット
	lea	s_name(pc),a3
	clr.b	d1
	cmpi.w	#'..',(a3)
	bne	@f
	addq.w	#2,a3
@@:
	move.l	a3,d3
setfnlp:
	move.b	(a3)+,d2
	bpl	@f
	cmpi.b	#$a0,d2
	bcs	knjsetfn
	cmpi.b	#$df,d2
	bls	@f
knjsetfn:
	addq.w	#1,a3
	bra	setfnlp
@@:
	cmpi.b	#'.',d2
	bne	@f
	st.b	d1
	bra	setfnlp
@@:
	cmpi.b	#' ',d2
	bhi	setfnlp
	subq.w	#1,a3
	cmpi.b	#'.',-1(a3)	*ラストの文字が.だけか
	beq	@f
	tst.b	d1		*拡張子省略かどうか
	bne	set_edc_	*省略しなかった
	bra	set_kkc
@@:
	subq.w	#1,a3
set_kkc:
	move.b	#'.',(a3)+	*拡張子をセット
	move.b	#'C',(a3)+
	move.b	#'N',(a3)+
	move.b	#'F',(a3)+
	clr.b	(a3)
set_edc_:
	*(a3)=ソースネームの終端コード
	bsr	skip_spc_
	tst.b	(a2)		*終端コード？
	bne	get_dest_name
mk_default_nm:			*デフォルトのファイルネームを作る
	* < d3.l=始端アドレス
	* X d0/a0,a3
	move.l	d3,a3
	move.l	d3,a0
mdnlp:
	move.b	(a3)+,d0
	beq	do_mdn
	bpl	@f
	cmpi.b	#$a0,d0
	bcs	knj_mdn
	cmpi.b	#$df,d0
	bls	@f
knj_mdn:
	addq.w	#1,a3
	bra	mdnlp
@@:
	cmpi.b	#'\',d0
	beq	@f
	cmpi.b	#':',d0
	bne	mdnlp
@@:
	move.l	a3,a0
	bra	mdnlp
do_mdn:
	move.l	a0,a3
	lea	d_name(pc),a0
mdf_lp:
	move.b	(a3)+,d0
	bpl	@f
	cmpi.b	#$a0,d0
	bcs	knjmdf
	cmpi.b	#$df,d0
	bls	@f
knjmdf:
	move.b	d0,(a0)+
	move.b	(a3)+,(a0)+
	bra	mdf_lp
@@:
	cmpi.b	#' ',d0
	bls	exit_mdf
	cmpi.b	#'.',d0
	beq	exit_mdf
	move.b	d0,(a0)+
	bra	mdf_lp
exit_mdf:
	move.b	#'.',(a0)+
	move.b	#'Z',(a0)+
	move.b	#'P',(a0)+
	move.b	#'D',(a0)+
	clr.b	(a0)+
	bra	do_cnv_ope

get_dest_name:			*セーブファイルネームのゲット
	lea	d_name(pc),a0
	moveq.l	#0,d1
getd_lp:
	move.b	(a2)+,d0
	bpl	@f
	cmpi.b	#$a0,d0
	bcs	knjgetd
	cmpi.b	#$df,d0
	bls	@f
knjgetd:
	move.b	d0,(a0)+
	move.b	(a2)+,(a0)+
	bra	getd_lp
@@:
	cmpi.b	#' ',d0
	bls	exit_mdf
	cmpi.b	#'.',d0
	beq	exit_mdf
	move.b	d0,(a0)+
	bra	getd_lp
do_cnv_ope:
	bsr	print_title

	lea	s_name(pc),a2
	bsr	fopen
	tst.l	d5
	bmi	ropen_error	*ソースファイル読み込めず

	move.w	#32,-(sp)
	pea	d_name(pc)
	DOS	_CREATE
	addq.w	#6,sp
	move.w	d0,sfh-work(a6)	*save file handle
	bmi	wopen_error	*セーブファイルネームに異常有り

	bsr	read		*d3=size/a5=address
	move.l	d3,d4
	move.l	a5,a4

	move.l	#$40000,-(sp)		*ADPCM BUFFERを256kbデフォルトで確保
	DOS	_MALLOC
	addq.w	#4,sp
	move.l	d0,adpcm_buffer_top-work(a6)
	bmi	mem_error

	move.l	d0,a0
	move.l	#$105a6d41,(a0)+	*HEADER
	move.l	#$6470436d,(a0)

	move.l	#adpcm_n_max*10+8+4,d1	*+8=header size,+4=end code
	add.l	d1,d0		*一応テーブルの分を確保
	move.l	d0,adpcm_buffer_next-work(a6)
	move.l	d1,total_size-work(a6)

					*ワーク初期化
	lea	adpcm_ptr(pc),a2
	move.l	a2,a1
	clr.w	(a2)+
	moveq.l	#5,d1
	move.l	#adpcm_n_max*8-2,d2
	bsr	trans_dma

	move.l	#1,line_number-work(a6)
main_loop:
	bsr	skip_spc
	tst.l	d4
	beq	all_end
	move.b	(a4),d0
	cmpi.b	#$1a,d0
	beq	all_end
	cmpi.b	#' ',d0
	bhi	@f
	subq.l	#1,d4			*' '以下
	addq.w	#1,a4
	cmpi.b	#$0a,d0
	bne	main_loop
	addq.l	#1,line_number-work(a6)
	bra	main_loop
@@:
	cmpi.b	#'#',d0
	beq	goto_ers
	cmpi.b	#'.',d0
	bne	@f
goto_ers:
	subq.l	#1,d4
	addq.w	#1,a4			*skip '.' or '#'
	move.b	(a4),d0
	andi.b	#$df,d0
	cmpi.b	#'O',d0
	beq	case_onkai
	cmpi.b	#'E',d0
	beq	erase
	cmpi.b	#'A',d0
	beq	bank_sel
case_onkai:
	subq.l	#1,d4
	bmi	syntax_error
	addq.w	#1,a4			*skip 'O'
	bra	get_ntn_		*音階指定のケース
@@:
	cmpi.b	#'*',d0
	beq	remlp
	cmpi.b	#'/',d0
	bne	else
remlp:					*注釈のスキップ
	subq.l	#1,d4
	bmi	all_end
	moveq.l	#0,d1
	move.b	(a4)+,d1
	tst.b	display_cmt-work(a6)
	beq	end_rem?
	move.w	#2,-(sp)
	move.w	d1,-(sp)
	DOS	_FPUTC
	addq.w	#4,sp
end_rem?:
	cmpi.b	#$0a,d1
	bne	remlp
	addq.l	#1,line_number-work(a6)
	bra	main_loop
else:					*数字??
	bsr	chk_num
	bpl	get_ntn
	bra	syntax_error

get_ntn_:			*(音階指定)
	bsr	asc_to_n	*d1=note num(or octave num)
	tst.l	d4
	beq	syntax_error
	move.l	d1,d2		*d2=note number(or ocatve num)
	bsr	get_note_num	*英字だったらノートナンバー指定
	bmi	illegal_n
	bra	ocr_0
get_ntn:			*(ノート番号指定)
	bsr	asc_to_n	*d1=note num(or octave num)
	tst.l	d4
	beq	syntax_error
	move.l	d1,d2		*d2=note number(or ocatve num)
ocr_0:
	cmpi.l	#adpcm_n_max,d2
	bcc	illegal_n	*illegal note number
	bsr	skip_sep
	tst.l	d4
	beq	syntax_error
	bsr	chk_num
	bmi	do_copy_fnm
	bsr	asc_to_n		*ファイルネームではなくて数値のケース
	move.l	d1,filename-work(a6)	*転送もとの数値をファイルネームワークへセーブ
	bra	get_param
do_copy_fnm:
	move.b	(a4),d0
	cmpi.b	#'.',d0
	beq	bykc
	cmpi.b	#'#',d0
	beq	bykc
	cmpi.b	#' ',d0
	bls	syntax_error
	lea	filename(pc),a0	*destination
	bsr	copy_fn		*copy filename to work
get_param:
	move.l	#$ffff,mix_p-work(a6)	*non mix,delay=0
	moveq.l	#0,d0
	move.l	d0,pitch_p-work(a6)	*non pitch shift
	move.l	d0,vol_p-work(a6)	*vol=100%
	move.l	d0,fade_p-work(a6)
	move.l	d0,cut_p-work(a6)
	move.b	d0,rv_p-work(a6)
	tst.l	d4
	beq	do_read_cnf
ocr_lp:
	bsr	skip_sep
	cmpi.b	#'/',(a4)
	beq	do_read_cnf	*もう後ろにパラメータはない
	subq.l	#1,d4
	bmi	do_read_cnf_
	move.b	(a4)+,d0
	cmpi.b	#' ',d0
	bcs	do_read_cnf	*もう後ろにパラメータはない
	bsr	mk_capital
	cmpi.b	#'C',d0
	beq	get_cp
	cmpi.b	#'F',d0
	beq	get_fp
	cmpi.b	#'R',d0
	beq	get_rv
	cmpi.b	#'D',d0
	beq	get_dp
	cmpi.b	#'M',d0
	beq	get_mp		*get mix p
	cmpi.b	#'V',d0
	beq	get_vp		*get vol_p
	cmpi.b	#'P',d0
	bne	syntax_error

*get_pp:			*pitch change parameter
	bsr	chk_num
	bmi	syntax_error
	bsr	asc_to_n
	add.l	#12,d1
	bmi	illegal_pitch_err
	cmpi.l	#24,d1
	bhi	illegal_pitch_err
	bset.l	#31,d1
	move.l	d1,pitch_p-work(a6)
	tst.l	vol_p-work(a6)
	bne	ocr_lp
	move.l	#100,vol_p-work(a6)
	bra	ocr_lp

get_vp:
	bsr	chk_num
	bmi	syntax_error
	bsr	asc_to_n
	move.l	d1,vol_p-work(a6)
	beq	illegal_vol_err		*ボリュームにマイナスは駄目
	bmi	illegal_vol_err		*ボリュームにマイナスは駄目
	tst.l	pitch_p-work(a6)
	bne	ocr_lp
	move.l	#12,pitch_p-work(a6)
	bra	ocr_lp

get_dp:				*mix delay parameter
	bsr	chk_num
	bmi	illegal_delay_err
	bsr	asc_to_n
	cmpi.l	#$ffff,d1
	bhi	illegal_delay_err
	move.w	d1,mix_p-work(a6)
	bra	ocr_lp

get_mp:
	bsr	chk_num
	bpl	@f		*note number指定の場合
	subq.l	#1,d4
	bmi	syntax_error
	move.b	(a4)+,d0
	andi.b	#$df,d0
	cmpi.b	#'O',d0
	bne	syntax_error	*変な文字?
	bsr	chk_num
	bmi	syntax_error
	bsr	asc_to_n	*get dest. oct
	move.l	d2,d0		*save set note number
	move.l	d1,d2
	bsr	get_note_num
	bmi	illegal_n
	move.l	d2,d1
	move.l	d0,d2		*get back save note number
	bra	chk_mnote
@@:
	bsr	asc_to_n
chk_mnote:
	tst.l	d1
	bmi	illegal_n
	cmpi.l	#adpcm_n_max,d1
	bcc	illegal_n
	move.w	d1,mix_p+2-work(a6)
	bsr	skip_sep
	bsr	chk_num
	bmi	ocr_lp
	bsr	asc_to_n
	cmpi.l	#$ffff,d1
	bhi	illegal_delay_err
	move.w	d1,mix_p-work(a6)
	bra	ocr_lp

get_rv:
	st.b	rv_p-work(a6)
	bra	ocr_lp

get_cp:				*truncate parameter
	bsr	chk_num
	bmi	illegal_p
	bsr	asc_to_n
	cmpi.l	#$ffff,d1
	bhi	illegal_offst_err
	move.w	d1,cut_p-work(a6)
	moveq.l	#0,d1
	bsr	chk_num
	bmi	@f
	bsr	asc_to_n
	cmpi.l	#$ffff,d1
	bhi	illegal_cutsz_err
@@:
	move.w	d1,cut_p+2-work(a6)
	tst.l	cut_p-work(a6)
	beq	illegal_p
	bra	ocr_lp

get_fp:				*fade in/out parameter
	moveq.l	#0,d1
	bsr	chk_num
	bmi	@f
	bsr	asc_to_n
@@:
	move.b	#1,fade_p+2-work(a6)	*mode
	tst.l	d1
	bpl	@f
	neg.l	d1
	neg.b	fade_p+2-work(a6)	*mode
@@:
	cmpi.l	#$ffff,d1
	bhi	illegal_offst_err
	move.w	d1,fade_p-work(a6)
	bsr	skip_sep
	bsr	chk_num
	bmi	ocr_lp		*default=0
	bsr	asc_to_n
	cmpi.l	#$7f,d1
	bhi	illegal_outlvl_err
	move.b	d1,fade_p+3-work(a6)	*level
	bra	ocr_lp

bykc:				*note numberによるコピーノート指定
	subq.l	#1,d4
	bmi	syntax_error
	addq.w	#1,a4
	subq.l	#1,d4
	bmi	syntax_error
	move.b	(a4)+,d0
	andi.b	#$df,d0
	cmpi.b	#'O',d0
	bne	syntax_error		*変な文字?
	bsr	chk_num
	bmi	syntax_error
	bsr	asc_to_n	*get dest. oct
	tst.l	d4
	beq	syntax_error	*KEY nameがない
	move.l	d2,d0		*save set note number
	move.l	d1,d2
	bsr	get_note_num
	bmi	illegal_n
	move.l	d2,filename-work(a6)
	move.l	d0,d2		*get back save note number
	bra	get_param

do_read_cnf_:
	addq.l	#1,d4
do_read_cnf:
	movem.l	d0-d7/a0-a6,-(sp)
	move.l	pitch_p(pc),d3
	swap	d3
	move.l	vol_p(pc),d0
	move.w	d0,d3
	move.l	mix_p(pc),d4
	move.l	cut_p(pc),d5
	move.b	rv_p(pc),d6
	move.l	fade_p(pc),d7
	lea	filename(pc),a1
	bsr	adpcm_read
	movem.l	(sp)+,d0-d7/a0-a6
	bra	main_loop

chk_mj:
	* X d0 a0
	* ? d4 a4
	* - d1
	move.l	(sp)+,a0	*get return address
	subq.l	#1,d4
	bmi	all_end
	move.b	(a4)+,d0
	andi.b	#$df,d0
	cmp.b	d1,d0
	bne	syntax_error
	jmp	(a0)		*return

bank_sel:
	chk_mj	#'A'
	chk_mj	#'D'
	chk_mj	#'P'
	chk_mj	#'C'
	chk_mj	#'M'
	chk_mj	#'_'
	chk_mj	#'B'
	chk_mj	#'A'
	chk_mj	#'N'
	chk_mj	#'K'
	bsr	skip_sep
	tst.l	d4
	beq	syntax_error
	bsr	chk_num		*値あるか
	bmi	illegal_p	*値省略
	bsr	asc_to_n	*値をゲット
	subq.l	#1,d1		*0～
	lsl.l	#7,d1		*128倍
	cmp.l	#adpcm_n_max,d1
	bcc	bank_ovf
	move.w	d1,adpcm_bank-work(a6)
	bra	main_loop

erase:
	chk_mj	#'E'
	chk_mj	#'R'
	chk_mj	#'A'
	chk_mj	#'S'
	chk_mj	#'E'
	bsr	skip_sep
	tst.l	d4
	beq	syntax_error
	bsr	chk_num
	bmi	get_erkc
	bsr	asc_to_n
chk_erkc:
	cmpi.l	#adpcm_n_max,d1
	bcc	illegal_n	*illegal note number
	lea	adpcm_ptr(pc),a0
	lsl.l	#3,d1
	lea	(a0,d1.l),a3
	tst.l	(a3)
	beq	no_dat_error	*空です

	move.w	#2,-(sp)
	pea	ers_mes(pc)
	DOS	_FPUTS
	addq.w	#6,sp

	lsr.w	#3,d1
	move.l	d1,d0
	bsr	num_to_str
	move.w	#2,-(sp)
	pea	suji+6(pc)	*NOTE NUMBER
	DOS	_FPUTS
	addq.w	#6,sp

	bsr	PTT
	bsr	do_erase
	bsr	OK
	bra	main_loop

do_erase:
	* < a0.l=adpcm_ptr 0
	* < a3.l=adpcm_ptr n
	move.l	(a3),a2		*erase nt address
	move.l	4(a3),d3	*erase nt size
	addq.l	#1,d3
	bclr.l	#0,d3
	lea	(a2,d3.l),a1	*next data address
	move.l	adpcm_buffer_next(pc),d2
	sub.l	a1,d2		*d2=trans size
	bmi	exit_erase	*??
	moveq.l	#5,d1		*mode
	bsr	trans_dma
	sub.l	d3,adpcm_buffer_next-work(a6)
	sub.l	d3,total_size-work(a6)

	move.w	#adpcm_n_max-1,d1
erase_lp:
	cmp.l	(a0),a2
	bhi	next_dtbl
	beq	erase_tbl
	sub.l	d3,(a0)		*消したサイズ分引く
	bra	next_dtbl
erase_tbl:
	clr.l	(a0)		*erase
	clr.l	4(a0)
next_dtbl:
	addq.w	#8,a0
	dbra	d1,erase_lp
exit_erase:
	rts

get_erkc:				*キーコードでイレーズ
	subq.l	#1,d4
	bmi	syntax_error
	move.b	(a4)+,d0
	cmpi.b	#'.',d0
	beq	get_erkc
	andi.b	#$df,d0
	cmpi.b	#'O',d0
	bne	syntax_error		*変な文字?
	bsr	chk_num
	bmi	syntax_error
	bsr	asc_to_n	*get dest. oct
	tst.l	d4
	beq	syntax_error
	move.l	d1,d2
	bsr	get_note_num
	bmi	illegal_n
	move.l	d2,d1
	bra	chk_erkc

all_end:				*テーブルの整理
	move.w	#2,-(sp)
	pea	saving(pc)
	DOS	_FPUTS
	addq.w	#6,sp

	move.w	#2,-(sp)
	pea	d_name(pc)
	DOS	_FPUTS
	addq.w	#6,sp

	bsr	PTT2

	lea	adpcm_ptr(pc),a1
	movea.l	adpcm_buffer_top(pc),a2
	move.l	a2,d3
	addq.w	#8,a2			*HEADERの分加算
	move.w	#adpcm_n_max-1,d1
	moveq.l	#0,d2
apt_lp:
	tst.l	(a1)			*chk address
	beq	next_apt
	move.w	d2,(a2)+		*save note number
	move.l	(a1),d0
	addq.w	#4,a2
	sub.l	a2,d0
	move.l	d0,-4(a2)
	move.l	4(a1),(a2)+		*save size
next_apt:
	addq.w	#8,a1
	addq.w	#1,d2
	dbra	d1,apt_lp

	move.w	#-1,(a2)+		*end code
	movea.l	d3,a1
	move.l	#adpcm_n_max*10+8+4,d4
	add.l	d4,a1			*source
	move.l	a1,d5
	sub.l	a2,d5			*d5=差
	cmpa.l	a1,a2			*adpcm_n_max個のテーブルがあった場合は転送の必要は無し
	beq	mk_offset
	moveq.l	#5,d1		*mode
	move.l	total_size(pc),d2
	sub.l	d4,d2			*table sizeの分を差引く
	bsr	trans_dma
	sub.l	adpcm_buffer_top(pc),a2
	add.l	a2,d2			*d2=data total size
mk_offset:
	movea.l	d3,a1
	lea	8+2(a1),a1		*+8=header size
mkof_lp:
	tst.w	-2(a1)
	bmi	do_save
	sub.l	d5,(a1)+
	addq.w	#6,a1
	bra	mkof_lp
do_save:
	move.l	d2,-(sp)
	move.l	adpcm_buffer_top(pc),-(sp)
	move.w	sfh(pc),-(sp)
	DOS	_WRITE
	lea	10(sp),sp
	move.l	d0,-(sp)
	bmi	write_error

	move.l	date_buf(pc),-(sp)
	move.w	sfh(pc),-(sp)
	DOS	_FILEDATE
	addq.w	#6,sp

	DOS	_ALLCLOSE
	cmp.l	(sp)+,d2
	bne	dev_full

	bsr	OK2

	move.w	#2,-(sp)
	pea	no_er_mes(pc)
	DOS	_FPUTS
	addq.w	#6,sp

	DOS	_EXIT

adpcm_read:			*APDCMファイルを読み込む
	* < a1=filename address / (a1).l=source data number
	* < d2.l=note number(0-255)
	* < d3.hw=pitch_parametr(0～$18) d3.lw=volume parametr(1～300)
	* < d4.hw=mixing offset d4.lw=mixing pcm number(0～)/d4.lw=-1:non mix
	* < d5.hw=cut offset d5.lw=cut size
	* < d6.b=reverse(=0:no,nz:yes)
	* < d7.hw=fade point d7.lwhb=mode(-1:in,+1:out) d7.lwlb=fade in/out level

	clr.b	p16_or_not-work(a6)
	clr.b	read_or_not-work(a6)
	move.l	d4,mix_p-work(a6)	*save mix parameter
	move.l	d5,cut_p-work(a6)
	move.b	d6,rv_p-work(a6)
	move.l	d7,fade_p-work(a6)

	move.l	d2,d2_work-work(a6)

	*ノートナンバー表示
	move.l	d2,d0
	bsr	num_to_str
	move.w	#2,-(sp)
	pea	suji+6(pc)
	DOS	_FPUTS
	addq.w	#6,sp
	*ノート表示
	move.l	d2,d0
	bsr	mk_note_num
	move.w	#2,-(sp)
	pea	suji(pc)
	DOS	_FPUTS
	addq.w	#6,sp

	bsr	SPC
	bsr	EQU

	tst.b	(a1)
	beq	non_read	*ファイルネームがない場合はワーク間コピー

	move.l	a1,a0		*.p16かどうか検査
@@:
	move.b	(a0)+,d0
	beq	adfop0
	cmpi.b	#'.',d0
	bne	@b
@@:
	move.b	(a0)+,d0
	beq	adfop0
	cmpi.b	#'P',d0
	bne	adfop0
	move.b	(a0)+,d0
	beq	adfop0
	cmpi.b	#'1',d0
	bne	adfop0
	move.b	(a0)+,d0
	beq	adfop0
	cmpi.b	#'6',d0
	bne	adfop0
	st.b	p16_or_not-work(a6)
adfop0:
	move.w	#2,-(sp)
	pea	(a1)		*print filename
	DOS	_FPUTS
	addq.w	#6,sp

	bsr	PTT

	movea.l	a1,a2
	bsr	fopen
	tst.l	d5
	bmi	not_found

	move.w	#2,-(sp)	*ファィルの長さを調べる
	clr.l	-(sp)
	move.w	d5,-(sp)
	DOS	_SEEK
 	addq.w	#8,sp		*d0.l=file length
	move.l	d0,d4		*d4=length
	beq	file_len_error
	bmi	file_len_error

	move.l	d4,-(sp)
	move.w	#2,-(sp)
	DOS	_MALLOC2
	addq.w	#6,sp
	move.l	d0,read_adr-work(a6)
	bmi	mem_error	*out of memory
	move.l	d0,a1

	clr.w	-(sp)		*ファイルポインタを元に戻す
	clr.l	-(sp)
	move.w	d5,-(sp)
	DOS	_SEEK
	addq.w	#8,sp

	move.l	d4,-(sp)	*push size
	pea	(a1)		*push addr
	move.w	d5,-(sp)	*file handle
	DOS	_READ		*サンプリングデータの読み込み
	lea	10(sp),sp
	tst.l	d0
	bmi	read_error

	move.w	d5,-(sp)	*close
	DOS	_CLOSE
	addq.l	#2,sp

	st	read_or_not-work(a6)	*mark
	bra	process?
non_read:			*ディスクからでなくてメモリから
	lea.l	adpcm_ptr(pc),a0
	move.l	(a1)+,d0	*get note number
	lsl.l	#3,d0		*8倍
	adda.l	d0,a0
	move.l	4(a0),d4	*get size
	beq	no_dat_error

	lsr.w	#3,d0
	move.l	d0,-(sp)
	move.l	d0,-(sp)

	*ノートナンバー表示
	move.l	(sp)+,d0
	bsr	num_to_str
	move.w	#2,-(sp)
	pea	suji+6(pc)
	DOS	_FPUTS
	addq.w	#6,sp
	*ノート表示
	move.l	(sp)+,d0
	bsr	mk_note_num
	move.w	#2,-(sp)
	pea	suji(pc)
	DOS	_FPUTS
	addq.w	#6,sp

	bsr	PTT

	tst.l	d4
	beq	no_dat_error	*lengthが０なのでコピー不可

	move.l	d4,-(sp)
	move.w	#2,-(sp)
	DOS	_MALLOC2
	addq.w	#6,sp
	move.l	d0,read_adr-work(a6)
	bmi	mem_error	*out of memory
	move.l	d0,a2		*destination

do_copy_data:
	movea.l	(a0),a1		*source
	moveq.l	#5,d1
	move.l	d4,d2
	bsr	trans_dma
	move.l	a2,a1		*data exist adr
process?:			*加工処理
	* < a1=data address
	* < d4.l=size
	moveq.l	#0,d0
	move.l	d0,work_adr-work(a6)
	move.l	d0,dest_adr-work(a6)
	tst.w	mix_p+2-work(a6)
	bpl	process0	*MIXINGをするなら必ず加工
	move.l	d3,d0
	or.b	rv_p(pc),d0	*mode
	or.l	fade_p(pc),d0	*mode
	or.l	cut_p(pc),d0	*size
	beq	exit_smp_read	*加工処理無しのケース
process0:
	tst.l	d3		*ピッチ音量変更を伴うか
	bne	process1
	move.l	#$000c_0064,d3	*non pitch and 100%
process1:
	move.l	d4,d1		*d1=source size
	tst.b	p16_or_not-work(a6)
	bne	@f
	lsl.l	#2,d1
@@:
	swap	d3		*d3.w=pitch parameter
	cmpi.w	#12,d3
	bcc	@f
	add.l	d1,d1		*2倍(PITCH DOWNはデータサイズが最大２倍になるため)
@@:
	move.l	d1,-(sp)
	move.w	#2,-(sp)
	DOS	_MALLOC2
	addq.w	#6,sp		*ワーク確保
	move.l	d0,work_adr-work(a6)
	bmi	mem_error

	move.w	#2,-(sp)
	pea	proc(pc)
	DOS	_FPUTS
	addq.w	#6,sp

	bsr	PTT

	movea.l	a1,a0			*source
	movea.l	work_adr(pc),a1		*destination
	move.l	d4,d0		*size
	moveq.l	#0,d1
	move.w	d3,d1		*d1=pitch shift
	swap	d3
	moveq.l	#0,d6
	move.w	d3,d6		*d6=volume %
	tst.b	p16_or_not-work(a6)
	beq	@f
	bsr	transform_pcm
	bra	grpcsz
@@:
	bsr	adpcm_to_pcm	*a1=PCM data size
grpcsz:
	move.l	a1,d4		*d4=PCM data size
truncate?:
	lea	cut_p(pc),a0
	tst.l	(a0)
	beq	reverse?
	move.l	work_adr(pc),a2
	moveq.l	#0,d0
	move.w	(a0)+,d0	*get offset
	lsl.l	#2,d0
	cmp.l	d4,d0
	bcc	illegal_offst_err	*offsetが大きすぎ
	lea.l	(a2,d0.l),a1
	moveq.l	#5,d1		*dma mode
	moveq.l	#0,d2
	move.w	(a0)+,d2	*get cut size
	bne	@f
	move.l	d4,d2
	sub.l	d0,d2
	bra	do_truncate
@@:
	lsl.l	#2,d2
	add.l	d2,d0		*パラメータがおかしくないか
	cmp.l	d4,d0
	bhi	illegal_cutsz_err
do_truncate:
	bsr	trans_dma
	move.l	d2,d4		*PCM size
reverse?:			*リバース??
	tst.b	rv_p-work(a6)
	beq	fade_in_out?
	move.l	d4,-(sp)
	move.w	#2,-(sp)
	DOS	_MALLOC2
	addq.w	#6,sp		*ワーク確保
	tst.l	d0
	bmi	mem_error
	move.l	d0,a2		*destination
	pea	(a2)
	move.l	work_adr(pc),a1	*source
	add.l	d4,a1
	move.l	d4,d0
	lsr.l	d0		*d0.l=adpcm count
@@:
	move.w	-(a1),(a2)+
	subq.l	#1,d0
	bne	@b
	move.l	work_adr(pc),-(sp)	*ワークを切り捨てる
	DOS	_MFREE
	addq.w	#4,sp
	move.l	(sp)+,work_adr-work(a6)
fade_in_out?:
	lea	fade_p(pc),a0
	tst.l	(a0)
	beq	mix?
	move.l	work_adr(pc),a1
	moveq.l	#0,d0
	move.w	(a0)+,d0
	lsl.l	#2,d0
	cmp.l	d4,d0
	bcc	illegal_offst_err
	moveq.l	#0,d5
	tst.b	(a0)+		*check mode
	bpl	@f
				*case:fade in
	move.l	d0,d1		*fade count*2
	move.b	(a0),d5		*get in level
	moveq.l	#1,d7
	bra	calc_fio
@@:				*case:fade out
	add.l	d0,a1		*start point
	move.l	d4,d1
	sub.l	d0,d1		*fade count*2
	move.b	#128,d5
	moveq.l	#-1,d7
calc_fio:
	lsr.l	d1		*fade count
	move.l	#128,d0
	sub.b	(a0)+,d0	*get in/end level
	move.l	d1,d3
	bsr	wari2		d0.l/d1.l=d0.l...d1.l
	move.w	d0,d2		*step
	move.l	d1,d0
	swap	d0
	clr.w	d0		*d0=あまり*65536
	move.l	d3,d1
	bsr	wari2		d0.l/d1.l=d0.l...d1.l
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
mix?:
	move.l	work_adr(pc),a0	*a0.l=PCM data address
	lsr.l	#2,d4		*d4.l=adpcm size
	moveq.l	#0,d0
	move.w	mix_p+2(pc),d0
	bmi	get_save_adr	*non mix case
case_mix:
	* < a0.l=PCM data address
	* < d4.l=src adpcm data size	*MIXING
	lsl.l	#3,d0		*8倍
	lea.l	adpcm_ptr(pc),a3
	add.l	d0,a3		*a3=mix destination parameter pointer
	tst.l	(a3)
	beq	no_dat_error
	move.l	4(a3),d2
	moveq.l	#0,d0
	move.w	mix_p(pc),d0
	move.l	d0,mix_p-work(a6)
	add.l	d0,d2		*add delay count
	lsl.l	#2,d2

	move.l	d2,-(sp)
	move.w	#2,-(sp)
	DOS	_MALLOC2	*MIX DESTINATION用のワーク
	addq.w	#6,sp
	move.l	d0,dest_adr-work(a6)
	bmi	mem_error
	move.l	d0,a1

	move.l	a1,a2
	move.l	mix_p(pc),d0	*ディレイカウント分０で埋める
	beq	cnv_dst
	add.l	d0,d0
@@:
	clr.w	(a1)+
	subq.l	#1,d0
	bne	@b
cnv_dst:
	move.l	(a3),a0		*a0=adpcm data buffer
	move.l	4(a3),d0	*d0=destination adpcm data size
	movem.l	d4/a2/a3,-(sp)
	bsr	just_adpcm_to_pcm
	movem.l	(sp)+,d4/a1/a3	*わざとa1
	move.l	work_adr(pc),a0	*src data address
	move.l	4(a3),d0
	add.l	mix_p(pc),d0
	cmp.l	d0,d4		*srcとdestどっちが大きい?
	bhi	src_big_case
	*srcが小さいケース
	add.l	d4,d4		*d4=pcm data cnt
	move.l	a1,a2
mix_lp01:
	move.w	(a0)+,d0
	add.w	d0,(a2)+
	subq.l	#1,d4
	bne	mix_lp01
	move.l	4(a3),d4
	add.l	mix_p(pc),d4
	move.l	a1,a0
	bra	get_save_adr
src_big_case:
	*srcが大きいケース
	movea.l	a0,a2
	move.l	4(a3),d1
	add.l	mix_p(pc),d1
	add.l	d1,d1
mix_lp02:
	move.w	(a1)+,d0
	add.w	d0,(a2)+
	subq.l	#1,d1
	bne	mix_lp02
get_save_adr:
	* < a0.l=data address
	* < d4.l=adpcm size
	movem.l	a0-a1,-(sp)
	lea	adpcm_ptr(pc),a0
	move.l	d2_work(pc),d0
	lsl.l	#3,d0
	lea	(a0,d0.l),a3
	tst.l	(a3)
	beq	@f
	bsr	do_erase
@@:
	movem.l	(sp)+,a0-a1

	move.l	adpcm_buffer_next(pc),a1
	move.l	d4,d0
	addq.l	#1,d0
	bclr.l	#0,d0
	add.l	d0,adpcm_buffer_next-work(a6)
	add.l	d0,total_size-work(a6)

	move.l	total_size(pc),-(sp)
	move.l	adpcm_buffer_top(pc),-(sp)
	DOS	_SETBLOCK
	addq.w	#8,sp
	tst.l	d0
	bmi	mem_error

	* < d4.l=data size
	* < a1.l=save address
	* < a0.l=PCM data address
	movem.l	d4/a1,-(sp)
	exg.l	a1,a0
	move.l	d4,d0
	bsr	pcm_to_adpcm	*a1->a0
	movem.l	(sp)+,d4/a1

	tst.l	work_adr-work(a6)	*確保したワーク群の開放
	beq	@f
	move.l	work_adr(pc),-(sp)
	DOS	_MFREE
	addq.w	#4,sp
@@:
	tst.l	dest_adr-work(a6)
	beq	@f
	move.l	dest_adr(pc),-(sp)
	DOS	_MFREE
	addq.w	#4,sp
@@:
	move.l	a1,a2
	bra	set_adr_work

exit_smp_read:			*パラメータを書き込んで帰還
	* < a1=data adr		*読み込んだか単なるコピーのケース
	* < d4.l=data size
	move.l	a0,-(sp)
	lea	adpcm_ptr(pc),a0
	move.l	d2_work(pc),d0
	lsl.l	#3,d0
	lea	(a0,d0.l),a3
	tst.l	(a3)
	beq	@f
	bsr	do_erase
@@:
	move.l	(sp)+,a0

	move.l	adpcm_buffer_next(pc),a2
	move.l	d4,d0
	addq.l	#1,d0
	bclr.l	#0,d0
	add.l	d0,adpcm_buffer_next-work(a6)
	add.l	d0,total_size-work(a6)

	move.l	total_size(pc),-(sp)
	move.l	adpcm_buffer_top(pc),-(sp)
	DOS	_SETBLOCK
	addq.w	#8,sp
	tst.l	d0
	bmi	mem_error
	move.l	d4,d2
	moveq.l	#5,d1
	bsr	trans_dma

set_adr_work:
	move.l	read_adr(pc),-(sp)
	DOS	_MFREE		*ワーク開放
	addq.w	#4,sp

	lea.l	adpcm_ptr(pc),a0
	move.l	d2_work(pc),d2	*get back note number
	lsl.l	#3,d2		*8倍
	adda.l	d2,a0
	tst.l	(a0)
	beq	@f
	move.w	#2,-(sp)
	pea	warn(pc)
	DOS	_FPUTS
	addq.w	#6,sp
@@:
	move.l	a2,(a0)+	*set address
	move.l	d4,(a0)		*set size
	bra	OK

wari:				*32ﾋﾞｯﾄ/16ﾋﾞｯﾄ=32ﾋﾞｯﾄ...32ﾋﾞｯﾄ
	* < d1.l/d2.l=d1.l ...d2.l
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

wari2:				*32ﾋﾞｯﾄ/32ﾋﾞｯﾄ=32ﾋﾞｯﾄ...32ﾋﾞｯﾄ
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

just_adpcm_to_pcm:		*ピッチチェンジやレベルチェンジを
				*行わない単なるADPCM→PCM変換
	* < a0=adpcm data buffer
	* < a1=pcm data buffer
	* < d0.l=adpcm data size
	* > a1=data size
	pea	(a6)
	lea	scaleval(pc),a5
	lea	levelchg(pc),a6
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
	move.l	(sp)+,a6
	rts

adpcm_to_pcm:				*ＡＤＰＣＭをＰＣＭデータへ変換する
	* < a0=adpcm data buffer
	* < a1=pcm data buffer
	* < d0.l=data size
	* < d1.w=note shift(0～b～c(non shift)～d～18)
	* < d6.l=volume value(0%～200%)
	* > a1=data size
	pea	(a6)
				*パーセンテージを１６進変換
	lea	up_down(pc),a2
	clr.b	(a2)
	lea	last_val(pc),a3
	clr.w	(a3)
	lea	hajimete(pc),a4
	clr.b	(a4)

	pea	(a1)
	lsl.l	#7,d6
	divu	#100,d6
	andi.l	#$ffff,d6	*d6=0～200→0～256

	add.w	d1,d1		*d1=d1*2
	cmpi.b	#$18,d1
	beq	est		*ピッチシフトは無しのケース
	bcs	shift_down	*ピッチを下げるケース
*shift_up			*ピッチを上げるケース
	lea	frq_tbl(pc),a5
	move.w	-26(a5,d1.w),6(a3)	*frq_flg
	move.b	#1,(a2)			*up_down	*set switch
	bra	init_wk
shift_down:
	lea	frq_tbl2(pc),a5
	move.w	(a5,d1.w),6(a3)		*frq_flg
	st	(a2)			*up_down	*set switch
init_wk:
	clr.w	2(a3)			*last_val2
	clr.w	4(a3)			*frq_wk
est:
	lea	scaleval(pc),a5
	lea	levelchg(pc),a6
	moveq.l	#0,d3
	moveq.l	#0,d7
	moveq.l	#$0f,d4
	add.l	d0,d0
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

*	andi.l	#$ffff,d2	*ボリュームの変更
	muls	d6,d2		*d6にはパーセントが
	asr.l	#7,d2		*入っている

	bset.b	#0,(a4)		*初めて
	beq	wrt_pcm_dt
	tst.b	(a2)
	beq	wrt_pcm_dt	*non pitch shift
	bmi	do_shift_down

	move.w	6(a3),d1
	add.w	d1,4(a3)
	bcc	check_dsz
	bra	wrt_pcm_dt
do_shift_down:
	move.w	6(a3),d1
	beq	@f
	add.w	d1,4(a3)
	bcc	wrt_pcm_dt
@@:
	move.w	d2,d1
	add.w	2(a3),d1
	asr.w	d1		*線形補間
	move.w	d1,(a1)+
wrt_pcm_dt:
	move.w	d2,(a1)+	*store pcm data to buffer
check_dsz:
	move.w	d2,2(a3)
	subq.l	#1,d0
	bne	atp_lp
	sub.l	(sp)+,a1	*data size
	move.l	(sp)+,a6
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

transform_pcm:				*ＰＣＭデータの変換
	* < a0=source pcm data buffer
	* < a1=destination pcm data buffer
	* < d0.l=data size
	* < d1.w=note shift(0～b～c(non shift)～d～18)
	* < d6.l=volume value(0%～200%)
	* > a1=data size
				*パーセンテージを１６進変換
	lea	up_down(pc),a2
	clr.b	(a2)
	lea	last_val(pc),a3
	clr.w	(a3)
	lea	hajimete(pc),a4
	clr.b	(a4)

	move.l	a1,-(sp)
	lsl.l	#7,d6
	divu	#100,d6
	andi.l	#$ffff,d6	*d6=0～200→0～256

	add.w	d1,d1		*d1=d1*2
	cmpi.b	#$18,d1
	beq	est__		*ピッチシフトは無しのケース
	bcs	shift_down__	*ピッチを下げるケース
*shift_up			*ピッチを上げるケース
	lea	frq_tbl(pc),a5
	move.w	-26(a5,d1.w),6(a3)	*frq_flg
	move.b	#1,(a2)			*up_down	*set switch
	bra	init_wk__
shift_down__:
	lea	frq_tbl2(pc),a5
	move.w	(a5,d1.w),6(a3)		*frq_flg
	st	(a2)			*up_down	*set switch
init_wk__:
	clr.w	2(a3)			*last_val2
	clr.w	4(a3)			*frq_wk
est__:
	lsr.l	d0
atp_lp__:
	move.w	(a0)+,d2	*実際の計算

*	andi.l	#$ffff,d2	*ボリュームの変更
	muls	d6,d2		*d6にはパーセントが
	asr.l	#7,d2		*入っている

	bset.b	#0,(a4)		*初めて
	beq	wrt_pcm_dt__
	tst.b	(a2)
	beq	wrt_pcm_dt__	*non pitch shift
	bmi	@f

	move.w	6(a3),d1
	add.w	d1,4(a3)
	bcc	check_dsz__
	bra	wrt_pcm_dt__
@@:
	move.w	6(a3),d1
	beq	@f
	add.w	d1,4(a3)
	bcc	wrt_pcm_dt__
@@:
	move.w	d2,d1
	add.w	2(a3),d1
	asr.w	d1		*線形補間
	move.w	d1,(a1)+
wrt_pcm_dt__:
	move.w	d2,(a1)+	*store pcm data to buffer
check_dsz__:
	move.w	d2,2(a3)
	subq.l	#1,d0
	bne	atp_lp__
	sub.l	(sp)+,a1	*data size
	rts

pcm_to_adpcm:			*ＰＣＭデータをＡＤＰＣＭデータへ変換する
	* < a0=adpcm data buffer
	* < a1=pcm data buffer
	* < d0.l=data size
	* X d0-d5/a1,a2,a3,a5,a6

	pea	(a6)

	lea	scaleval(pc),a5
	lea	levelchg(pc),a6

	moveq.l	#0,d6		*scalelevel=0
	moveq.l	#0,d7
	moveq.l	#0,d4
	add.l	d0,d0
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
	move.l	(sp)+,a6
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
	beq	file_len_error
	bmi	file_len_error

	move.l	d0,-(sp)
	DOS	_MALLOC
	addq.w	#4,sp
	tst.l	d0
	bmi	mem_error	*OUT OF MEMORY
	move.l	d0,a5

	clr.w	-(sp)		*ファイルポインタを元に戻す
	clr.l	-(sp)
	move.w	d5,-(sp)
	DOS	_SEEK
	addq.w	#8,sp

	move.l	d3,-(sp)	*push size
	pea	(a5)		*push addr
	move.w	d5,-(sp)	*file handle
	DOS	_READ
	lea	10(sp),sp
	tst.l	d0
	bmi	read_error0	*読み込み失敗

	clr.l	-(sp)
	move.w	d5,-(sp)
	DOS	_FILEDATE
	addq.w	#6,sp
	move.l	d0,date_buf-work(a6)

	move.w	d5,-(sp)	*close
	DOS	_CLOSE
	addq.l	#2,sp
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
	subq.l	#1,d4
	addq.w	#1,a4		*skip '-'
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

skip_spc_:
@@:
	tst.b	(a2)
	beq	@f
	cmpi.b	#' ',(a2)+	*最初のファイルネームまでスキップ
	bls	@b
	subq.w	#1,a2
@@:
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

skip_plus:			*PLUSをスキップする
	subq.l	#1,d4
	bmi	exit_skip_plus
	cmpi.b	#'+',(a4)+
	beq	skip_plus
	subq.w	#1,a4
exit_skip_plus:
	addq.l	#1,d4
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

chk_num:			*数字かどうかチェック
	* > eq=number
	* > mi=not num
	move.l	d0,-(sp)
	bsr	skip_spc
	move.b	(a4),d0
	cmpi.b	#'%',d0
	beq	yes_num
	cmpi.b	#'$',d0
	beq	yes_num
	cmpi.b	#'-',d0
	beq	yes_num
	cmpi.b	#'+',d0
	beq	yes_num
	cmpi.b	#'0',d0
	bcs	not_num
	cmpi.b	#'9',d0
	bhi	not_num
yes_num:
	moveq.l	#0,d0		*eq
	bra	@f
not_num:
	moveq.l	#-1,d0		*mi
@@:
	movem.l	(sp)+,d0	*わざとmovem
	rts

get_note_num:			*ノートナンバーで指定してきたケースの対処
	* < d2.l=octave
	movem.l	d0-d1/a2,-(sp)
	addq.l	#1,d2
	move.l	d2,d0
	moveq.l	#0,d2
	subq.l	#1,d4
	bmi	@f
	move.b	(a4)+,d2	*A-G
	andi.b	#$df,d2
	subi.b	#'A',d2
	lea	kc_value(pc),a2
	move.b	(a2,d2.w),d2	*d2=0～11(kc)
	mulu	#12,d0		*d0=d0*12
	add.b	d0,d2		*d0=オクターブを考慮したキー値(0～127)
	bsr	chk_chogo2	*♭・♯のチェック
	add.b	d1,d2		*d2=real key code
	add.w	adpcm_bank(pc),d2
	moveq.l	#0,d0
	movem.l	(sp)+,d0-d1/a2
	rts
@@:
	moveq.l	#-1,d0
	movem.l	(sp)+,d0-d1/a2
	rts

chk_chogo2:			*調号チェック
	* > d1=+1～-1
	moveq.l	#0,d1
chk_chogo_lp2:
	bsr	skip_spc
	tst.l	d4
	beq	@f
	cmpi.b	#'#',(a4)
	beq	case_sharp2
	cmpi.b	#'+',(a4)
	beq	case_sharp2
	cmpi.b	#'-',(a4)
	beq	case_flat2
@@:
	rts			*none
case_sharp2:
	addq.b	#1,d1		*+1
	subq.l	#1,d4
	addq.w	#1,a4
	bra	chk_chogo_lp2
case_flat2:
	subq.b	#1,d1		*-1
	subq.l	#1,d4
	addq.w	#1,a4
	bra	chk_chogo_lp2

srch_num:			*数字までスキップ
	* X d0
	subq.l	#1,d4
	bmi	@f
	move.b	(a4)+,d0
	cmpi.b	#$0d,d0
	beq	sn_err		*コマンドの途中で改行
	cmpi.b	#'$',d0
	beq	its_num
	cmpi.b	#'%',d0
	beq	its_num
	cmpi.b	#'0',d0
	bcs	srch_num
	cmpi.b	#'9',d0
	bhi	srch_num
its_num:
	subq.w	#1,a4
	addq.l	#1,d4
	moveq.l	#0,d0
	rts
sn_err:
	subq.w	#1,a4
@@:
	addq.l	#1,d4
	moveq.l	#-1,d0
	rts

copy_fn:			*ファイルネームをバッファへ転送
	* < a0.l=destination address
	* < a4=address
	* < d4=counter
	* X d0 a0
	movem.l	d0-d1/a0,-(sp)
	moveq.l	#0,d1
copy_fnlp01:
	subq.l	#1,d4
	bmi	ec1
	move.b	(a4)+,d0
	cmpi.b	#',',d0		*separater
	beq	exit_copyfn
	cmpi.b	#' ',d0		*ctrl code
	bls	exit_copyfn
	tst.b	d1
	beq	@f
	tst.b	-1(a0)
	bmi	st_lt
@@:
	bsr	mk_capital
st_lt:
	move.b	d0,(a0)+
	st.b	d1
	bra	copy_fnlp01
exit_copyfn:
	subq.w	#1,a4
ec1:
	addq.l	#1,d4
	clr.b	(a0)
	movem.l	(sp)+,d0-d1/a0
	rts

SPC:				*print space
	move.w	#2,-(sp)
	pea	spc_data(pc)
@@:
	DOS	_FPUTS
	addq.w	#6,sp
	rts
EQU:
	move.w	#2,-(sp)
	pea	equ_data(pc)
	bra	@b
PTT:
	move.w	#2,-(sp)
	pea	ptt_data(pc)
	bra	@b
OK:
	move.w	#2,-(sp)
	pea	ok_data(pc)
	bra	@b
CRLF:
	move.w	#2,-(sp)
	pea	crlf_data(pc)
	bra	@b

PTT2:
	move.w	#2,-(sp)
	pea	ptt_data(pc)
	bra	@b
OK2:
	move.w	#2,-(sp)
	pea	ok_data(pc)
	bra	@b

mk_note_num:				*ノート表示
	movem.l	d0-d1/a1,-(sp)
	lea	suji(pc),a1
	move.b	#'(',(a1)+
	divu	#128,d0
	add.b	#$31,d0			*bank
	move.b	d0,(a1)+
	move.b	#':',(a1)+
	clr.w	d0
	swap	d0
	divu	#12,d0
	moveq.l	#$2f,d1
	add.b	d1,d0
	cmp.b	d1,d0
	bhi	@f
	move.b	#'-',2(a1)
	move.b	#'1',3(a1)
	move.b	#')',4(a1)
	clr.b	5(a1)
	bra	dnd0
@@:
	move.b	d0,2(a1)
	move.b	#')',3(a1)
	move.b	#' ',4(a1)
	clr.b	5(a1)
dnd0:
	swap	d0
	add.w	d0,d0
	move.b	nn(pc,d0.w),(a1)+
	move.b	nn+1(pc,d0.w),(a1)+
	movem.l	(sp)+,d0-d1/a1
	rts

nn:	dc.b	'C '
	dc.b	'C#'
	dc.b	'D '
	dc.b	'D#'
	dc.b	'E '
	dc.b	'F '
	dc.b	'F#'
	dc.b	'G '
	dc.b	'G#'
	dc.b	'A '
	dc.b	'A#'
	dc.b	'B '

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
	tst.b	d2
	bne	nml_ktset
	tst.b	d4
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
	cache_flush_ng
	move.l	#$ff00,d3
trans_dma_lp:
	cmp.l	d3,d2
	bcs	go_single_dma

	movem.l	d2/a1-a2,-(sp)
	move.l	d3,d2
	IOCS	_DMAMOVE
	movem.l	(sp)+,d2/a1-a2
d3a1:	nop
d3a2:	nop
	sub.l	d3,d2
	bne	trans_dma_lp
bye_dma:
	cache_flush_ng
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

print_title:
	move.w	#2,-(sp)
	pea	title_mes(pc)
	DOS	_FPUTS
	addq.w	#6,sp
	rts

bye_bye_:
	moveq.l	#-1,d1			*カーソル位置補正
	IOCS	_B_LOCATE
	swap	d0
	tst.w	d0
	beq	@f
	bsr	CRLF
@@:
	pea	d_name(pc)
	DOS	_PRINT
	addq.w	#4,sp
	bra	prt_emsg

bye_bye:
	moveq.l	#-1,d1			*カーソル位置補正
	IOCS	_B_LOCATE
	swap	d0
	tst.w	d0
	beq	@f
	bsr	CRLF
@@:
	pea	s_name(pc)
	DOS	_PRINT
	addq.w	#4,sp
prt_emsg:
	move.l	line_number(pc),d1
	beq	@f

	pea	TAB(pc)
	DOS	_PRINT
	addq.w	#4,sp

	move.l	d1,d0
	bsr	num_to_str
	pea	suji(pc)
	DOS	_PRINT		*line number
	addq.w	#4,sp
@@:
	pea	TAB(pc)
	DOS	_PRINT
	addq.w	#4,sp

	DOS	_PRINT		*err message
	addq.w	#4,sp

	move.w	#$07,-(sp)
	clr.w	-(sp)
	DOS	_CONCTRL
	addq.w	#4,sp

	pea	d_name(pc)
	DOS	_DELETE
	addq.w	#4,sp

	move.w	#1,-(sp)
	DOS	_EXIT2
ropen_error:
	pea	rop_er_mes(pc)
	bra	bye_bye

wopen_error:
	pea	wop_er_mes(pc)
	bra	bye_bye_

syntax_error:
	pea	stx_er_mes(pc)
	bra	bye_bye

read_error0:
	pea	red_er_mes0(pc)
	bra	bye_bye

read_error:
	pea	red_er_mes(pc)
	bra	bye_bye

write_error:
	pea	wrt_er_mes(pc)
	bra	bye_bye_

dev_full:
	pea	dev_ful_mes(pc)
	bra	bye_bye

not_found:
	pea	not_er_mes(pc)
	bra	bye_bye

mem_error:
	pea	mem_er_mes(pc)
	bra	bye_bye

illegal_offst_err:
	pea	illegal_ofs_mes(pc)
	bra	bye_bye

illegal_cutsz_err:
	pea	illegal_cts_mes(pc)
	bra	bye_bye

illegal_outlvl_err:
	pea	illegal_otl_mes(pc)
	bra	bye_bye

illegal_p:
	pea	illegal_pf_mes(pc)
	bra	bye_bye

bank_ovf:
	pea	bank_ovf_mes(pc)
	bra	bye_bye

illegal_n:
	pea	illegal_n_mes(pc)
	bra	bye_bye

illegal_vol_err:
	pea	illegal_v_mes(pc)
	bra	bye_bye

illegal_pitch_err:
	pea	illegal_p_mes(pc)
	bra	bye_bye

illegal_delay_err:
	pea	illegal_d_mes(pc)
	bra	bye_bye

file_len_error:
	pea	fil_er_mes(pc)
	bra	bye_bye

no_dat_error:
	pea	ndt_er_mes(pc)
	bra	bye_bye

print_hlp:
	bsr	print_title

	move.w	#2,-(sp)
	pea	hlp_mes(pc)
	DOS	_FPUTS
	addq.w	#6,sp

	move.w	#1,-(sp)
	DOS	_EXIT2

	.data
work:
title_mes:
*	dc.b	$1b,'[32m・',$1b,'[35mpcnv.R '
	dc.b	$1b,'[36mΖ',$1b,'[35mpcnv.R '
	dc.b	$1b,'[37m',$f3,'V',$F3,'E',$F3,'R',$F3,'S',$F3,'I',$F3,'O',$F3,'N'
	dc.b	$f3,' ',$f3,'2',$f3,'.',$f3,'0',$f3,'4'
	dc.b	$f3,'d'
	dc.b	$1b,'[m (C) 1991,1992,1993,1994 '
	dc.b	$1b,'[36mZENJI SOFT',$1b,'[m',13,10,0
hlp_mes:
	dc.b	$1b,'[37m< USAGE > '
	dc.b	$1b,'[m ZPCNV.R  <FILENAME1[.CNF]> [FILENAME2]',13,10
	dc.b	$1b,'[37m< FUNCTION >',13,10,$1b,'[m'
	dc.b	' Convert ADPCM data list into a binary block',13,10 
	dc.b	"data for 'ZMUSIC.X'. FILENAME2 may be omitted.",13,10
	dc.b	'And in that case, FILENAME2 will be made from',13,10
	dc.b	'FILENAME1 by automatically.',13,10,0
rop_er_mes:
	dc.b	'File open error. Does the FILENAME1 surely exist?',13,10,0
wop_er_mes:
	dc.b	'File open error. Check FILENAME2.',13,10,0
stx_er_mes:
	dc.b	'Syntax error.',13,10,0
red_er_mes0:
	dc.b	'File read error. Check FILENAME1.',13,10,0
red_er_mes:
	dc.b	'is unable to read.',13,10,0
wrt_er_mes:
	dc.b	'File write error. Check FILENAME2.',13,10,0
dev_ful_mes:
	dc.b	'Device full.',13,10,0
mem_er_mes:
	dc.b	'Out of memory.',13,10,0
illegal_otl_mes:
	dc.b	'Illegal output level.',13,10,0
illegal_cts_mes:
	dc.b	'Cut size is too big.',13,10,0
illegal_ofs_mes:
	dc.b	'Illegal offset value.',13,10,0
illegal_pf_mes:
	dc.b	'Illegal parameter format.',13,10,0
bank_ovf_mes:
	dc.b	'Illegal Bank number.',13,10,0
illegal_n_mes:
	dc.b	'Illegal note number.',13,10,0
illegal_p_mes:
	dc.b	'Illegal pitch shift parameter.',13,10,0
illegal_v_mes:
	dc.b	'Illegal volume value.',13,10,0
illegal_d_mes:
	dc.b	'Illegal delay value.',13,10,0
fil_er_mes:
	dc.b	'Illegal File length.',13,10,0
ndt_er_mes:
	dc.b	'Referred to empty note number.',13,10,0
not_er_mes:
	dc.b	'not found.',13,10,0
saving:
	dc.b	'WRITING ',0
proc:
	dc.b	'PROCESSING',0
warn:
	dc.b	$1b,'[47mOVER WRITE',$1b,'[m … ',0
no_er_mes:	dc.b	$1b,'[mOperations are all set.',13,10
		dc.b	'A ',$1b,'[37m','♪SOUND',$1b,'[m mind in a '
		dc.b	$1b,'[37mSOUND',$1b,'[m',' body.',13,10,0
spc_data:	dc.b	' ',0
equ_data:	dc.b	'← ',0
ers_mes:	dc.b	'ERASE ',0
ptt_data:	dc.b	' … ',0
ok_data:	dc.b	'OK',13,10,0
crlf_data:	dc.b	13,10,0
TAB:		dc.b	9,0
kc_value:	*A  B  C  D  E  F  G
	dc.b	09,11,00,02,04,05,07
getname:	dc.b	'zmusic',0,0
display_cmt:	dc.b	0
	.even
line_number:	dc.l	0
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

adpcm_bank:	dc.w	0
	.bss
rv_p:		ds.b	1
read_or_not:	ds.b	1
up_down:	ds.b	1
hajimete:	ds.b	1
p16_or_not:	ds.b	1
	.even
env_bak:	ds.l	1
date_buf:	ds.l	1
work_adr:	ds.l	1
dest_adr:	ds.l	1
d2_work:	ds.l	1
adpcm_buffer_top:	ds.l	1
adpcm_buffer_next:	ds.l	1
dest_err:	ds.l	1
work_err:	ds.l	1
read_adr:	ds.l	1
total_size:	ds.l	1
cut_p:		ds.l	1
fade_p:		ds.l	1
mix_p:		ds.l	1
pitch_p:	ds.l	1
vol_p:		ds.l	1
last_val:	ds.w	1	*0
last_val2:	ds.w	1	*2
frq_wk:		ds.w	1	*4
frq_flg:	ds.w	1	*6
fnadr:		ds.l	1
sfh:		ds.w	1
filename:	ds.b	91
	.even
s_name:		ds.b	91
d_name:		ds.b	91
suji:		ds.b	64
open_fn:	ds.b	91
	.even
adpcm_ptr:	ds.l	adpcm_n_max*2
		ds.l	1024
user_sp:
