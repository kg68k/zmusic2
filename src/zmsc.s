*-------------------------------------------------------
*
*	      A ♪SOUND mind in a SOUND body
*
*			ＺＭＳＣ．Ｘ
*
*		 Programmed by Z.Nishikawa
*
*-------------------------------------------------------
*
*	参考文献	ソフトバンク	Ｘ６８ｋマシン語プログラミング/村田敏幸
*			シャープ	OPMDRV.X v1.01
*			SAN M.S.	MUSICDRV.X v1.00
*			T.ARIKUNI	IOCSの解析
*			N.Yuasa		Oh!X 1990.3 p38

	.include	doscall.mac
	.include	iocscall.mac
	.include	dma.mac
	.include	label.mac
	.include	macro.mac
	.list
	.text

tiny:		equ	1	*タイニー版 ZMUSIC.X

ch_max:		equ	25	*fm8+adpcm1+midi16=25
*pl_max:		equ	32	*一度に演奏可能なトラック数
tr_max:		equ	32	*確保出来るトラック数の最大値
tone_max:	equ	200	*音色番号の最大設定可能値
tempo_max:	equ	5000	*設定できるテンポの最大値
sp_max:		equ	4	*登録出来るサポートプログラムの総数
max_note_on:	equ	8	*一度にキー音出来るノート数
aftc_max:	equ	8	*アフタータッチシーケンス出来る最大ポイント数
modu_max:	equ	8	*モジュレーションシーケンスが出来る最大ポイント数
bank_max:	equ	64	*ＡＤＰＣＭの最大バンク数
adpcm_n_max_default:	equ	512
fm_addr_port:	equ	$e90001
fm_data_port:	equ	$e90003
at_least_wk:	equ	0	*ワークエリアのミニマムサイズ(通常)
dflt_trkbf:	equ	$8000	*デフォルトトラックバッファサイズ
dflt_pcmbf:	equ	$40000	*デフォルトＰＣＭバッファサイズ
stack:		equ	$4000
neiro_size:	equ	neiro_end_-neiro_
NOP:		equ	$4e71		*NOPの命令コード
NOP_NOP:	equ	$4e71_4e71	*NOPの命令コード
JMP:		equ	$4ef9		*JMPの命令コード
JSR:		equ	$4eb9		*JSRの命令コード
BRA:		equ	$6000		*BRA.wの命令コード
BSR:		equ	$6100		*BSR.wの命令コード
RTS:		equ	$4e75		*RTSの命令コード
RTE:		equ	$4e73		*RTEの命令コード
goto_ple:	equ	case_calc-loop_ope-2
goto_m_int_lp:	equ	m_int_lp-se_ope-2
wstep:		equ	'*'	*ワードステップタイム指定のコマンドヘッダ文字
mod_dflt:	equ	6	*ﾃﾞﾌｫﾙﾄのLFOｽﾋﾟｰﾄﾞ
arcc_dflt:	equ	11	*ﾃﾞﾌｫﾙﾄのARCC値
fo_dflt:	equ	16	*ﾃﾞﾌｫﾙﾄのﾌｪｰﾄﾞｱｳﾄｽﾋﾟｰﾄﾞ
fo_max:		equ	85	*ﾌｪｰﾄﾞｱｳﾄｽﾋﾟｰﾄﾞの上限
wv_max:		equ	24	*波形メモリの最大定義個数
wv_def_max:	equ	8	*デフォルト波形はいくつあるか

device_driver:
	.dc.l	dev_header1
	.dc.w	$8020		*char,ioctrl不可,raw
	.dc.l	strategy_entry
	.dc.l	interrupt_entry
dev_name:
	.dc.b	'OPM     '
pack_ptr:
	.dc.l	0
func_tbl:
	dc.w	dev_init-func_tbl	*0  初期化
	dc.w	not_com-func_tbl	*1  エラー
	dc.w	not_com-func_tbl	*2  無効
	dc.w	not_com-func_tbl	*3  (ioctrlによる入力)
	dc.w	not_com-func_tbl	*4  (入力)
	dc.w	ok_com-func_tbl		*5  １バイト先読み入力
	dc.w	ok_com-func_tbl		*6  入力ステータスチェック
	dc.w	ok_com-func_tbl		*7  入力バッファクリア
	dc.w	dev_out-func_tbl	*8  出力(verify off)
	dc.w	dev_out-func_tbl	*9  出力(verify on)
	dc.w	ok_com-func_tbl		*10 出力ステータスチェック
	dc.w	ok_com-func_tbl		*11 無効
	dc.w	not_com-func_tbl	*12 ioctrlによる出力

interrupt_entry:
	movem.l	d0/a4-a5,-(sp)
	movea.l	pack_ptr(pc),a5
	moveq.l	#0,d0			*d0.l=com code
	move.b	2(a5),d0
	add.w	d0,d0
	move.w	func_tbl(pc,d0.w),d0
	jsr	func_tbl(pc,d0.w)	*jump tableで参照したところへ…
	move.b	d0,3(a5)		*終了ステータスをセット
	lsr.w	#8,d0
	move.b	d0,4(a5)
	movem.l	(sp)+,d0/a4-a5
	rts

dev_header1:
	.dc.l	dev_header2
	.dc.w	$c020			*char,ioctrl可,raw
	.dc.l	strategy_entry1
	.dc.l	interrupt_entry1
dev_name1:
	.dc.b	'PCM     '
pack_ptr1:
	.dc.l	0
func_tbl1:
	dc.w	ok_com-func_tbl1	*0  初期化
	dc.w	not_com-func_tbl1	*1  エラー
	dc.w	not_com-func_tbl1	*2  無効
	dc.w	ioctrl_inp1-func_tbl1	*3  (ioctrlによる入力)
	dc.w	pcmdev_inp1-func_tbl1	*4  (入力)
	dc.w	ok_com-func_tbl1	*5  １バイト先読み入力
	dc.w	ok_com-func_tbl1	*6  入力ステータスチェック
	dc.w	ok_com-func_tbl1	*7  入力バッファクリア
	dc.w	pcmdev_out1-func_tbl1	*8  出力(verify off)
	dc.w	pcmdev_out1-func_tbl1	*9  出力(verify on)
	dc.w	ok_com-func_tbl1	*10 出力ステータスチェック
	dc.w	ok_com-func_tbl1	*11 無効
	dc.w	ioctrl_out1-func_tbl1	*12 ioctrlによる出力

interrupt_entry1:
	movem.l	d0/a4-a5,-(sp)
	movea.l	pack_ptr1(pc),a5
	moveq.l	#0,d0			*d0.l=com code
	move.b	2(a5),d0
	add.w	d0,d0
	move.w	func_tbl1(pc,d0.w),d0
	jsr	func_tbl1(pc,d0.w)	*jump tableで参照したところへ…
	move.b	d0,3(a5)		*終了ステータスをセット
	lsr.w	#8,d0
	move.b	d0,4(a5)
	movem.l	(sp)+,d0/a4-a5
	rts

dev_header2:
	.dc.l	-1
	.dc.w	$8000
	.dc.l	strategy_entry2
	.dc.l	interrupt_entry2
dev_name2:
	.dc.b	'MIDI    '
pack_ptr2:
	.dc.l	0
func_tbl2:
	dc.w	ok_com-func_tbl2	*0  初期化
	dc.w	not_com-func_tbl2	*1  エラー
	dc.w	not_com-func_tbl2	*2  無効
	dc.w	not_com-func_tbl2	*3  ioctrlによる入力
	dc.w	dev_inp-func_tbl2	*4　入力
	dc.w	dev_inp-func_tbl2	*5  1バイト先読み入力
	dc.w	ok_com-func_tbl2	*6  入力ステータスチェック
	dc.w	ok_com-func_tbl2	*7  入力バッファクリア
	dc.w	dev_out_2-func_tbl2	*8  出力(verify off)
	dc.w	dev_out_2-func_tbl2	*9  出力(verify on)
	dc.w	ok_com-func_tbl2	*10 出力ステータスチェック
	dc.w	ok_com-func_tbl2	*11 無効
	dc.w	not_com-func_tbl2	*12 ioctrlによる出力

interrupt_entry2:			*MIDI生データ通信処理
	movem.l	d0/a4-a5,-(sp)
	movea.l	pack_ptr2(pc),a5
	moveq.l	#0,d0			*d0.l=com code
	move.b	2(a5),d0
	add.w	d0,d0
	move.w	func_tbl2(pc,d0.w),d0
	jsr	func_tbl2(pc,d0.w)	*jump tableで参照したところへ…
	move.b	d0,3(a5)		*終了ステータスをセット
	lsr.w	#8,d0
	move.b	d0,4(a5)
	movem.l	(sp)+,d0/a4-a5
	rts

strategy_entry:
	move.l	a5,pack_ptr
	rts
strategy_entry1:
	move.l	a5,pack_ptr1
	rts
strategy_entry2:
	move.l	a5,pack_ptr2
	rts

not_com:
	move.w	#$5003,d0	*無視/中止,無効
	rts
ok_com:
	clr.w	d0		*no error
	rts

dev_out:	*ＭＭＬ　ＯＵＴ
	* グローバルレジスタ d4		a4 a6
	movem.l	d1-d7/a0-a6,-(sp)
	lea	-512(sp),sp
	lea	work(pc),a6
	clr.b	first_cmt-work(a6)
	movea.l	14(a5),a4	*get buff. add.
	move.l	18(a5),d4	*n bytes
	beq	dev_out_end	*case:data length is 0(リードエラー)
	cmpi.b	#$10,(a4)	*ヘッダチェック
	beq	exec_cmp	*コンパイルデータの実行
dev_out_end:			* < 通常モード
	moveq.l	#0,d0
	lea	512(sp),sp
	movem.l	(sp)+,d1-d7/a0-a6
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

skip_spc:			*スペースをスキップする
	subq.l	#1,d4
	bmi	@f
	cmpi.b	#' ',(a4)+
	beq	skip_spc
	cmpi.b	#09,-1(a4)	*skip tab
	beq	skip_spc
	subq.w	#1,a4
@@:
	addq.l	#1,d4
	rts

skip_plus:			*PLUSをスキップする
	subq.l	#1,d4
	bmi	@f
	cmpi.b	#'+',(a4)+
	beq	skip_plus
	subq.w	#1,a4
@@:
	addq.l	#1,d4
	rts

skip_sep:			*セパレータをスキップする(スペースやタブも)
	move.l	d0,-(sp)
skip_sep_lp:
	subq.l	#1,d4
	bmi	@f
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
@@:
	addq.l	#1,d4
	move.l	(sp)+,d0
	rts

get_fsize:			*ファイルサイズの偶数調整なし
	* < d5.w=file handle
	* > d3.l=data size
	* > mi=err
	* X d0
	* - d5
	move.w	#2,-(sp)	*ファィルの長さを調べる
	clr.l	-(sp)
	move.w	d5,-(sp)
	DOS	_SEEK
 	addq.w	#8,sp		*d0.l=file length
gf0:
	move.l	d0,d3		*d3=length
	beq	exit_adpcm_cnf
	bmi	exit_adpcm_cnf

	clr.w	-(sp)		*ファイルポインタを元に戻す
	clr.l	-(sp)
	move.w	d5,-(sp)
	DOS	_SEEK
	addq.w	#8,sp
	moveq.l	#0,d0
	rts

get_fsize2:			*ファイルサイズの偶数調整をする
	* < d5.w=file handle
	* > d3.l=data size
	* > mi=err
	* X d0
	* - d5
	move.w	#2,-(sp)	*ファィルの長さを調べる
	clr.l	-(sp)
	move.w	d5,-(sp)
	DOS	_SEEK
 	addq.w	#8,sp		*d0.l=file length
	addq.l	#1,d0
	bclr.l	#0,d0		*.even
	bra	gf0

exit_adpcm_cnf:			*case error
	bsr	do_fclose
	moveq.l	#-1,d0
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
rwff1:
	DOS	_V2_GETPDB
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

copy_fn:			*ファイルネームをバッファへ転送
	* < a0.l=destination address
	* < a4=address
	* < d4=counter
	* > d1.b=minus 拡張子がある
	* > d1.b=plus 拡張子はなかった
	* - all except d1,d4,a4
	movem.l	d0/d2/a0,-(sp)
	moveq.l	#0,d1
	moveq.l	#0,d2
copy_fnlp01:
	subq.l	#1,d4
	bmi	exit_cfn
	move.b	(a4)+,d0
	cmpi.b	#'.',d0
	bne	@f
	cmpi.b	#' ',(a4)
	bls	@f
	st.b	d1
@@:
	cmpi.b	#',',d0		*separater
	beq	exit_copyfn
	cmpi.b	#' ',d0		*ctrl code
	bls	exit_copyfn
	tst.b	d2
	beq	@f
	tst.b	-1(a0)
	bmi	cfst_lt
@@:
	bsr	mk_capital
cfst_lt:
	move.b	d0,(a0)+
	st.b	d2
	bra	copy_fnlp01
exit_copyfn:
	subq.w	#1,a4
exit_cfn:
	addq.l	#1,d4
	cmpi.b	#'.',-1(a0)
	bne	@f
	subq.w	#1,a0		*最後の'.'を潰す
@@:
	clr.b	(a0)
	movem.l	(sp)+,d0/d2/a0
	rts

fname_chk:
	* > eq=same
	* > mi=not same
	movem.l	d0-d1/a0-a1,-(sp)
	lea	filename(pc),a0
@@:
	moveq.l	#0,d1
	lea	last_fn(pc),a1
chk_fnlp:			*同じファイルを以前読んでないか
	move.b	(a0)+,d0
	tst.b	d1
	beq	@f
	tst.b	-2(a0)
	bmi	chkfn0
@@:
	bsr	mk_capital
chkfn0:
	st.b	d1
	cmp.b	(a1)+,d0
	bne	fn_diff
	tst.b	(a1)
	bne	chk_fnlp
				*同じ物は読まない
	moveq.l	#0,d0
	movem.l	(sp)+,d0-d1/a0-a1
	rts
fn_diff:
	moveq.l	#-1,d0
	movem.l	(sp)+,d0-d1/a0-a1
	rts

fn_to_lastfn:			*ファイルネーム保存
	movem.l	d0-d1/a1-a2,-(sp)
	lea	filename(pc),a1
@@:
	moveq.l	#0,d1
	lea	last_fn(pc),a2
ftl_lp:
	move.b	(a1)+,d0
	tst.b	d1
	beq	@f
	tst.b	-2(a1)
	bmi	ftl_lt
@@:
	bsr	mk_capital
ftl_lt:
	st.b	d1
	move.b	d0,(a2)+
	bne	ftl_lp
	movem.l	(sp)+,d0-d1/a1-a2
	rts

exec_cmp:			*コンパイルデータの実行
	addq.w	#1,a4		*skip '$10'
	subq.l	#7,d4
	bmi	dev_out_end
	cmpi.b	#'Z',(a4)+
	bne	match_err	*ヘッダの不一致
	cmpi.b	#'m',(a4)+
	bne	match_err
	cmpi.b	#'u',(a4)+
	bne	zmadpcm??
	cmpi.w	#$5369,(a4)+	*Si
	bne	match_err
	cmpi.b	#'C',(a4)+
	bne	match_err
	lea	mp_bak(pc),a1
	clr.l	(a1)+
	clr.l	(a1)+
	clr.l	(a1)+
	movea.l	a4,a1		*address
	move.l	d4,d2		*size
case_child:	nop		*自己出力時の矛盾を防止
	ZM	#$11
	bra	dev_out_end
zmadpcm??:			*ADPCMのリードか
	subq.l	#1,d4		*こっちのヘッダは8文字分
	bmi	dev_out_end
	subq.w	#1,a4		*一文字戻る
	cmpi.b	#'A',(a4)+
	bne	match_err
	cmpi.l	#$6470436d,(a4)+	*dpCm
	bne	match_err
	clr.b	last_fn-work(a6)
	movea.l	a4,a1		*source address
	move.l	d4,d2		*size
	addq.l	#1,d2
	bclr.l	#0,d2
	* < d2.l=total data size
	* < a1.l=data address(最初のtableから)
	cmp.l	adpcm_buffer_size(pc),d2
	bhi	match_err	*ADPCMバッファが小さすぎる
	moveq.l	#5,d1		*mode
	movea.l	adpcm_buffer_top(pc),a2
	lea	(a2,d2.l),a0
	move.l	a0,adpcm_buffer_next-work(a6)
	clr.b	adpb_clr-work(a6)
	bsr	trans_dma
	bsr	set_adpcm_tbl	*ブロックデータのインフォメーションをワークにセット
	bra	dev_out_end
match_err:			*ヘッダの不一致
	pea	dev_out_end(pc)
	bra	play_beep

pcmdev_inp1:				*PCMDRV.SYS処理
	movem.l	d1-d2/a1,-(sp)
	moveq.l	#$61,d0
	bra	@f
pcmdev_out1:
	movem.l	d1-d2/a1,-(sp)
	moveq.l	#0,d1
	IOCS	_ADPCMMOD
	moveq.l	#$60,d0
@@:
	move.l	$12(a5),d2		*length
	move.l	$0e(a5),a1		*address
	move.w	frqpan(pc),d1		*frq/pan
	trap	#$0f
	movem.l	(sp)+,d1-d2/a1
	bra	ok_com

ioctrl_inp1:
	IOCS	_ADPCMSNS
	movea.l	$0e(a5),a4
	move.b	d0,(a4)
	bra	ok_com

ioctrl_out1:
	movea.l	$0e(a5),a4
	move.b	(a4)+,d0
	lsl.w	#8,d0
	move.b	(a4)+,d0
	move.w	d0,frqpan
	bra	ok_com

frqpan:	.dc.w	$0403

dev_inp:			*MIDI生データ入力
				*nmdb!!
	tst.l	18(a5)		*check size
	beq	ok_com
	movem.l	d0-d4/a0/a4/a6,-(sp)
	lea	work(pc),a6
	movea.l	14(a5),a4
	move.l	18(a5),d4
	movea.l	rec_data_now(pc),a0
	move.l	a0,d2		*d2に意味無し
	bne	dvi_lp01
	moveq.l	#0,d2		*CONVERT ASCII MODE
	ZM	#$17		*QUIT FROM MIDI DATA RECORDING
dvi_lp01:
	cmpa.l	rec_data_end(pc),a0
	beq	dvi_exit
	move.b	(a0)+,(a4)+
	subq.l	#1,d4
	bne	dvi_lp01
do_dvi_exit:
	move.l	a0,rec_data_now-work(a6)
	movem.l	(sp)+,d0-d4/a0/a4/a6
	bra	ok_com
dvi_exit:
	move.b	#$1a,(a4)
	bra	do_dvi_exit

dev_out_2:			*MIDI生データの出力
	tst.l	18(a5)		*check size
	beq	ok_com
nmdb0:				*nmdb!!
	movem.l	d0-d1/d4/a0/a4/a6,-(sp)	*ここを変えたら-sの処理も変える
	lea	work(pc),a6
	move.l	14(a5),a4	*addr
	move.l	18(a5),d4	*size
do2_lp01:
	subq.l	#1,d4
	bmi	do2_exit
	move.b	(a4)+,d0
	bmi	mark_cmnt
	cmpi.b	#'/',d0
	beq	mark_cmnt
	cmpi.b	#'*',d0
	beq	mark_cmnt
	cmpi.b	#$0d,d0		*改行コード等をSKIP
	bls	come_to_an_end
	cmpi.b	#'.',d0		*SPCその他をスキップ
	bls	come_to_sep
	bsr	mk_capital
	move.b	d0,d1
	sub.b	#'0',d1
	bmi	do2_exit	*16進数データでないと判断
	cmpi.b	#9,d1
	bls	out_midi_data
	subq.b	#7,d1
	cmpi.b	#$0f,d1
	bhi	do2_exit	*16進数データではないと判断
out_midi_data:
	tst.b	cmnt-work(a6)
	bmi	do2_lp01
	move.b	data(pc),d0
	bne	do_out
	lsl.b	#4,d1
	ori.b	#1,d1		*marking
	move.b	d1,data-work(a6)
	bra	do2_lp01
do_out:
	andi.b	#$f0,d0		*erase marking
	or.b	d0,d1
	clr.b	data-work(a6)
	bsr	chk_out_midi	*多量のデータ転送が考えられるので
	bra	do2_lp01
do2_exit:
	movem.l	(sp)+,d0-d1/d4/a0/a4/a6
	bra	ok_com
come_to_sep:
	clr.b	data-work(a6)
	bra	do2_lp01
come_to_an_end:
	clr.b	cmnt-work(a6)
	clr.b	data-work(a6)
	bra	do2_lp01
mark_cmnt:
	st.b	cmnt-work(a6)
	clr.b	data-work(a6)
	bra	do2_lp01

com_max:	equ	(m_job_tbl_end-m_job_tbl)/2-1	*MAXコマンドナンバー

		dc.b	'TINY'
		dc.b	'ZmuSiC'	*ID
ver_num:	dc.b	v_code		*ZMUSIC VERSION NUMBER
		dc.b	v_code_+ver_type

Z_MUSIC_t3:	* trap #3でここに飛んでくる
	* < d1.b=command number
	* > d0,a0 (この仕様は変更出来ない)
	* - d1-d7,a1-a6
*	cmpi.b	#com_max,d1
*	bhi	error_miocs_t3
	movem.l	d1-d7/a1-a6,-(sp)
	lea	work(pc),a6
	move.w	$0034(a7),d0
	ori.w	#$2000,d0
	move.w	d0,sr_type-work(a6)	*SRを割り込み発生前に戻す時に使用
	ext.w	d1			*127以下ならこれでいい(andi.lより高速)
	add.w	d1,d1
	move.w	m_job_tbl(pc,d1.w),d1
	jsr	m_job_tbl(pc,d1.w)
	movem.l	(sp)+,d1-d7/a1-a6
	rte
*error_miocs_t3:
*	moveq.l	#-1,d0		*エラー
*	rte

*Z_MUSIC:	* IOCS #$f0でここに飛んでくる
*	* < d1.b=command number
*	* > d0(IOCS コールを用いた時はa0を戻り値として使用出来ず)
*	* - d1-d7,a1-a6
*	cmpi.b	#$5e,d1		*OPMDRV3.Xではないことを知らせる
*	beq	error_miocs
*	cmpi.b	#com_max,d1
*	bhi	error_miocs
*	movem.l	d1-d7/a1-a6,-(sp)
*	lea	work(pc),a6
*	move.w	#$2700,sr_type-work(a6)
*	ext.w	d1		*127以下ならこれでいい(andi.lより高速)
*	add.w	d1,d1
*	move.w	m_job_tbl(pc,d1.w),d1
*	jsr	m_job_tbl(pc,d1.w)
*	movem.l	(sp)+,d1-d7/a1-a6
*	rts
*error_miocs:
*	moveq.l	#-1,d0		*エラー
*	rts

m_job_tbl:
	dc.w	m_init-m_job_tbl		*$00
	dc.w	t_dat_ok-m_job_tbl		*m_alloc-m_job_tbl		*$01
	dc.w	t_dat_ok-m_job_tbl		*m_assign-m_job_tbl		*$02
	dc.w	t_dat_ok-m_job_tbl		*m_vget-m_job_tbl		*$03
	dc.w	m_vset-m_job_tbl		*$04
	dc.w	m_tempo-m_job_tbl		*$05
	dc.w	t_dat_ok-m_job_tbl		*m_trk-m_job_tbl		*$06
	dc.w	t_dat_ok-m_job_tbl		*m_free-m_job_tbl		*$07
	dc.w	m_play-m_job_tbl		*$08
	dc.w	m_stat-m_job_tbl		*$09
	dc.w	m_stop-m_job_tbl		*$0a
	dc.w	m_cont-m_job_tbl		*$0b
	dc.w	m_atoi-m_job_tbl		*$0c
	dc.w	init_all-m_job_tbl		*$0d
	dc.w	int_stop-m_job_tbl		*$0e
	dc.w	m_play2-m_job_tbl		*$0f
	dc.w	adpcm_read-m_job_tbl		*$10
	dc.w	play_cnv_data-m_job_tbl		*$11
	dc.w	se_play-m_job_tbl		*$12
	dc.w	se_adpcm1-m_job_tbl		*$13
	dc.w	se_adpcm2-m_job_tbl		*$14
	dc.w	set_ch_mode-m_job_tbl		*$15
	dc.w	midi_rec-m_job_tbl		*$16
	dc.w	midi_rec_end-m_job_tbl		*$17
	dc.w	midi_trns-m_job_tbl		*$18
	dc.w	calc_total-m_job_tbl		*$19
	dc.w	fade_out-m_job_tbl		*$1a
	dc.w	m_vset2-m_job_tbl		*$1b
	dc.w	send_rd_exc-m_job_tbl		*$1c
	dc.w	send_exc-m_job_tbl		*$1d
	dc.w	sc55_p_rsv-m_job_tbl		*$1e
	dc.w	sc55_reverb-m_job_tbl		*$1f
	dc.w	sc55_chorus-m_job_tbl		*$20
	dc.w	sc55_part_parameter-m_job_tbl	*$21
	dc.w	sc55_drum_parameter-m_job_tbl	*$22
	dc.w	sc55_print-m_job_tbl		*$23
	dc.w	sc55_display-m_job_tbl		*$24
	dc.w	mt32_p_rsv-m_job_tbl		*$25
	dc.w	mt32_reverb-m_job_tbl		*$26
	dc.w	mt32_setup-m_job_tbl		*$27
	dc.w	mt32_drum-m_job_tbl		*$28
	dc.w	mt32_common-m_job_tbl		*$29
	dc.w	mt32_partial-m_job_tbl		*$2a
	dc.w	mt32_patch-m_job_tbl		*$2b
	dc.w	mt32_print-m_job_tbl		*$2c
	dc.w	u220_setup-m_job_tbl		*$2d
	dc.w	u220_common-m_job_tbl		*$2e
	dc.w	u220_d_setup-m_job_tbl		*$2f
	dc.w	u220_p_setup-m_job_tbl		*$30
	dc.w	u220_print-m_job_tbl		*$31
	dc.w	u220_timbre-m_job_tbl		*$32
	dc.w	u220_drum-m_job_tbl		*$33
	dc.w	m1_midi_ch-m_job_tbl		*$34
	dc.w	send_to_m1-m_job_tbl		*$35
	dc.w	m1_p_setup-m_job_tbl		*$36
	dc.w	m1_e_setup-m_job_tbl		*$37
	dc.w	m1_print-m_job_tbl		*$38
	dc.w	adpcm_block_data-m_job_tbl	*$39
	dc.w	get_trk_tbl-m_job_tbl		*$3a
	dc.w	set_loop_time-m_job_tbl		*$3b
	dc.w	get_play_work-m_job_tbl		*$3c
	dc.w	get_tm_mode-m_job_tbl		*$3d
	dc.w	set_fm_master_vol-m_job_tbl	*$3e
	dc.w	set_timer_value-m_job_tbl	*$3f
	dc.w	release_support-m_job_tbl	*$40
	dc.w	t_dat_ok-m_job_tbl		*jump_active-m_job_tbl		*$41
	dc.w	set_mclk-m_job_tbl		*$42
	dc.w	picture_sync-m_job_tbl		*$43
	dc.w	mask_channels-m_job_tbl		*$44
	dc.w	buffer_info-m_job_tbl		*$45
	dc.w	set_zpd_tbl-m_job_tbl		*$46
	dc.w	set_output_level-m_job_tbl	*$47
	dc.w	eox_wait-m_job_tbl		*$48
	dc.w	set_wave_form1-m_job_tbl	*$49
	dc.w	set_wave_form2-m_job_tbl	*$4a
	dc.w	mask_tracks-m_job_tbl		*$4b
	dc.w	set_output_level2-m_job_tbl	*$4c
	dc.w	get_loop_time-m_job_tbl		*$4d
	dc.w	get_1st_comment-m_job_tbl	*$4e
	dc.w	int_start-m_job_tbl		*$4f
	dc.w	zm_status-m_job_tbl		*$50
	dc.w	sc55_init-m_job_tbl		*$51
	dc.w	mt32_init-m_job_tbl		*$52
	dc.w	t_dat_ok-m_job_tbl		*relative_uv-m_job_tbl		*$53
	dc.w	intercept_play-m_job_tbl	*$54
	dc.w	midi_inp1-m_job_tbl		*$55
	dc.w	midi_out1-m_job_tbl		*$56
	dc.w	occupied_size-m_job_tbl		*$57
	dc.w	call_int_play_ope-m_job_tbl	*$58	!2.03
m_job_tbl_end:

m_init:				*イニシャライズ
	*   cmd=$00

	move.w	sr,-(sp)				*念のため
	ori.w	#$700,sr
	bsr	m_stop_all				*演奏中ならまずそれを停止

*	move.l	#$0000_00_ff,adpcm_bank-work(a6)	*adpcm_bank(.w),err_exist_f(.b),fm_master(.b)
	move.l	#192,pcm8_ch-work(a6)			*pcm8_ch,ch_tr_msk,ch_tr_opl,mclk
	move.l	#78125,tmp_base-work(a6)
	move.l	timer_i_v(pc),m_tmp_buf-work(a6)	*m_tmp_buf(.w),timer_value(.w)
	move.l	adpcm_work_true_size(pc),adpcm_work_size-work(a6)
	move.l	adpcm_work_top(pc),adpcm_work_now-work(a6)
	st.b	adpb_clr-work(a6)
	move.w	#NOP,se_ope-work(a6)			*ALL ENABLEのケース


	bsr	init_play_trk_tbl	*演奏トラックテーブル初期化

	bsr	trk_top_set_

	bsr	tr_end_set		*dummyエンドコードセット
	bsr	top_ptr_set_all		*演奏ポインタを全て先頭へ
	bsr	init_play_wk		*演奏ワークの初期化
	bsr	init_cmn_wks		*共通ワーク初期化
	move.w	(sp)+,sr

	bsr	init_midi		*MIDI楽器の初期化
	moveq.l	#0,d2			*(b0)
	bsr	set_ch_mode

	bra	t_dat_ok

init_cmn_wks:			*PLAY等用
	move.w	#$0403,frq-work(a6)		*frq,pan
	moveq.l	#0,d0
	move.b	d0,noise_mode-work(a6)
	move.b	d0,se_mode-work(a6)
	move.b	d0,jump_flg2-work(a6)
	opmset	#15,d0
	rts

init_cmn_wks2	macro		*CONTINUE/STOP等用
	clr.b	se_mode-work(a6)
	endm

init_midi:			*MIDI楽器基本設定
	* - all
				*nmdb!!
	movem.l	d0-d1/d4/a0/a3-a4,-(sp)
	set_a3a4
	m_out_	#$ff		*初期設定
	moveq.l	#$b0,d1
	moveq.l	#16-1,d4
ilp1:
	lea	outtbl(pc),a0
	bsr	m_out_d1
@@:
	move.b	(a0)+,d0
	bmi	inmd0
	bsr	m_out_d0
	bsr	m_outa0
	bra	@b
inmd0:	nop			*-Jで"bsr.w f7_wait"が書き込まれる
	nop
	addq.b	#1,d1
	dbra	d4,ilp1
	movem.l	(sp)+,d0-d1/d4/a0/a3-a4
skip_im:
	rts

outtbl:
	dc.b	$79,0		*reset all controllers
	dc.b	$7c,0		*omni mode off
	dc.b	$7f,0		*poly mode on
	dc.b	$7a,$7f		*local on
	dc.b	$65,0		*RPN H
	dc.b	$64,1		*RPN L
	dc.b	$06,$40		*FINE TUNINGをニュートラルへ(H)
	dc.b	$26,0		*FINE TUNINGをニュートラルへ(L)
	dc.b	$65,0		*RPN H
	dc.b	$64,2		*RPN L
	dc.b	$06,$40		*COURSE TUNINGをニュートラルへ
	dc.w	-1

*m_vget:				*ボイスゲット
*	*   cmd=$03
*	* < d2.l=tone number
*	* < a1.l=address
*
*	subq.w	#1,d2
*	cmpi.l	#tone_max-1,d2
*	bhi	t_err_39
*
*	movea.l	neiro(pc),a0
*	mulu	#55,d2
*	adda.w	d2,a0
*	moveq.l	#55-1,d0
*vget_lp:
*	move.b	(a0)+,(a1)+
*	dbra	d0,vget_lp
*	bra	t_dat_ok

m_vset:				*ボイスセット
	*   cmd=$04
	* < d2.l=tone number
	* < a1.l=data address

	subq.w	#1,d2
	cmpi.l	#tone_max-1,d2
	bhi	t_err_39	*illegal tone number

	movea.l	neiro(pc),a0
	mulu	#55,d2
	adda.w	d2,a0
	moveq	#55-1,d0
m_vset_lp:
	move.b	(a1)+,(a0)+
	dbra	d0,m_vset_lp
	bra	t_dat_ok

m_tempo:			*テンポセット
	*   cmd=$05
	* < d2.l=tempo value(-1:で現在のテンポリータン)
	ori.w	#$0700,sr
	move.l	d2,d0
	bpl	@f
	moveq.l	#0,d0
	move.w	m_tmp_buf(pc),d0
	move.w	timer_value(pc),a0
	rts
@@:
	moveq.l	#20,d1
	cmp.l	d1,d0
	bcc	m_tempo1
	move.l	d1,d0
	bra	m_tempo2
m_tempo1:
	move.l	#tempo_max,d1
	cmp.l	d1,d0
	bls	m_tempo2
	move.l	d1,d0
m_tempo2:
	move.w	d0,m_tmp_buf-work(a6)
	bsr	calc_tm_b	*タイマーB
	bsr	init_timer
m_tempo_patch:			*nmdb!!
				*タイミングクロック自動送出設定
	if	(type<>3.and.type<>4)
	bsr	midi_clk
	endif
	bra	t_dat_ok

	if	(type<>3.and.type<>4)
midi_clk:			*タイミングクロック値セット
	move.w	sr,-(sp)
	ori.w	#$0700,sr
	pea	(a0)
	lea	rgr,a0
	bsr	do_calc_tmm
	move.b	#8,(a0)
				midiwait
	ori.w	#$8000,d1
	move.b	d1,grp6-rgr(a0)
				midiwait
	ror.w	#8,d1
	move.b	d1,grp7-rgr(a0)
				midiwait
	move.b	#5,(a0)		*つじつまをあわせる
				midiwait
	move.l	(sp)+,a0
	move.w	(sp)+,sr
	rts
	endif

*m_trk:
*	*   cmd=$06
*	* < d2.l=track number
*	* < a1=MML data address
*	* > a0=next address
*	bra	t_dat_ok

*m_free:
*	*   cmd=$07
*	* < d2.l=trk number
*	* > d0.l=rest free area
*	bra	t_dat_ok

m_play:				*演奏開始
	*    cmd=$08
	* < d2.l=bit pattern of enable tr numbers(1-32)
	* < d3.l=bit pattern of enable tr numbers(33-64)
	* < d4.w=bit pattern of enable tr numbers(65-80)
	*	=0 to play all

	clr.l	done_flg-work(a6)
	movem.l	d2-d4,mp_bak-work(a6)
	move.l	d2,d0
	or.w	d4,d0
	or.l	d3,d0
	beq	m_play_all
	ori.w	#$700,sr
	bsr	init_cmn_wks
	moveq.l	#0,d1
	moveq.l	#0,d5
m_play_lp:
	btst.l	d5,d2
	beq	mp_next		*case:bit=0
	lea	play_trk_tbl(pc),a0
@@:
	move.b	(a0)+,d0
	bmi	mp_next
	cmp.b	d0,d1
	bne	@b
	bsr	calc_wk
	bsr	top_ptr_set
	bsr	ms_key_off
	bsr	init_wks
	moveq.l	#-1,d0
	bsr	init_inst
	clr.b	p_not_empty(a5)	*alive
mp_next:
	addq.b	#1,d5
	cmpi.b	#31,d5
	bls	@f
	moveq.l	#0,d5
	move.l	d3,d2
	move.l	d4,d3
@@:
	addq.b	#1,d1
	cmpi.b	#tr_max,d1
	bne	m_play_lp
	bsr	init_timer
	bra	t_dat_ok

m_play_all:			*演奏可能なチャンネルを全部演奏
	bsr	stop_timer2	*割り込み期間以外を狙う
	bsr	all_key_off	*all key off
	bsr	init_cmn_wks

	lea	play_trk_tbl(pc),a0
	moveq.l	#tr_max-1,d1	*pl_max
mpa_lp:
	move.b	(a0)+,d0	*0-79
	bmi	mpa_end
	bsr	calc_wk
	bsr	top_ptr_set
	bsr	init_wks
	moveq.l	#-1,d0
	bsr	init_inst
	clr.b	p_not_empty(a5)
	dbra	d1,mpa_lp
mpa_end:
m_play00:				*func $54で書き変わる
	ori.w	#$0700,sr
	bsr.w	init_timer
m_play_patch:				*nmdb!!
	if	(type<>3.and.type<>4)
	bsr	midi_clk
	lea	rgr+6,a4	*=grp4
	move.b	#1,-6(a4)
					midiwait
	move.b	#%1111_1010,2(a4)	*midi start!($fa)
					midiwait
	move.b	#%0011_1011,(a4)	*trans $f8 start
					midiwait
	else
	set_a3a4
	m_out	#$fa
	endif
	bra	t_dat_ok

init_wks:				*ここではレジスタを壊してはいけない(except d0)
	* X d0.l
	move.l	#$0001_0001,(a5)	*一度に複数を初期化(p_on_count,p_gate_time)
	moveq.l	#0,d0
	cmpi.b	#8,p_ch(a5)
	bhi	iw1
	bcs	iw0
						*ADPCM
	move.l	#$00_ff_03_38,p_fo_mode(a5)	*一度に複数を初期化(p_fo_mode,p_pgm,p_pan,p_vol)
	bra	@f
iw0:						*FM
	move.l	#$00_ff_03_15,p_fo_mode(a5)	*一度に複数を初期化(p_fo_mode,p_pgm,p_pan,p_vol)
	bra	@f
iw1:						*MIDI
	move.l	#$00_ff_03_40,p_fo_mode(a5)	*一度に複数を初期化(p_fo_mode,p_pgm,p_pan,p_vol)
	move.w	d0,p_sp_tie(a5)
	move.l	#-1,p_bank_msb(a5)		*一度に複数を初期化(p_bank_msb,p_bank_lsb
						*		    p_effect1,p_effect3)
@@:
	move.l	#$40_00_04_7f,p_pan2(a5)	*p_pan2,p_non_off,p_frq,p_velo

	move.l	d0,p_rpt_cnt(a5)
	move.l	d0,p_rpt_cnt+4(a5)

	move.w	d0,p_pmod_work2(a5)
	move.l	d0,p_bend_sw(a5)	*一度に複数を初期化(p_bend_sw,p_aftc_flg
					*		    p_md_flg,p_waon_flg)
	move.l	d0,p_port_flg(a5)	*一度に複数を初期化(p_port_flg,p_bend_flg)
	move.l	d0,p_detune_f(a5)	*一度に複数を初期化(p_detune_f,p_detune_m)
*	move.l	d0,p_port_dly(a5)	*一度に複数を初期化(p_port_dly,p_bend_dly)
	move.l	d0,p_pmod_dly(a5)	*一度に複数を初期化(p_pmod_dly,p_arcc_dly)
	move.l	#$0100_0c00+arcc_dflt,p_sync_wk(a5)	*一度に複数を初期化
					*(p_sync_wk,p_rpt_last?,p_@b_range,p_arcc)
	move.l	#$00007f7f,p_pb_add(a5)	*一度に複数を初期化(p_bend_add,p_vset_flg
					*                   p_arcc_rst,p_arcc_def)
	move.l	d0,p_arcc_flg(a5)	*一度に複数を初期化(p_arcc_flg,p_aftc_sw,
					*		    p_dumper,p_tie_flg)
	move.l	d0,p_pmod_flg(a5)	*一度に複数を初期化(p_pmod_flg,p_pmod_sw,p_arcc_sw)
	move.l	d0,p_pmod_dpt(a5)	*一度に複数を初期化(p_pmod_dpt,p_seq_flag,p_do_loop_flag)
*	move.l	#$41101600,p_maker(a5)	*一度に複数を初期化(p_maker,p_device,p_module,*)
	move.l	d0,p_tie_pmod(a5)	*一度に複数を初期化(p_tie_pmod～p_tie_aftc)
	move.l	#mod_dflt*65536+mod_dflt,p_pmod_spd(a5)
					*一度に複数を初期化(p_pmod_spd,p_amod_spd)
	move.l	#$02_00_02_00,p_pmod_wf(a5)	*一度に複数を初期化(p_pmod_wf,p_amd_dpt
						*		    p_amod_wf,p_dmp_n)
	move.l	#$ff_000000,p_velo_dmy(a5)	*一度に複数を初期化(p_velo_dmy,p_waon_mark,p_marker)
	move.l	#$0000_ffff,p_pmod_omt(a5)	*一度に複数を初期化(p_pmod_omt,p_amod_omt,
						*		    p_pmod_mode,p_arcc_mode)
	moveq.l	#-1,d0
	move.l	d0,p_note(a5)
	move.l	d0,p_note+4(a5)
	cmpi.w	#NOP,se_ope-work(a6)	*m_solo,m_muteを使用中かどうか
	bne	@f
	move.b	d0,p_se_mode(a5)
@@:
	rts

init_wks2:				*ここではレジスタを壊してはいけない(except d0)
	* X d0.l
					*効果音モード時のワーク初期化
	move.l	#$0001_0001,(a4)	*一度に複数を初期化(p_on_count,p_gate_time)
	moveq.l	#0,d0
	move.b	d0,p_fo_mode(a4)	*init_wksとは違う点に注意

	move.b	d0,p_non_off(a4)	*init_wksとは違う点に注意
					*リピート関連のワークの初期化
	move.l	d0,p_rpt_cnt(a4)
	move.l	d0,p_rpt_cnt+4(a4)

*	move.w	d0,p_pmod_work2(a4)
	move.l	d0,p_bend_sw(a4)	*一度に複数を初期化(p_bend_sw,p_aftc_flg
					*		    p_md_flg,p_waon_flg)
	move.l	d0,p_port_flg(a4)	*一度に複数を初期化(p_port_flg,p_bend_flg)
*	move.l	d0,p_detune_f(a4)	*一度に複数を初期化(p_detune_f,p_detune_m)
*	move.l	d0,p_port_dly(a4)	*一度に複数を初期化(p_port_dly,p_bend_dly)
*	move.l	d0,p_pmod_dly(a4)	*一度に複数を初期化(p_pmod_dly,p_arcc_dly)
	move.b	d0,p_rpt_last?(a4)	*init_wksとは違う
	move.w	d0,p_pb_add(a4)		*init_wksとは違う一度に複数を初期化(p_bend_add,p_vset_flg)
	move.l	d0,p_arcc_flg(a4)	*一度に複数を初期化(p_arcc_flg,p_aftc_sw,
					*		    p_dumper,p_tie_flg)
	move.l	d0,p_pmod_flg(a4)	*一度に複数を初期化(p_pmod_flg,p_pmod_sw,p_arcc_sw)
	move.w	d0,p_seq_flag(a4)	*init_wksとは違う
					*一度に複数を初期化(p_seq_flag,p_do_loop_flag)
*	move.l	#$41101600,p_maker(a4)	*一度に複数を初期化(p_maker,p_device,p_module,*)
	move.l	d0,p_tie_pmod(a4)	*一度に複数を初期化(p_tie_pmod～p_tie_aftc)
*	move.l	#mod_dflt*65536+mod_dflt,p_pmod_spd(a4)
					*一度に複数を初期化(p_pmod_spd,p_amod_spd)
*	move.l	#$02_00_02_00,p_pmod_wf(a4)	*一度に複数を初期化(p_pmod_wf,p_amd_dpt
						*		    p_amod_wf,p_dmp_n)
	moveq.l	#-1,d0
	move.w	d0,p_pmod_mode(a4)	*init_wksとは違う点に注意(p_pmod_mode,p_arcc_mode)
	move.b	d0,p_marker(a4)		*init_wksとは違う点に注意(mark se track)
	move.w	d0,p_note(a4)
	move.b	d0,p_se_mode(a4)
	rts

m_stat:				*演奏状態の検査
	*   cmd=$09
	* < d2.l=ch number pattern
	* > d0.l=status

	move.l	d2,d5
	beq	m_stat_all
	bsr	rev_ch
	bsr	do_m_stat
	and.l	d5,d0
	beq	exit_m_stat
	moveq.l	#1,d0
exit_m_stat:
	rts
m_stat_all:			*値省略時
	moveq.l	#$ff,d2
do_m_stat:
	moveq.l	#32-1,d1	*PCM8考慮
	moveq.l	#0,d3
m_stat_lp:
	btst.l	d1,d2
	beq	mst_next
	lea	play_trk_tbl(pc),a0
mstlp:
	move.b	(a0)+,d0
	bmi	mst_next
	bsr	calc_wk
	tst.b	p_not_empty(a5)
	bne	mstlp
	cmpi.b	#24,d1
	bhi	mst_pcm8
	cmp.b	p_ch(a5),d1
	bne	mstlp
npc8_0:				*npc8!
	cmpi.b	#8,p_ch(a5)
	bne	mkp8
	tst.b	p_extra_ch(a5)
	bne	mstlp
mkp8:
	bset.l	d1,d3
	bra	mstlp
mst_pcm8:			*npc8!	*PCM8のケース
	cmpi.b	#8,p_ch(a5)
	bne	mstlp
	move.b	p_extra_ch(a5),d0
	beq	mstlp
	add.b	#24,d0
	bset.l	d0,d3
	bra	mstlp
mst_next:
	dbra	d1,m_stat_lp
mst_end:
	move.l	d3,d0
	tst.l	d2			*個別指定の時は既に考慮済み
	bpl	@f
	tst.b	real_ch_tbl-work(a6)	*ベースチャンネル考慮
	beq	@f
	moveq.l	#9,d1
	lsr.l	d1,d0
	andi.l	#$1ff,d3
	swap	d3
	or.l	d3,d0
@@:
	rts

rev_ch:			*チャンネルビットパターンをベースチャンネルを考慮したものに修正する
	* < d2.l=ch bit pattern
	* X d0-d1
	tst.b	real_ch_tbl-work(a6)
	beq	exit_rev_ch
	move.l	d2,d1
	andi.l	#$fe00_0000,d1
	swap	d2
	move.l	d2,d0
	clr.w	d0
	lsr.l	#7,d0
	andi.l	#$1ff,d2
	or.l	d0,d2
	or.l	d1,d2
exit_rev_ch:
	rts

m_stop:				*演奏停止
	*   cmd=$0a
	* < d2.l=bit pattern of enable tr numbers(1-32)
	* < d3.l=bit pattern of enable tr numbers(33-64)
	* < d4.w=bit pattern of enable tr numbers(65-80)

	move.l	d2,d0
	or.w	d4,d0
	or.l	d3,d0
	beq	m_stop_all
	moveq.l	#0,d1
	moveq.l	#0,d5
m_stop_lp:
	btst.l	d5,d2
	beq	ms_next
	lea	play_trk_tbl(pc),a0
@@:
	move.b	(a0)+,d0
	bmi	ms_next
	cmp.b	d0,d1
	bne	@b
	bsr	calc_wk
	move.b	p_not_empty(a5),d0
	bne	@f
	move.b	#$99,p_not_empty(a5)	*dead #1
	bsr	ms_key_off
	bra	ms_next
@@:
	cmpi.b	#$7f,d0
	bne	ms_next
	move.b	#$EE,p_not_empty(a5)	*dead #2
ms_next:
	addq.b	#1,d5
	cmpi.b	#31,d5
	bls	@f
	moveq.l	#0,d5
	move.l	d3,d2
	move.l	d4,d3
@@:
	addq.b	#1,d1
	cmpi.b	#tr_max,d1
	bne	m_stop_lp
	bra	t_dat_ok

m_stop_all:			*演奏中の全トラックを停止
	bsr	stop_timer2	*割り込み期間以外を狙う
	bsr	all_key_off	*all key off

	lea	play_trk_tbl(pc),a0
msa_lp:
	move.b	(a0)+,d0
	bmi	m_stop_patch
	bsr	calc_wk
	move.b	p_not_empty(a5),d0
	beq	@f
	cmpi.b	#$7f,d0
	bne	msa_lp
	move.b	#$EE,p_not_empty(a5)	*dead #2
	bra	msa_lp
@@:
	bsr	ms_key_off
	move.b	#$99,p_not_empty(a5)	*dead #1
	bra	msa_lp
m_stop_patch:				*nmdb!!
	if	(type<>3.and.type<>4)
	lea	rgr+6,a4	*=grp4
	ori.w	#$0700,sr
	move.b	#1,-6(a4)
					midiwait
	move.b	#%1111_1100,2(a4)	*midi stop($fc)
					midiwait
	move.b	#%0000_1011,(a4)	*trans $f8 stop
					midiwait
	else
	set_a3a4
	m_out	#$fc
	endif
	bra	t_dat_ok

ms_key_off:			*キーオフする
	movem.l	d0-d2/a1/a3-a4,-(sp)
nmdb1:					*nmdb!!
					*ダンパーを一時的にオフする動作
	set_a3a4
	move.b	p_ch(a5),d0
	sub.b	#9,d0
	bmi	msko0
	tst.b	p_dumper(a5)
	beq	msko0
	ori.b	#$b0,d0
	bsr	m_out_d0
	bsr	m_out64
	bsr	m_out0
msko0:
	move.b	p_dumper(a5),-(sp)
	move.b	p_se_mode(a5),-(sp)
	move.b	p_non_off(a5),-(sp)
	st.b	p_se_mode(a5)
	clr.b	p_dumper(a5)
	clr.b	p_non_off(a5)
	bsr	key_off
	move.b	(sp)+,p_non_off(a5)
	move.b	(sp)+,p_se_mode(a5)
	move.b	(sp)+,p_dumper(a5)
	movem.l	(sp)+,d0-d2/a1/a3-a4
	rts

m_cont:				*演奏再開
	*   cmd=$0b
	* < d2.l=bit pattern of enable tr numbers(1-32)
	* < d3.l=bit pattern of enable tr numbers(33-64)
	* < d4.w=bit pattern of enable tr numbers(65-80)

	clr.l	done_flg-work(a6)
	move.l	d2,d0
	or.w	d4,d0
	or.l	d3,d0
	beq	m_cont_all
	ori.w	#$700,sr
	init_cmn_wks2
	moveq.l	#0,d1
	moveq.l	#0,d5
m_cont_lp:
	btst.l	d5,d2
	beq	mc_next
	lea	play_trk_tbl(pc),a0
@@:
	move.b	(a0)+,d0		*active ?
	bmi	mc_next
	cmp.b	d0,d1
	bne	@b
	bsr	calc_wk
	move.b	p_not_empty(a5),d0
	cmpi.b	#$99,d0			*m_stopされたトラックか
	bne	@f
	clr.b	p_not_empty(a5)		*on
	clr.b	p_waon_mark(a5)
	move.b	p_pgm(a5),d0		*set last pgm
	bsr	cont_inst
	bra	mc_next
@@:
	cmpi.b	#$EE,d0
	bne	mc_next
	move.b	#$7f,p_not_empty(a5)
mc_next:
	addq.b	#1,d5
	cmpi.b	#31,d5
	bls	@f
	moveq.l	#0,d5
	move.l	d3,d2
	move.l	d4,d3
@@:
	addq.b	#1,d1
	cmpi.b	#tr_max,d1
	bne	m_cont_lp
	bsr	init_timer
	bra	t_dat_ok

m_cont_all:				*全チャンネル再開
	bsr	stop_timer2
	init_cmn_wks2
	lea	play_trk_tbl(pc),a0
	moveq.l	#0,d0
	moveq.l	#tr_max-1,d1	*pl_max
mca_lp:
	move.b	(a0)+,d0
	bmi	mca_next
	bsr	calc_wk
	move.b	p_not_empty(a5),d0
	cmpi.b	#$99,d0			*m_stopされたトラックか
	bne	@f
	clr.b	p_not_empty(a5)
	clr.b	p_waon_mark(a5)
	move.b	p_pgm(a5),d0		*set last pgm
	bsr	cont_inst
	bra	mca_next
@@:
	cmpi.b	#$EE,d0
	bne	mca_next
	move.b	#$7f,p_not_empty(a5)
mca_next:
	dbra	d1,mca_lp
	ori.w	#$0700,sr
	bsr	init_timer
m_cont_patch:				*nmdb!!
	if	(type<>3.and.type<>4)
	bsr	midi_clk
	lea	rgr+6,a4	*=grp4
	move.b	#1,-6(a4)
					midiwait
	move.b	#%1111_1011,2(a4)	*midi continue($fb)
					midiwait
	move.b	#%0011_1011,(a4)	*trans $f8 start
					midiwait
	else
	set_a3a4
	m_out	#$fb
	endif
	bra	t_dat_ok

cont_inst:			*FM音源の時だけ音色設定もする
	cmpi.b	#8,p_ch(a5)
	bcs	@f
	rts
@@:
	move.b	p_arcc(a5),-(sp)
	bsr	init_inst_
	move.b	(sp)+,p_arcc(a5)
	rts

m_atoi:				*トラックデータアドレスを返す
	*   cmd=$0c
	* < d2.l=track number
	* > d0.l=track address

	subq.w	#1,d2
	cmpi.l	#tr_max-1,d2
	bhi	t_err_2		*illegal trk number
	movea.l	trk_po_tbl(pc),a0
	add.w	d2,d2
	add.w	d2,d2
	move.l	(a0,d2.w),d0
	rts

wave_tbl_init:			*wave_tblの初期化
	* - all
	movem.l	d0-d1/a0-a1,-(sp)
	movea.l	wave_tbl(pc),a0
	lea	wv_dmy(pc),a1
	moveq.l	#wv_max-1,d0
	moveq.l	#-1,d1
@@:
	move.l	a1,(a0)+	*start address
	addq.w	#2,a1
	move.l	a1,(a0)+	*end address
	subq.w	#2,a1
	move.l	a1,(a0)+	*loop point
	move.w	d1,(a0)+	*loop mode
	dbra	d0,@b
	movem.l	(sp)+,d0-d1/a0-a1
	rts

adpcm_tbl_init:			*adpcm_tblの初期化
	* - all
	movem.l	d0-d1/a0,-(sp)
	move.l	adpcm_tbl(pc),d0
	beq	exit_ati
	move.l	d0,a0
	move.l	adpcm_n_max(pc),d0
	subq.w	#1,d0
	moveq.l	#0,d1
@@:
	move.l	d1,(a0)+	*init addr
	addq.w	#4,a0		*skip size
	dbra	d0,@b
exit_ati:
	movem.l	(sp)+,d0-d1/a0
	rts

	if	(type<>3.and.type<>4)
init_midibd:				*MIDIボードのイニシャライズ
					*nmdb!!
	move.w	sr,-(sp)
	ori.w	#$700,sr
	movem.l	d0-d1/a0-a6,-(sp)
	lea	rgr,a0
	move.b	#$80,(a0)	*initial reset
	moveq.l	#0,d0		*1/60/512=0.000032secのウエイト
	bsr	h_wait
	clr.b	(a0)		*intial reset end
	midiwait
	lea	grp4-rgr(a0),a4
	lea	2(a4),a5
	lea	2(a5),a6
	lea	2(a6),a3
	midi	#%10000000,#0,(a4)	*midi board initialize
	midi	#%00000000,#0,(a6)
	midi	#%00000010,#6,(a6)
	midi	#%11111111,#8,(a4)
	midi	#%11111111,#8,(a5)
	midi	#%11111111,#8,(a6)
	midi	#%11111111,#8,(a3)
	midi	#%00011000,#6,(a3)
	midi	#%10010100,#6,(a5)
	midi	#%10000101,#5,(a5)
	midi	#%00001000,#4,(a4)
	midi	#%10010000,#3,(a5)
	midi	#%00001000,#2,(a4)
	midi	#%00000000,#2,(a5)
	midi	#%00000010,#0,(a5)
	st.b	icr-rgr(a0)
					midiwait
	midi	#%00001011,#1,(a4)
	midi	#%00000000,#9,(a4)
	midi	#%11000001,#3,(a5)
	midi	#%10000001,#5,(a5)

	movem.l	(sp)+,d0-d1/a0-a6
	move.w	(sp)+,sr
exit_i_mdbd:
	rts

	elseif	type=3

init_midibd:				*MIDIボードのイニシャライズ(RS232C)
					*nmdb!!
	move.w	sr,-(sp)
	ori.w	#$700,sr
	movem.l	d0-d1/a0-a6,-(sp)
	lea	rs_data(pc),a0
	lea	$e98005,a1
	tst.b	(a1)			*dummy
	moveq.l	#(rs_data_e-rs_data)-1,d0
@@:
	move.b	(a0)+,(a1)
	dbra	d0,@b
	movem.l	(sp)+,d0-d1/a0-a6
	move.w	(sp)+,sr
exit_i_mdbd:
	rts

rs_data:	dc.b	$09,$80,$04,$44,$01,$00,$03,$00
		dc.b	$05,$00,$0b,$50,$0c
rs_data_clock:	dc.b	                    $03,$0d,$00
		dc.b	$0e,$02,$03,$c1,$05,$ea,$0e,$03
		dc.b	$10,$30,$38,$09,$09
rs_data_e:
	.even
	endif

	if	type=4
init_midibd:
	move.l	d0,-(sp)
	move.w	#$0301,d0
	trap	#2
	move.l	(sp)+,d0
exit_i_mdbd:
	rts
	endif

	if	type=3
	.include	rsmidi_mbrs.s
	.include	rsmidi_rate.s
	endif

int_stop:			*割り込みの停止(kill driver)
	*   cmd=$0e
	bsr	m_stop_all
	andi.b	#$f7,$00e88015	*MFP FM_int off
	bra	t_dat_ok

m_play2:			*前回のm_play()をもう一度やる
	*   cmd=$0f
	movem.l	mp_bak-work(a6),d2-d4
	bra	m_play

adpcm_read:			*ADPCMファイルを読み込む
	*   cmd=$10
	* < a1=filename address / (a1).l=source data number
	* < d2.l=note number(0～)
	* < d3.hw=pitch_parametr(0～$18) d3.lw=volume parameter(1～300)
	* < d4.hw=mixing offset d4.lw=mixing pcm number(0～)/d4.lw=-1:non mix
	* < d5.hw=cut offset d5.lw=cut size
	* < d6.b=reverse(=0:no,nz:yes)
	* < d7.hw=fade point d7.lwhb=mode(-1:in,+1:out) d7.lwlb=fade in/out level
	bsr	clr_adpb?
	tst.l	d3
	beq	@f
	tst.w	d3		*0%はダメ
	beq	t_err_67
	cmpi.w	#300,d3		*check parameter
	bhi	t_err_67
	swap	d3
	cmpi.w	#24,d3		*１オクターブ以上はダメ
	bhi	t_err_67
	swap	d3
@@:
	tst.w	d4
	bmi	@f
	cmp.w	adpcm_n_max+2(pc),d4	*MIXノート番号が異常
	bcc	t_err_67
@@:
	tst.l	d5
	beq	@f
*	tst.w	d5
*	beq	t_err_67	*CUT サイズが異常
@@:
	tst.l	d7
	beq	@f
	tst.b	d7
	bmi	t_err_67	*fade levelが異常
@@:
	tst.l	adpcm_buffer_size-work(a6)
	beq	t_err_61	*adpcm can't use mode

	clr.b	p16_or_not-work(a6)
	clr.b	read_or_not-work(a6)
	move.l	d4,mix_p-work(a6)
	move.l	d5,cut_p-work(a6)
	move.b	d6,rv_p-work(a6)
	move.l	d7,fade_p-work(a6)

	cmp.l	adpcm_n_max(pc),d2
	bcc	t_err_38	*note number is too big
	move.l	d2,d2_work-work(a6)

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
	movea.l	a1,a2
	bsr	fopen
	tst.l	d5
	bmi	t_err_62
*	clr.b	last_fn-work(a6)

	exg	d3,d4
	bsr	get_fsize
	bmi	t_err_62	*lengthがminusだったらエラー
	exg	d3,d4		*d4=length

	bsr	adpcm_buf_chk	*ADPCMの読み込みアドレスを求める(return:a1)
	beq	@f
	bpl	t_err_60	*adpcm buffer is not enough
	bmi	t_err_61	*adpcm is can not use
@@:
	move.w	sr,-(sp)
	andi.w	#$f8ff,sr
	move.l	d4,-(sp)	*push size
	pea	(a1)		*push addr
	move.w	d5,-(sp)	*file handle
	DOS	_READ		*サンプリングデータの読み込み
	lea	10(sp),sp
	move.w	(sp)+,sr
	tst.l	d0
	bmi	t_err_62	*read error

	bsr	do_fclose

	st	read_or_not-work(a6)
	bra	process?
non_read:			*ディスクからでなくてメモリから
	movea.l	adpcm_tbl(pc),a0
	moveq.l	#0,d0
	move.l	(a1)+,d0	*get note number
	lsl.l	#3,d0		*8倍
	adda.l	d0,a0
	move.l	4(a0),d4	*get size
	beq	t_dat_ok	*lengthが０なのでコピー不可

	bsr	adpcm_buf_chk	*ADPCMの読み込みアドレスを求める(return:a1)
	beq	do_copy_data
	bpl	t_err_60	*adpcm buffer is not enough
	bmi	t_err_61	*adpcm is can not use
do_copy_data:
	movea.l	a1,a2		*destination
	movea.l	(a0),a1		*source
	moveq.l	#5,d1
	move.l	d4,d2
	bsr	trans_dma
	move.l	a2,a1		*data exist adr
process?:			*加工処理
	* < a1=data address
	* < d4.l=size
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
	cmp.l	adpcm_work_size(pc),d1
	bhi	t_err_64			*ワークエリアが小さすぎる
	sub.l	d4,adpcm_buffer_next-work(a6)	*読みこんだデータは無かった物とする

	movea.l	a1,a0		*source
	movea.l	adpcm_work_now(pc),a1	*destination
	move.l	d4,d0		*size
	moveq.l	#0,d1
	move.w	d3,d1		*d1=pitch shift
	swap	d3
	moveq.l	#0,d6
	move.w	d3,d6		*d6=volume %
	tst.b	p16_or_not-work(a6)
	beq	@f
	bsr	transform_pcm
	bra	gtpcsz
@@:
	bsr	adpcm_to_pcm	*a1=PCM data size
gtpcsz:
	move.l	a1,d4
truncate?:
	lea	cut_p(pc),a0
	tst.l	(a0)
	beq	reverse?
	move.l	adpcm_work_now(pc),a2
	moveq.l	#0,d0
	move.w	(a0)+,d0	*get offset
	lsl.l	#2,d0
	cmp.l	d4,d0
	bcc	t_err_67	*offsetが大きすぎ
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
	bhi	t_err_67	*カットサイズが大きすぎる
do_truncate:
	bsr	trans_dma
	move.l	d2,d4		*PCM size
reverse?:			*リバース??
	tst.b	rv_p-work(a6)
	beq	fade_in_out?
	move.l	adpcm_work_now(pc),a2
	add.l	d4,a2
	lea	(a2,d4.l),a0
	cmp.l	adpcm_work_end(pc),a0
	bcc	t_err_64	*work too small
	pea	(a2)
	move.l	adpcm_work_now(pc),a1	*source
	add.l	d4,a1
	move.l	d4,d0
	lsr.l	d0		*d0.l=adpcm count
@@:
	move.w	-(a1),(a2)+
	subq.l	#1,d0
	bne	@b
	move.l	(sp)+,a1
	move.l	adpcm_work_now(pc),a2
	moveq.l	#5,d1		*dma mode
	move.l	d4,d2		*size
	bsr	trans_dma
fade_in_out?:
	lea	fade_p(pc),a0
	tst.l	(a0)
	beq	mix?
	move.l	adpcm_work_now(pc),a1
	moveq.l	#0,d0
	move.w	(a0)+,d0
	lsl.l	#2,d0
	cmp.l	d4,d0
	bcc	t_err_67	*オフセット値が大きすぎる
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
	move.l	adpcm_work_now(pc),a0
	move.l	d4,a1
	lsr.l	#2,d4		*d4.l=adpcm size
	moveq.l	#0,d0
	move.w	mix_p+2(pc),d0	*d0=mix note number
	bmi	get_save_adr	*non mix case
case_mix:
	* < a1=source PCM data size
	* < d4=src adpcm data size	*MIXING
	lsl.l	#3,d0		*8倍
	movea.l	adpcm_tbl(pc),a3
	add.l	d0,a3		*a3=mix destination parameter pointer
	tst.l	(a3)
	beq	t_err_67	*空っぽの時
	move.l	adpcm_work_end(pc),d1
	move.l	a1,d2
	add.l	adpcm_work_now(pc),a1	*destination
	sub.l	a1,d1		*d1=free area
	cmp.l	d1,d2
	bhi	t_err_64	*work is not enough
	move.l	4(a3),d2	*出来上がるデータのサイズはsrcまたはdestの
	moveq.l	#0,d0		*どちらかなので両方チェック
	move.w	mix_p(pc),d0
	move.l	d0,mix_p-work(a6)
	add.l	d0,d2
	lsl.l	#2,d2
	cmp.l	d1,d2
	bhi	t_err_64	*work is not enough

	move.l	a1,a2
	move.l	mix_p(pc),d0	*ディレイカウント分０で埋める
	beq	cnv_dst
	add.l	d0,d0
@@:
	clr.w	(a1)+
	subq.l	#1,d0		*for dbra
	bne	@b
cnv_dst:
	move.l	(a3),a0		*a0=adpcm data buffer
	move.l	4(a3),d0	*d0=destination adpcm data size
	movem.l	d4/a2/a3,-(sp)
	bsr	just_adpcm_to_pcm
	movem.l	(sp)+,d4/a1/a3	*わざとa1
	move.l	adpcm_work_now(pc),a0	*src data address
	move.l	4(a3),d0
	add.l	mix_p(pc),d0
	cmp.l	d0,d4		*dest,srcどっちが大きい?
	bhi	src_big_case
	*srcが小さいケース
	add.l	d4,d4		*d4=pcm data cnt
	move.l	a1,a2
mix_lp01:
	move.w	(a0)+,d0
	add.w	d0,(a2)+
	subq.l	#1,d4
	bne	mix_lp01
	move.l	4(a3),d4	*size=dest
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
	* < d4.l=size
	* < a0.l=data address
	bsr	adpcm_buf_chk	*a1=read adr
	beq	mk_adpcm
	bpl	t_err_60	*adpcm buffer is not enough
	bmi	t_err_61	*adpcm is can not use
mk_adpcm:
	* < d4.l=data size
	* < a1.l=save address
	* < a0.l=PCM data address
	movem.l	d4/a1,-(sp)
	exg.l	a1,a0
	move.l	d4,d0
	bsr	pcm_to_adpcm	*a1->a0
	movem.l	(sp)+,d4/a1
exit_smp_read:			*パラメータを書き込んで帰還
	* < a1=data adr
	* < d4.l=data size
	movea.l	adpcm_tbl(pc),a0
	move.l	d2_work(pc),d2	*get back note number
	lsl.l	#3,d2		*8倍
	lea.l	4(a0,d2.l),a0
	move.l	d4,(a0)		*set size (わざと逆にして変な音がなるのを防ぐ)
	move.l	a1,-(a0)	*set address
	bra	t_dat_ok
adpcm_buf_chk:
	* < d4.l=data size
	* > a1=読み込みアドレス
	* eq=no error
	* mi=error
	* X d0,d1 a2
	tst.l	adpcm_buffer_size-work(a6)
	beq	cant_abc		*バッファ未確保(can't use adpcm)
	move.l	adpcm_buffer_next(pc),d0
	movea.l	d0,a1
	add.l	d4,d0			*データを読み込むと
	move.l	d0,adpcm_buffer_next-work(a6)
	cmp.l	adpcm_buffer_end(pc),d0	*最終アドレスをオーバーするか
	bls	chk_abc			*ok
	bsr	play_beep		*ダメだった
	move.l	adpcm_buffer_top(pc),d0
	movea.l	d0,a1
	add.l	d4,d0
	cmp.l	adpcm_buffer_end(pc),d0	*読み込むデータがバッファないに収まるのか
	bhi	error_abc		*FILE is too big
	move.l	d0,adpcm_buffer_next-work(a6)
chk_abc:
	move.l	adpcm_n_max(pc),d1
	subq.l	#1,d1
	move.l	adpcm_tbl(pc),a2
abc_lp:
	move.l	(a2),d0
	beq	next_abc
	cmp.l	adpcm_buffer_next(pc),d0
	bcc	next_abc
	add.l	4(a2),d0	*d0=end adr
	cmp.l	a1,d0
	bls	next_abc
	clr.l	(a2)		*disable tbl
	clr.l	4(a2)		*disable size
next_abc:
	addq.w	#8,a2
	dbra	d1,abc_lp
	moveq.l	#0,d0
	rts
cant_abc:
	moveq.l	#1,d0
	rts
error_abc:
	sub.l	d4,adpcm_buffer_next-work(a6)
	moveq.l	#-1,d0
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
	move.w	d3,(a3)
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

	move.l	a1,-(sp)
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
	add.b	d7,d7
	move.w	(a5,d7.w),d3	*=d
	lsr.b	d7

	moveq.l	#0,d2		*=def

	btst	#2,d1
	beq	tst_bit1
	move.w	d3,d2
tst_bit1:
	lsr.w	d3
	btst	#1,d1
	beq	tst_bit0
	add.w	d3,d2
tst_bit0:
	lsr.w	d3
	btst	#0,d1
	beq	decide_def
	add.w	d3,d2
decide_def:
	lsr.w	d3
	add.w	d3,d2

	btst	#3,d1		*minus or plus??
	beq	plus_lastval
	neg.w	d2		*case not zero
plus_lastval:
	add.w	(a3),d2		*last_val
*	bsr	chk_ovf
	move.w	d2,(a3)		*d2=pcmdata

	add.b	d1,d1
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
	* X d0-d5/a1,a2,a3,a5
	pea	(a6)

	lea	scaleval(pc),a5
	lea	levelchg(pc),a6

	moveq.l	#0,d6		*scalelevel=0
	moveq.l	#0,d7
	moveq.l	#%1010_1010,d4
	add.l	d0,d0
	lea	last_val(pc),a3
	move.w	d6,(a3)
pta_lp:
	move.w	(a1)+,d3	*d3=pcm data
	bsr	calc_adpcm_val
	ror.b	d4
	bcc	set_lower
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
	move.l	(sp)+,a6
	rts

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
	lsr.b	d1
	exg	d7,d6
	bsr	calc_pcm_val
	exg	d6,d7
	lsr.b	d1
	rts

play_cnv_data:			*コンパイルデータの演奏
	*   cmd=$11
	* < d2.l=total data size d2がゼロならバッファへは転送せず即演奏
	* < a1=play data address (a1)=ver num,1(a1)=data...
	move.b	(a1)+,d0
	cmp.b	ver_num(pc),d0
	bhi	t_err_66	*バージョン番号不一致
	movem.l	d2/a1,-(sp)
	bsr	stop_timer2	*割り込み期間以外を狙う
	bsr	all_key_off2

	move.l	#192,pcm8_ch-work(a6)		*pcm8_ch,ch_tr_msk,ch_tr_opl,mclk
	move.l	#78125,tmp_base-work(a6)
	move.l	timer_i_v(pc),m_tmp_buf-work(a6)	*m_tmp_buf(.w),timer_value(.w)
	move.l	adpcm_work_true_size(pc),adpcm_work_size-work(a6)
	move.l	adpcm_work_top(pc),adpcm_work_now-work(a6)
	st.b	adpb_clr-work(a6)
	move.w	#NOP,se_ope-work(a6)		*ALL ENABLEのケース
	move.l	#$0403_0000,frq-work(a6)	*frq,pan,noise_mode,first_cmt
						*noise mode off(レジスタ書き込みは
						*必要であればinit_instで行なわれる)
	bsr	init_midi			*MIDIデバイス初期化
	movem.l	(sp)+,d2/a2	*d2=size/a2=address(swaping)
	move.l	d2,zmd_size-work(a6)
	beq	do_comn_cmd
	cmp.l	trk_buf_size(pc),d2
	bhi	t_err_5		*track buffer is too small
	movea.l	trk_top(pc),a1
	moveq.l	#%1000_0101,d1	*dma mode
	bsr	trans_dma	*trans (a2)->(a1)
	lea	mp_bak(pc),a2
	moveq.l	#0,d0
	move.l	d0,(a2)+
	move.l	d0,(a2)+
	move.l	d0,(a2)+
	movea.l	a1,a2
do_comn_cmd:
	tst.b	(a2)
	bpl	@f
	addq.w	#2,a2
	bra	dcc
@@:
	bsr	comn_cmd	*共通コマンド実行
				*set information block & play
	move.l	a2,d0
	addq.l	#1,d0
	bclr.l	#0,d0		*.even
	move.l	d0,a2
dcc:
	move.w	(a2)+,d1	*num of trks
	subq.w	#1,d1
	bmi	t_dat_ok	*no play data
				*各トラックの先頭アドレスをワークへセット
	movea.l	trk_po_tbl(pc),a0
	movea.l	seq_wk_tbl(pc),a5
	moveq.l	#0,d3
	move.l	d3,done_flg-work(a6)
*	move.w	se_ope(pc),d7
*	move.w	d3,se_ope-work(a6)	*p_se_mode(a5)を保存するため
set_i_lp01:
	move.b	d3,(a3)+	*play track number
	move.l	(a2)+,d2	*offset
	lea	(a2,d2.l),a4
	move.w	(a2)+,d2	*ch
	cmpi.b	#8,d2
	bcs	pcd_getwk
	beq	inc_pcmch
	cmpi.b	#24,d2
	bhi	its_pcm8ch
nmdb2:					*nmdb!!
	nop
	nop
pcd_getwk:
	move.b	d2,p_ch(a5)
	bsr	init_wks
	move.l	a4,p_data_pointer(a5)	*same as 'bsr top_ptr_set'
	move.l	a4,(a0)+		*set address
	clr.b	p_not_empty(a5)
	moveq.l	#-1,d0
	bsr	init_inst
	addq.w	#1,d3
	lea	wk_size(a5),a5
	dbra	d1,set_i_lp01
	move.b	d1,(a3)			*set end code/write 255
*	move.w	d7,se_ope-work(a6)	*「騙し」解除
	bra	m_play00
its_pcm8ch:
	sub.b	#24,d2
	move.b	d2,pcm8_ch-work(a6)
	moveq.l	#8,d2
	bra	@f
inc_pcmch:			*npc8!	*case:PCM8 mode
	tst.w	d2
	bpl	@f
	clr.b	pcm8_ch-work(a6)
@@:
	move.b	pcm8_ch(pc),p_extra_ch(a5)
	addq.b	#1,pcm8_ch-work(a6)
	bra	pcd_getwk
_pex0:
	clr.b	p_extra_ch(a5)
	bra	pcd_getwk
pcd_err:
	pea	t_err_68(pc)
	bra	play_beep

all_key_off2:			*seチャンネル以外の全チャンネルをノートオフする
	* x d0-d5/a2-a4
	* > a3.l=play_trk_tbl+x
	lea	play_trk_tbl(pc),a2
	move.l	a2,a1
nmdb3:					*nmdb!!
	set_a3a4
ako2_lp:
	move.b	(a2)+,d3
	bmi	end_ako2
	move.b	d3,d0
	lsl.w	#8,d0
	move.l	seq_wk_tbl(pc),a5
	adda.w	d0,a5
	tst.b	p_not_empty(a5)		*play_end,dead?
	bne	ako2_next
	tst.b	p_marker(a5)		*効果音として新たにアサインされたトラック?
	beq	@f
	move.b	d3,(a1)+		*play_trk_tblを新設定
	bra	ako2_lp
@@:
	tst.b	p_se_mode(a5)		*効果音を演奏中?(or masking)
	bpl	ako2_lp
	move.b	p_ch(a5),d4
	bsr	ako_sub
ako2_next:
	st.b	p_se_mode(a5)
	st.b	p_not_empty(a5)
	bra	ako2_lp
end_ako2:
	move.l	a1,a3
	rts

ako_sub:				*実際に音を止める
	* < a3.l,a4.l=midi param.
	* X d0-d2
	* - d4
	cmpi.b	#8,d4
	bcs	ako_fm
	beq	ako_adpcm
nmdb4:					*nmdb!!
ako_midi:
	move.b	d4,d1
	add.b	#$b0-9,d1
	bsr	m_out_d1
	bsr	m_out64
	bsr	m_out0				*dumper off
*	bsr	m_out_d1
	m_out_	#$7b
	bra	m_out0				*all notes off
ako_fm:
	moveq.l	#8,d1
	move.l	d4,d2
	bsr	opmset			*=fmkey_off	d4
	move.l	d4,d1
	add.b	#$60,d1
	moveq.l	#127,d2			*v=0
	bsr	opmset			*TL1
	addq.w	#8,d1
	bsr	opmset			*TL3
	addq.w	#8,d1
	bsr	opmset			*TL2
	addq.w	#8,d1
	bra	opmset			*TL4
ako_adpcm:
	tst.b	se_mode-work(a6)
	bmi	ako_sub_exit
npc8_1:				*npc8!
	move.w	#$0101,d0
	trap	#2		*ADPCM OFF
ako_sub_exit:
	rts
adof:
	moveq.l	#0,d1
	IOCS	_ADPCMMOD	*ADPCM OFF
	rts

se_play:			*効果音モードプレイ
	*   cmd=$12		高速処理が要求されるのでﾊﾟﾗﾒｰﾀﾁｪｯｸは無し
	* < a1.l=compiled data address (a1).w=total of play trks...
	* < d2.l=何番トラックから割り込ませるか(1-32)
	move.w	(a1)+,d1	*num of trks
	subq.w	#1,d1		*make n into 0-31 for dbra counter
				*各トラックの先頭アドレスをワークへセット
	move.l	d2,d0		*d0=which track to play
	subq.w	#1,d0		*d0=0-31
	move.l	d0,d3		*d3=汎用
	move.l	d0,d5
	lsl.w	#wk_size2,d3
	move.l	seq_wk_tbl(pc),a5
	move.l	seq_wk_tbl2(pc),a4
	adda.w	d3,a5
	adda.w	d3,a4
	lea	play_trk_tbl(pc),a3
	ori.w	#$700,sr
	andi.b	#$f7,$00e88015	*MFP FM_int off
	move.w	sr_type(pc),sr	*INT enable
sepl_lp01:
	moveq.l	#0,d0
	move.l	(a1)+,d3	*offset
	lea	(a1,d3.l),a2
	move.w	(a1)+,d3	*ch
	cmpi.b	#7,d3
	bcs	sepl1
	bhi	@f
	opmset	#15,d0
sepl1:
	opmset	#8,d3		*=fmkey_off	d3
@@:
	tst.b	p_se_mode(a5)
	beq	@f
	move.b	p_not_empty(a5),p_ne_buff(a4)
	bmi	case_dead			*0 or 1はＯＫ,-1は全くそのトラックが死んでる時
	move.b	d0,p_se_mode(a5)		*se mode on
@@:
	move.b	d0,p_not_empty(a5)		*on
	move.b	d0,p_not_empty(a4)		*on
	move.b	d3,p_ch(a4)
	move.l	a2,p_data_pointer(a4)		*set address
	bsr	init_wks2			*効果音モード用演奏ワーク初期化
se_ope_next:
	lea	wk_size(a5),a5
	lea	wk_size(a4),a4
	addq.w	#1,d5
	dbra	d1,sepl_lp01
	ori.b	#$08,$00e88015	*MFP FM_int ON
	rts

case_dead:			*そのトラックが全く死んでいる場合
@@:
	tst.b	(a3)+
	bpl	@b
	move.b	d5,-1(a3)		*set play trk tbl
	st.b	(a3)			*end mark
	exg.l	a4,a5
	move.b	d0,p_not_empty(a4)	*make it alive
	move.b	d3,p_ch(a4)
	move.l	a2,p_data_pointer(a4)	*set address
	bsr	init_wks2
	exg.l	a4,a5
	bra	se_ope_next

comn_cmd:			*共通コマンド群の処理(vsetやtempo等)
				*ここを変えたら、ZP.Xの'get_sakiyomi_name'も変える)
	* < a2=comn data addr
	* X d0-d2,a0-a2
pcd_lp01:
	move.b	(a2)+,d1
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
	beq	single_adpcm	*adpcm data cnf
	cmpi.b	#$4a,d1
	beq	cnv_wvf		*wave form setting
	cmpi.b	#$60,d1
	beq	cnv_rd_cnf	*#com0:read opmd.cnf
	cmpi.b	#$61,d1
	beq	cnv_print	*#com2:print message
	cmpi.b	#$62,d1
	beq	cnv_midi	*#com22:trans midi data dump
	cmpi.b	#$63,d1
	beq	cnv_abdt	*#com35:read adpcm block data
	cmpi.b	#$7e,d1
	beq	pcd_lp01	*dummy code
	cmpi.b	#$7f,d1
	beq	cnv_cmt		*#com36:comment
	bra	t_err_65	*unknown command
exit_ccd:
	rts
				*コンパイルデータの実行部
cnv_vset:
cnv_vset2:
	moveq.l	#0,d2
	move.b	(a2)+,d2
	movea.l	a2,a1
	trap	#3
	lea	55(a2),a2
	bra	pcd_lp01

*cnv_vset2:
*	moveq.l	#0,d2
*	move.b	(a2)+,d2
*	movea.l	a2,a1
*	trap	#3
*	lea	55(a2),a2
*	bra	pcd_lp01

cnv_tempo:
	lea	m_tmp_buf(pc),a1
	move.b	(a2)+,(a1)+
	move.b	(a2)+,(a1)+
	bsr	calc_tm_b
	bra	pcd_lp01

cnv_mclk:
	move.b	(a2)+,mclk-work(a6)
	lea	tmp_base(pc),a1
	move.b	(a2)+,(a1)+
	move.b	(a2)+,(a1)+
	move.b	(a2)+,(a1)+
	move.b	(a2)+,(a1)+
	bra	pcd_lp01

cnv_scm:			*チャンネルモード切り換え
	moveq.l	#0,d2
	move.b	(a2)+,d2
	trap	#3
	bra	pcd_lp01

cnv_mdtrns:
	moveq.l	#0,d2
	bsr	get_cm_w
	move.w	d0,d2		*n bytes
	movea.l	a2,a1
	trap	#3
	add.l	d2,a2
	bra	pcd_lp01

single_adpcm:
	moveq.l	#0,d2
	bsr	get_cm_w
	move.w	d0,d2		*d2.l=note

	bsr	get_cm_l
	move.l	d0,d3		*get pitch & get volume

	bsr	get_cm_l	*get mix note & get mix delay
	move.l	d0,d4

	bsr	get_cm_l	*get truncate
	move.l	d0,d5

	move.b	(a2)+,d6	*get reverse

	bsr	get_cm_l	*get fade in/out
	move.l	d0,d7

	movea.l	a2,a1
	ZM	#$10
	tst.b	(a2)
	bne	srch_fn_end
	addq.w	#4,a2		*直接値の場合
	bra	pcd_lp01
srch_fn_end:			*ファイル名の場合
	tst.b	(a2)+
	bne	srch_fn_end	*ファイルネームのエンドを捜して
	bra	pcd_lp01	*ループに戻る

cnv_cmt:
	lea	first_cmt(pc),a0
	tst.b	(a0)
	bne	srch_fn_end
	moveq.l	#96+1,d0
@@:
	subq.w	#1,d0
	beq	@f
	move.b	(a2)+,(a0)+
	bne	@b
	bra	pcd_lp01
@@:
	clr.b	(a0)+
	bra	srch_fn_end

cnv_wvf:			*波形メモリセット
	moveq.l	#0,d2
	bsr	get_cm_w
	move.w	d0,d2		*get size

	bsr	get_cm_l
	move.l	d0,d3		*get wave_n,loop_type,loop_addr.

	move.l	a2,a1
	trap	#3
	add.l	d2,d2
	adda.l	d2,a2
	bra	pcd_lp01

cnv_print:			*print
	* < a2.l=pointer
	pea	(a2)
	DOS	_PRINT
	addq.w	#4,sp
	bra	srch_fn_end

cnv_abdt:
	movea.l	a2,a1		*ファイルネームポインタを渡してコール
	ZM	#$39
	bra	srch_fn_end

cnv_rd_cnf:
	lea	OPM(pc),a1
	bra	self_output2

cnv_midi:
	lea	MIDI(pc),a1
*	bra	self_output2

self_output2:			*自己出力ルーチンその２
	* < a1.l=output destination
	* < a2.l=filename
	move.l	a1,out_name-work(a6)
	move.l	a2,d6
	lea	filename(pc),a0
@@:
	move.b	(a2)+,(a0)+
	bne	@b
	lea	filename(pc),a0
	cmpi.l	#'MiDi',(a1)
	bne	@f
	lea	MDD(pc),a1
	bra	so2
@@:
	lea	CNF(pc),a1
so2:
	bsr	kakuchoshi

	lea	filename(pc),a2
	bsr	fopen		*環境変数対応
	move.l	d6,a2
	tst.l	d5		*d5=file_handle
	bmi	exit_rd_cnf

	bsr	get_filedate
	move.l	date_buf(pc),d6
	exg.l	d0,d6
	cmp.l	d0,d6
	bne	@f
	bsr	fname_chk
	beq	exit_no_err
@@:
	bsr	get_fsize	*>d3.l=file size
	bmi	exit_rd_cnf

	lea	adpcm_work_size(pc),a0
	cmp.l	(a0),d3
	bhi	exit_rd_cnf

	move.l	adpcm_work_end(pc),d1	*ワークサイズを一時的に縮小する。
	sub.l	d3,d1			*read address
	move.l	(a0),-(sp)
	move.l	adpcm_work_end(pc),-(sp)
	sub.l	d3,(a0)
	move.l	d1,adpcm_work_end-work(a6)

	bsr	so_read
	tst.l	d0
	bmi	exit_rd_cnf2	*read error

	bsr	do_fclose

	move.l	d6,date_buf-work(a6)
	bsr	fn_to_lastfn

	bsr	so_ope

	move.l	(sp)+,adpcm_work_end-work(a6)
	move.l	(sp)+,(a0)
	bra	srch_fn_end	*same

exit_rd_cnf2:
	move.l	(sp)+,adpcm_work_end-work(a6)
	move.l	(sp)+,(a0)
exit_rd_cnf:			*case error
	bsr	play_beep
exit_no_err:
	bsr	do_fclose
	bra	srch_fn_end	*exit

get_cm_w:
	move.b	(a2)+,d0
	lsl.w	#8,d0
	move.b	(a2)+,d0
	rts

get_cm_l:
	move.b	(a2)+,d0
	lsl.w	#8,d0
	move.b	(a2)+,d0
	swap	d0
	move.b	(a2)+,d0
	lsl.w	#8,d0
	move.b	(a2)+,d0
	rts

so_read:
	move.w	sr,-(sp)
	andi.w	#$f8ff,sr
	move.l	d3,-(sp)	*push size
	move.l	d1,-(sp)	*push addr
	move.w	d5,-(sp)	*file handle
	DOS	_READ		*サンプリングデータの読み込み
	lea	10(sp),sp
	move.w	(sp)+,sr
	rts

so_ope:
	move.w	#$7400,case_child-work(a6)

	move.w	#%0_000_01,-(sp)	*自分自身へ出力しちゃう
	move.l	out_name(pc),-(sp)
	DOS	_OPEN
*	addq.w	#6,sp
	move.l	d0,d5		*d5.w=file handle

	move.l	d3,-(sp)	*size
	move.l	d1,-(sp)	*data address
	move.w	d5,-(sp)
	DOS	_WRITE
*	lea	10(sp),sp
	lea	16(sp),sp	*!!!

	bsr	do_fclose

	move.w	#NOP,case_child-work(a6)
	rts

get_filedate:
	clr.l	-(sp)
	move.w	d5,-(sp)
rwff3:
	DOS	_V2_FILEDATE
	addq.w	#6,sp
	rts

se_adpcm1:			*ADPCMをSEモードで鳴らす
	*   cmd=$13
	* < a1=adpcm data addr
	* < d2=adpcm data size
	* < d3=adpcm pan/frq parameters
	ori.w	#$700,sr
*	andi.b	#$f7,$00e88015	*MFP FM_int off
*	move.w	sr_type(pc),sr	*INT enable
	move.w	d3,d1
sea1:				*pc8!
	swap	d3
	tst.b	se_mode-work(a6)
	beq	@f
	cmp.b	se_level(pc),d3
	bcs	exit_se_ad1
@@:
	move.b	d3,se_level-work(a6)
	st.b	se_mode-work(a6)
sea1_:
	jsr	adpcmout-work(a6)
exit_se_ad1:
*	ori.b	#$08,$00e88015	*MFP FM_int ON
	rts

se_adpcm2:			*ドライバー内のADPCM DATAをSEモードで鳴らす
	*   cmd=$14
	* < d2=note number
	* < d3=adpcm pan/frq parameters
	ori.w	#$700,sr
*	andi.b	#$f7,$00e88015	*MFP FM_int off
*	move.w	sr_type(pc),sr	*INT enable
	move.w	d3,d1
	move.l	adpcm_tbl(pc),a0
	lsl.l	#3,d2
	adda.l	d2,a0
	move.l	(a0)+,d0	*address
	beq	exit_se_ad2
	move.l	d0,a1
	move.l	(a0)+,d2	*size
sea2:
	swap	d3
	tst.b	se_mode-work(a6)
	beq	@f
	cmp.b	se_level(pc),d3
	bcs	exit_se_ad2
@@:
	move.b	d3,se_level-work(a6)
	st.b	se_mode-work(a6)
sea2_:
	jsr	adpcmout-work(a6)
exit_se_ad2:
*	ori.b	#$08,$00e88015	*MFP FM_int ON
	rts

set_ch_mode:			*チャンネルモード設定
	*   cmd=$15
	* < d2=mode number
	*    0=FM基準モード
	*    1=MIDI基準モード
	lea	real_ch_tbl(pc),a0
	tst.l	d2
	beq	fm_mode
				*MIDI
	moveq.l	#9,d0
	bsr	scm_midi
	moveq.l	#0,d0
	bsr	scm_fm
	bra	t_dat_ok
fm_mode:			*FM
	moveq.l	#0,d0
	bsr	scm_fm
	bsr	scm_midi
	bra	t_dat_ok
scm_midi:
	moveq.l	#16-1,d1
scm_lp01:
	move.b	d0,(a0)+
	addq.b	#1,d0
	dbra	d1,scm_lp01
	rts
scm_fm:
	moveq.l	#9-1,d1
scm_lp02:
	move.b	d0,(a0)+
	addq.b	#1,d0
	dbra	d1,scm_lp02
	rts

midi_rec:			*MIDI生データの録音
	*   cmd=$16

*nmdb5:				*nmdb!!
	bsr	m_stop_all	*演奏停止
	bsr	init_play_trk_tbl
	move.l	adpcm_work_now(pc),rec_data_end-work(a6)
	clr.l	rec_data_now-work(a6)	*mark rec start

	ori.w	#$0700,sr
	if	(type<>3.and.type<>4)
	lea	rgr,a0
	move.b	#3,(a0)
					midiwait
	move.b	#$81,grp5-rgr(a0)	*buffer clear
					midiwait
	clr.b	(a0)
					midiwait
	move.b	#$20,grp6-rgr(a0)	*割り込みオン
					midiwait
	bra	t_dat_ok

	elseif	type=3
	lea	$e98005,a0
	tst.b	(a0)
	move.b	#$01,(a0)
	move.b	#%000_10_000,(a0)
	bra	t_dat_ok

	elseif	type=4
	bra	t_dat_ok
	endif

midi_rec_end:			*MIDI生データ録音終了&GETステータス
	*   cmd=$17
	* < d2.l＝0 MAKE ASCII DATA
	* < d2.l≠0 NO TOUCH WITH DATA
	* > d0.l=data size
	* > a0.l=data address
	* エラーの場合はd0=エラーコード/a0=0となる

*nmdb6:					*nmdb!!
	if	type=4
	bra	t_dat_ok
	endif
	if	(type<>3.and.type<>4)
	move.w	sr,-(sp)
	ori.w	#$0700,sr
	lea	rgr,a1
	clr.b	(a1)
					midiwait
	clr.b	grp6-rgr(a1)			*int end
					midiwait
	move.w	(sp)+,sr

	elseif	type=3
	bsr	init_midibd		*int end (記録強制終了)
	endif

	tst.l	rec_data_now-work(a6)
	bne	t_dat_ok
	move.l	rec_data_end(pc),a1
	move.l	a1,rec_data_now-work(a6)	*エラーのために途中で
						*抜けた場合を考え暴走防止のためこうする
	cmp.l	adpcm_work_end(pc),a1
	bcc	over_flow2		*work is too small
	move.l	adpcm_work_now(pc),a0
	cmp.l	a0,a1
	beq	t_err_73		*buffer is empty.
	tst.l	d2
	bne	no_touch_mode
	movem.l	a0-a1,-(sp)
	bsr	m_init
	movem.l	(sp)+,a0-a1
	movea.l	trk_top(pc),a2
	move.b	#$0d,(a2)+	*DUMMY HEADER
	move.b	#$0a,(a2)+
	moveq.l	#16-1,d2
	moveq.l	#0,d3
gmd_lp01:
	cmpi.b	#$f7,d3		*$F7以上で改行
	bcs	chk_gmd_end
	bsr	set_crlf
chk_gmd_end:
	cmpa.l	a1,a0
	beq	all_end_gmd
	cmpa.l	trk_buf_end(pc),a2
	bcc	over_flow	*trk buffer is too small
	move.b	(a0)+,d1
	move.b	d1,d0
	lsr.b	#4,d0
	add.b	#$30,d0
	cmpi.b	#'9',d0
	bls	go_next_gmd
	addq.b	#7,d0
go_next_gmd:
	move.b	d0,(a2)+
	move.b	d1,d3		*save last
	andi.b	#$0f,d1
	addi.b	#$30,d1
	cmpi.b	#'9',d1
	bls	go_next_gmd1
	addq.b	#7,d1
go_next_gmd1:
	move.b	d1,(a2)+
	move.b	#' ',(a2)+
	dbra	d2,gmd_lp01
	bsr	set_crlf
	bra	gmd_lp01
set_crlf:
	moveq.l	#16-1,d2
	move.b	#$0d,-1(a2)
	move.b	#$0a,(a2)+
	rts
all_end_gmd:
	move.b	#$1a,(a2)+
	move.l	a2,d2
	movea.l	trk_top(pc),a1		*source
	sub.l	a1,d2			*size
	movea.l	adpcm_work_now(pc),a2	*destination
	move.l	a2,rec_data_now-work(a6)
	move.b	#$1a,(a2)		*dummy for error exition
	move.l	a2,d0
	add.l	d2,d0
	cmp.l	adpcm_work_end(pc),d0
	bcc	over_flow2		*work area is too small
	move.l	d0,rec_data_end-work(a6)
	moveq.l	#5,d1
	bsr	trans_dma
	move.l	d2,d0
	movea.l	a2,a0
	rts
over_flow:			*トラックバッファが小さすぎる
	bsr	play_beep
	move.l	#1,rec_data_end-work(a6)
	move.l	#1,rec_data_now-work(a6)	*dummy
	bra	t_err_28	*track buffer is full
over_flow2:			*ワークエリアが小さすぎる
	bsr	play_beep
	move.l	#1,rec_data_end-work(a6)
	move.l	#1,rec_data_now-work(a6)	*dummy
	bra	t_err_64		*work area is too small

no_touch_mode:
	move.l	a1,d0
	sub.l	a0,d0	*d0=size
	rts

	if	(type<>3.and.type<>4)
chk_out_midi:			*データ出力(必要ならばMIDIボードを初期化する)
	* < d1.b=data
	* - all
	movem.l	d2/a3-a4,-(sp)
comd0:
	set_a3a4
	moveq.l	#$10,d2
	swap	d2		*d2=$10_0000
@@:
	subq.l	#1,d2
	bmi	go_init_md	*待ってもバッファが一杯なので初期化
				*midiwait
	btst.b	#6,(a4)
	beq	@b
				*midiwait
	move.b	d1,(a3)		*send data
				midiwait
	cmpi.b	#$f7,d1
	bne	@f
	bsr	f7_wait
@@:
	movem.l	(sp)+,d2/a3-a4
	rts

go_init_md:
	bsr	init_midibd
	bra	comd0

	elseif	type=3

chk_out_midi:			*データ出力(必要ならばRS232Cを初期化する)
	* < d1.b=data
	* - all
	movem.l	d2/a0,-(sp)
comd0:
	lea	$e98005,a0
	moveq.l	#$10,d2
	swap	d2		*d2=$10_0000
@@:
	subq.l	#1,d2
	bmi	go_init_md	*待ってもバッファが一杯なので初期化
	btst.b	#2,(a0)
	beq	@b
	move.b	d1,2(a0)	*send data
	cmpi.b	#$f7,d1
	bne	@f
	bsr	f7_wait
@@:
	movem.l	(sp)+,d2/a0
	rts

go_init_md:
	bsr	init_midibd
	bra	comd0

	endif

	if	type=4		*polyphon case

chk_out_midi:			*データ出力
	* < d1.b=data
	* - all
	movem.l	d0-d2,-(sp)
	move.l	d1,d2
	moveq.l	#0,d1
@@:
	move.w	#$0303,d0
	trap	#2
	tst.l	d0
	bmi	@b
	cmpi.b	#$f7,d1
	bne	@f
	bsr	f7_wait
@@:
	movem.l	(sp)+,d0-d2
	rts

	endif

midi_trns:			*MIDI生データの転送
	*   cmd=$18
	* < d2.l:mode (size)
	*	＝0 ASCII mode (end code:$1a)
	*	≠0 BIN mode (data size)最大64kB
	* < a1.l=data address

nmdb7:				*nmdb!!
	tst.l	d2		*check size
	bne	bin_trns_md
mtrs_lp01:
	move.b	(a1)+,d0
	cmpi.b	#$1a,d0
	beq	t_dat_ok
	cmpi.b	#'.',d0
	bls	mtrs_lp01	*セパレータスキップ
	cmpi.b	#'/',d0
	beq	skip_cmnt
	cmpi.b	#'*',d0
	beq	skip_cmnt
	bsr	mk_capital
	move.b	d0,d1
	sub.b	#'0',d1
	cmpi.b	#9,d1
	bls	next_b
	subq.b	#7,d1
next_b:
	move.b	(a1)+,d0
	bsr	mk_capital
	sub.b	#'0',d0
	cmpi.b	#9,d0
	bls	do_trns_data
	subq.b	#7,d0
do_trns_data:
	lsl.b	#4,d1
	or.b	d0,d1
	bsr	chk_out_midi	*大量データ転送が考えられるのであえてこうする
	bra	mtrs_lp01
skip_cmnt:			*改行までスキップ
	move.b	(a1)+,d0
	cmpi.b	#" ",d0
	bcc	skip_cmnt
	cmpi.b	#09,d0
	beq	skip_cmnt	*skip tab
	bra	mtrs_lp01

bin_trns_md:			*BINARY DATAの送信
	* < d2.l=size
	* < a1.l=data address
bin_trns_md_lp:
	move.b	(a1)+,d1
	bsr	chk_out_midi	*大量データ転送が考えられるのであえてこうする
	subq.l	#1,d2
	bne	bin_trns_md_lp
	bra	t_dat_ok

calc_total:			*各ﾄﾗｯｸのｽﾃｯﾌﾟﾀｲﾑの合計を求める
	*   cmd=$19
	* < d2＝0 表示
	* < d2≠0 表示無し
	move.b	d2,-(sp)		*mark

	bsr	m_stop_all

	move.w	sr,-(sp)
	ori.w	#$0700,sr

	bsr	top_ptr_set_all

	lea	play_trk_tbl(pc),a0
	moveq.l	#tr_max-1,d1	*pl_max
clc_ttl_lp:
	move.b	(a0)+,d0		*0-79
	bmi	clc_ttl_end
	bsr	calc_wk
	bsr	init_wks
	moveq.l	#0,d0
	move.b	d0,p_not_empty(a5)
	move.b	d0,p_se_mode(a5)		*dummy
	move.l	d0,p_total(a5)
	move.l	d0,p_total_olp(a5)
	st.b	p_marker(a5)	*!!!
	dbra	d1,clc_ttl_lp
clc_ttl_end:







	bsr	set_clc_patch1		*パッチ当て処理へ
	bsr	set_clc_patch2
	bsr	set_clc_patch3
	cache_flush
	st.b	trace_mode-work(a6)	*flag on
clc_ttl_lp01:
	lea	play_trk_tbl(pc),a0
	moveq.l	#tr_max-1,d1	*pl_max
	moveq.l	#0,d2
clc_ttl_lp02:
	bsr	reset_int_e		*疑似割り込み
	bsr	reset_int_e		*疑似割り込み
	move.b	(a0)+,d0
	bmi	@f
	bsr	calc_wk
	tst.b	p_not_empty(a5)
	beq	clc_ttl_next
@@:
	addq.b	#1,d2
clc_ttl_next:
	dbra	d1,clc_ttl_lp02
chk_dead_tr:
	cmpi.b	#tr_max,d2	*pl_max
	bne	clc_ttl_lp01

	bsr	back_patch		*パッチ復元処理
	bsr	back_patch1
	bsr	back_patch3
	cache_flush

	lea	play_trk_tbl(pc),a0
	clr.b	trace_mode-play_trk_tbl(a0)	*flag off
	moveq.l	#tr_max-1,d1	*pl_max
	moveq.l	#0,d3
tc_hosei:
	move.b	(a0)+,d0
	bmi	tc_hosei_end
	bsr	calc_wk
	move.l	p_total_olp(a5),d2	*ループ外
	tst.b	p_do_loop_flag(a5)
	bmi	@f
	add.l	p_total(a5),d2
	move.l	d2,p_total_olp(a5)
	clr.l	p_total(a5)
@@:
	or.l	p_total(a5),d3
	or.l	p_total_olp(a5),d3
	dbra	d1,tc_hosei
tc_hosei_end:
	tst.l	d3			*all zero?
	bne	@f
	move.w	(sp)+,sr
	addq.w	#2,sp
	moveq.l	#-2,d0
	rts
@@:
	move.w	(sp)+,sr
	tst.b	(sp)+			*表示無しのモードか?
	bne	t_dat_ok

	lea	CRLF(pc),a0
	bsr	prta0

	lea	play_trk_tbl(pc),a0
	lea	suji(pc),a2
	moveq.l	#tr_max-1,d1	*pl_max
	moveq.l	#4,d2
	moveq.l	#1,d3		*crした回数
clc_ttl_lp03:
	moveq.l	#0,d0
	move.b	(a0)+,d0
	bmi	clc_ttl_all_end
	bsr	calc_wk
	addq.b	#1,d0		*1-80
	bsr	num_to_str
	tst.b	1(a2)
	bne	@f
	move.b	(a2)+,(a2)+	*１桁のケース
	clr.b	(a2)		*end code
	subq.w	#2,a2
	move.b	#' ',(a2)
@@:
	pea	(a2)
	DOS	_PRINT
*	addq.w	#4,sp
	move.w	#':',-(sp)
	DOS	_PUTCHAR
*	addq.w	#2,sp

	move.l	p_total_olp(a5),d0	*ループ外
	movea.l	a2,a1			*a1=suji
	bsr	get_hex32
	pea	(a2)
	DOS	_PRINT
*	addq.w	#4,sp

	move.w	#' ',-(sp)
	DOS	_PUTCHAR
*	addq.w	#2,sp

	move.l	p_total(a5),d0		*ループ内
	move.l	a2,a1			*a1=suji
	bsr	get_hex32
	pea	(a2)
	DOS	_PRINT
*	addq.w	#4,sp
	lea	16(sp),sp		*!!!

	subq.b	#1,d2
	bne	ttl_tab
	pea	CRLF(PC)
	DOS	_PRINT
	addq.w	#4,sp
	addq.b	#1,d3		*inc cr counter
	moveq.l	#4,d2
	bra	clc_ttl0
ttl_tab:
	pea	SPC2(PC)
	DOS	_PRINT
	addq.w	#4,sp
clc_ttl0:
	dbra	d1,clc_ttl_lp03
clc_ttl_all_end:
	lea	_0d(pc),a0
	bsr	prta0

	subq.w	#1,d3
	lea	b_up(pc),a0
@@:
	bsr	prta0
	dbra	d3,@b
	bra	t_dat_ok

_0d:	dc.b	$0d,0
b_up:	dc.b	$0b,0

set_clc_patch1:			*レジスタ破壊禁止	*パッチ当て処理その1
	move.w	#BRA+(goto_ple.and.$ff),loop_ope-work(a6)
	move.w	#BRA+(goto_m_int_lp.and.$ff),se_ope-work(a6)
	rts

set_clc_patch2:			*レジスタ破壊禁止	*パッチ当て処理その2
	move.l	d0,-(sp)
	move.l	#$d3ad0000+p_total,d0		*add.l	d1,p_total(a5)
	move.l	d0,w_com_patch-work(a6)
	move.l	d0,case_key_patch-work(a6)
	move.l	d0,w_step_patch-work(a6)
	move.l	d0,rest_patch-work(a6)
	move.l	d0,port_patch-work(a6)
	move.l	d0,waon_patch-work(a6)
	move.l	(sp)+,d0
	rts

set_clc_patch3:			*レジスタ破壊禁止	*パッチ当て処理その3
	pea	(a1)
	lea	h_work(pc),a1
	move.w	sr_restore(pc),(a1)+
	move.w	#BRA+((sr_restore_e-sr_restore-2).and.$ff),sr_restore-work(a6)
	move.w	#RTS,int_rte-work(a6)
	move.l	dec_gate(pc),(a1)+
	move.l	#BRA*65536+((run_cmd-dec_gate-2).and.$ffff),dec_gate-work(a6)
	move.l	wrt_tmp(pc),(a1)+
	move.l	#BRA*65536+((next_cmd-wrt_tmp-2).and.$ffff),wrt_tmp-work(a6)
	move.l	(sp)+,a1
	rts

back_patch:
	movem.l	d0/a0,-(sp)
	move.l	#NOP_NOP,d0
	lea	w_com_patch(pc),a0		*わざとa0.l
	move.l	d0,(a0)
	move.l	d0,case_key_patch-w_com_patch(a0)
	move.l	d0,w_step_patch-w_com_patch(a0)
	move.l	d0,rest_patch-w_com_patch(a0)
	move.l	d0,port_patch-w_com_patch(a0)
	move.l	d0,waon_patch-w_com_patch(a0)
	move.l	d0,noteon_patch-w_com_patch(a0)
	move.l	d0,noteoff_patch-w_com_patch(a0)
	move.l	d0,len0_patch-w_com_patch(a0)
	move.l	d0,opmd_y2_ope-w_com_patch(a0)
	movem.l	(sp)+,d0/a0
	rts

back_patch1:
	move.l	d0,-(sp)
	move.l	#NOP_NOP,d0
	move.w	d0,se_ope-work(a6)
	move.l	d0,loop_ope-work(a6)		*当てたパッチを戻す
	move.l	(sp)+,d0
	rts

back_patch3:
	pea	(a1)
	lea	h_work(pc),a1
	move.w	(a1)+,sr_restore-work(a6)
	move.w	rte_src(pc),int_rte-work(a6)	*ori.w
	move.l	(a1)+,dec_gate-work(a6)
	move.l	(a1)+,wrt_tmp-work(a6)
	move.l	(sp)+,a1
	rts

fade_out:			*ﾌｪｰﾄﾞｱｳﾄ/ｲﾝ処理
	*   cmd=$1a
	* < d2.l=fade out speed(d2.l=0:off,d2.l=1～85:ﾌｪｰﾄﾞｱｳﾄ,d2.l=-1～-85:ﾌｪｰﾄﾞｲﾝ)

	ori.w	#$700,sr
	andi.b	#$f7,$00e88015	*MFP FM_int off
	move.w	sr_type(pc),sr	*INT enable
fo__:
	tst.l	d2
	beq	fo_off
	lea	play_trk_tbl(pc),a0
mf0_lp:
	move.b	(a0)+,d0
	bmi	fo_mode_chk?
	bsr	calc_wk
	move.b	p_fo_mode(a5),d0	*すでにやっているか
	beq	mf0_lp
	cmpi.b	#$47,d0
	beq	mf0_lp
	moveq.l	#-1,d0
	bra	exit_mfo_	*やってるなら無視
fo_mode_chk?:
	moveq.l	#-1,d3		*フェードアウト
	tst.l	d2
	bpl	chk_fo_max
	moveq.l	#1,d3		*フェードイン
	neg.l	d2		*スピードを絶対値に
	bra	chk_fo_max
fo_off:
	moveq.l	#0,d3		*OFF
chk_fo_max:			*スピードチェック
	cmpi.l	#fo_max,d2
	bls	mfo_set
	moveq.l	#fo_dflt,d2	*範囲外ならデフォルトに
mfo_set:
	lea	play_trk_tbl(pc),a0
mfo_lp:
	move.b	(a0)+,d0
	bmi	exit_mfo
	bsr	calc_wk
	clr.b	1+p_marker(a5)
	move.b	d2,p_fo_spd(a5)
	move.b	d3,p_fo_mode(a5)
	bmi	fo_set_
				*フェードインのケース
	moveq.l	#0,d0		*MIDI/ADPCM
	cmpi.b	#8,p_ch(a5)
	bcc	@f
	moveq.l	#64,d0		*FM
@@:
	move.b	d0,p_fo_lvl(a5)
	bra	mfo_lp
fo_set_:			*フェードアウトのケース
	move.b	#127,p_fo_lvl(a5)
	bra	mfo_lp
exit_mfo:
	moveq.l	#0,d0
exit_mfo_:
	ori.b	#$08,$00e88015	*MFP FM_int ON
	rts

m_vset2:
	*   cmd=$1b
	* < d2.l=voice number
	* < a1.l=data address

	subq.w	#1,d2
	cmpi.l	#tone_max-1,d2
	bhi	t_err_39

	movea.l	neiro(pc),a2
	mulu	#55,d2
	lea	11(a2,d2.w),a2
	moveq.l	#44-1,d2
@@:
	move.b	(a1)+,(a2)+
	dbra	d2,@b

	lea	-44-11(a2),a2
	move.b	1(a1),d0	*fb
	lsl.b	#3,d0		*fb*8
	or.b	(a1),d0		*make AF
	move.b	d0,(a2)+	*AF  0
	move.b	2(a1),(a2)+	*OM  1
	lea	4(a1),a3
	move.b	(a3)+,(a2)+	*WF  2
	move.b	(a3)+,(a2)+	*syc 3
	move.b	(a3)+,(a2)+	*spd 4
	move.b	(a3)+,(a2)+	*pmd 5
	move.b	(a3)+,(a2)+	*amd 6
	move.b	(a3)+,(a2)+	*pms 7
	move.b	(a3)+,(a2)+	*ams 8
	move.b	3(a1),(a2)+	*pan 9
	clr.b	(a2)+		*--- 10
	bra	t_dat_ok

send_rd_exc:
	*   cmd=$1c
	* < a1.l=data address
	* < d2.l=size
	* < d3.w=id,model
	movem.l	d2/a1,-(sp)
	lea	header(pc),a1
	move.w	d3,2(a1)	*devIDとmodel Idをセット
	moveq.l	#5,d2
	bsr	midi_trns
	movem.l	(sp)+,d2/a1
	movem.l	d2/a1,-(sp)
	bsr	midi_trns	*data 本体を転送
	movem.l	(sp)+,d2/a1
	moveq.l	#0,d1		*init sum
	bsr	calc_chksum
	move.b	d0,tail-work(a6)	*尻尾を転送
	lea	tail(pc),a1
	moveq.l	#2,d2
	bra	midi_trns

calc_chksum:			*ﾛｰﾗﾝﾄﾞｴｸｽｸﾙｰｼﾌﾞﾃﾞｰﾀのﾁｪｯｸｻﾑを算出する
	* < a1.l=data addr
	* < d2.l=data size
	* < check sum初期値
	* > d0.b=check sum value
	* > d1.b total sum
	* - all
	movem.l	d2/a1,-(sp)
@@:				*calc checksum
	add.b	(a1)+,d1
	subq.l	#1,d2
	bne	@b
	moveq.l	#$80,d0
	move.b	d1,d2
	andi.b	#$7f,d2
	sub.b	d2,d0
	andi.b	#$7f,d0
	movem.l	(sp)+,d2/a1
	rts

send_exc:			*通常のｴｸｽｸﾙｰｼﾌﾞ転送
	*   cmd=$1d
	* < a1.l=data address
	* < d2.l=size
	movem.l	d2/a1,-(sp)
	lea	header(pc),a1
	moveq.l	#1,d2		*header
	bsr	midi_trns
	movem.l	(sp)+,d2/a1
	movem.l	d2/a1,-(sp)
	bsr	midi_trns	*data 本体を転送
	movem.l	(sp)+,d2/a1
	lea	tail+1(pc),a1
	moveq.l	#1,d2
	bra	midi_trns	*send tail

send_header:
	movem.l	d2/a1,-(sp)
	lea	header(pc),a1
	moveq.l	#8,d2
	bsr	midi_trns
	movem.l	(sp)+,d2/a1
	rts

send_tail:
	* < d0.b=check sum
	move.b	d0,tail-work(a6)	*尻尾を転送
	lea	tail(pc),a1
	moveq.l	#2,d2
	bra	midi_trns

sc55_p_rsv:			*SC55のパーシャルリザーブを設定
	*   cmd=$1e
	* < a1.l=data address
	* < d2.l=size
	* < d3.b=sc55 id number
	tst.b	d3
	bpl	@f
	move.b	sc55_id(pc),d3		*DEFAULTを使う
@@:
	lea	header+2(pc),a2
	move.b	d3,sc55_id-work(a6)
	move.b	d3,(a2)+		*set dev ID
	move.b	#$42,(a2)+		*set model id
	addq.w	#1,a2
	move.b	#$40,(a2)+		*set addr H
	move.w	#$0110,(a2)+		*set addr L
	bsr	send_header
	lea	v_buffer(pc),a0
	move.b	9(a1),(a0)+	*copy part 10 
	moveq.l	#9-1,d0
@@:
	move.b	(a1)+,(a0)+	*copy part 1 to 9
	dbra	d0,@b
	addq.w	#1,a1		*skip part 10
	moveq.l	#6-1,d0
@@:
	move.b	(a1)+,(a0)+	*copy part 11 to 16
	dbra	d0,@b
	moveq.l	#16,d2
	lea	v_buffer(pc),a1
	bsr	midi_trns	*data 本体を転送
	moveq.l	#$51,d1		*アドレスのサム
	moveq.l	#16,d2
	lea	v_buffer(pc),a1
	bsr	calc_chksum
	bra	send_tail

sc55_reverb:			*SC55のリバーブパラメータ設定
	*   cmd=$1f
	* < a1.l=data address
	* < d2.l=size
	* < d3.b=sc55 id number
	tst.b	d3
	bpl	@f
	move.b	sc55_id(pc),d3		*DEFAULTを使う
@@:
	lea	header+2(pc),a2
	move.b	d3,sc55_id-work(a6)
	move.b	d3,(a2)+		*set dev ID
	move.b	#$42,(a2)+		*set model id
	addq.w	#1,a2
	move.b	#$40,(a2)+
	move.w	#$0130,(a2)+
	subq.w	#1,d2			*for dbra
	bmi	t_err_69
sc55rev_set:
	move.b	-(a2),d3
	add.b	-(a2),d3
	add.b	-(a2),d3
	move.l	a1,a3
	moveq.l	#0,d1
sc55rev_set_lp:
	add.b	(a1)+,d3
	addq.l	#1,d1
	dbra	d2,sc55rev_set_lp
	move.w	d3,-(sp)
	move.l	d1,-(sp)
	bsr	send_header
	move.l	a3,a1
	move.l	(sp)+,d2
	bsr	midi_trns
	moveq.l	#$80,d0
	move.w	(sp)+,d2
	andi.b	#$7f,d2
	sub.b	d2,d0
	andi.b	#$7f,d0
	bsr	send_tail
	bra	t_dat_ok

sc55_chorus:			*SC55のコーラスパラメータ設定
	*   cmd=$20
	* < a1.l=data address
	* < d2.l=size
	* < d3.b=sc55 id number
	tst.b	d3
	bpl	@f
	move.b	sc55_id(pc),d3		*DEFAULTを使う
@@:
	lea	header+2(pc),a2
	move.b	d3,sc55_id-work(a6)
	move.b	d3,(a2)+		*set dev ID
	move.b	#$42,(a2)+		*set model id
	addq.w	#1,a2
	move.b	#$40,(a2)+
	move.w	#$0138,(a2)+
	subq.w	#1,d2			*for dbra
	bmi	t_err_69
	bra	sc55rev_set		*後は上と同じ

sc55_part_parameter:		*SC55のパートパラメータ設定
	*   cmd=$21
	* < a1.l=data address
	* < d2.l=size(1～120)
	* < d3.hw=sc55 part number
	* < d3.lw=sc55 id number
	tst.b	d3
	bpl	@f
	move.b	sc55_id(pc),d3		*DEFAULTを使う
@@:
	lea	header+2(pc),a2
	move.b	d3,sc55_id-work(a6)
	move.b	d3,(a2)+		*set dev ID
	move.b	#$42,(a2)+		*set model id
	move.l	#$401002,d4		*inst address
	subq.w	#1,d2			*for dbra
	bmi	t_err_69
	swap	d3
	ext.w	d3
	cmpi.w	#16,d3
	bhi	t_err_69		*illegal part
	cmpi.b	#10,d3
	bcs	@f			*9以下はそのまま
	bhi	dec_ptn
	moveq.l	#0,d3			*10はd3=0
	bra	@f
dec_ptn:
	subq.b	#1,d3			*10以上はd3=d3-1
@@:
	lsl.w	#8,d3
	or.w	d3,d4			*partナンバーにあったアドレス値
	move.b	(a1),d0			*保存
	movem.l	d0/a1,-(sp)		*先頭データ保存
	subq.b	#1,d0			*MIDI CH(1～16)->内部コード($00～$0f)
	cmpi.b	#$0f,d0
	bls	@f
	move.b	#$10,d0			*範囲外はOFFとみなす
@@:
	move.b	d0,(a1)

	move.l	d4,d5
	move.l	a1,a3
	moveq.l	#0,d1
	moveq.l	#0,d3
	lea	sc55ptofs(pc),a2
sc55ptlp:
	add.b	(a1)+,d1
	addq.l	#1,d3
	addq.b	#1,d4		*どうせ上位は変化しないから
	move.w	d4,d0
	andi.w	#$f0ff,d0
	cmp.w	(a2),d0
	bne	@f
	bsr	send_sc55ptdt
	addq.w	#2,a2
	add.w	(a2)+,d4
	move.l	d4,d5
	move.l	a1,a3
	moveq.l	#0,d1
	moveq.l	#0,d3
@@:
	dbra	d2,sc55ptlp
	tst.l	d3
	beq	@f
	bsr	send_sc55ptdt
@@:
	movem.l	(sp)+,d0/a1
	move.b	d0,(a1)		*書き換えた値をもとに戻す
	bra	t_dat_ok

send_sc55ptdt:
	* < a3.l=data address
	* < d1.b=sum
	* < d5.l=address
	* < d3.l=count
	movem.l	d2/d4/a1-a2,-(sp)
	lea	exc_addr(pc),a1
	swap	d5
	move.b	d5,(a1)+
	swap	d5
	move.w	d5,(a1)+
	add.b	-(a1),d1
	add.b	-(a1),d1
	add.b	-(a1),d1
	move.w	d1,-(sp)
	move.l	d3,-(sp)
	bsr	send_header
	move.l	a3,a1
	move.l	(sp)+,d2
	bsr	midi_trns
	moveq.l	#$80,d0
	move.w	(sp)+,d2
	andi.b	#$7f,d2
	sub.b	d2,d0
	andi.b	#$7f,d0
	bsr	send_tail
	movem.l	(sp)+,d2/d4/a1-a2
	rts

sc55ptofs:
	dc.w	$1017,$0000
	dc.w	$1019,$0000
	dc.w	$1023,$000d
	dc.w	$1038,$0008
	dc.w	$104c,$0fb4
	dc.w	$200b,$0005
	dc.w	$201b,$0005
	dc.w	$202b,$0005
	dc.w	$203b,$0005
	dc.w	$204b,$0005
	dc.w	-1

sc55_drum_parameter:		*SC55のドラムパラメータ設定
	*   cmd=$22
	* < a1.l=data address
	* < d2.l=size
	* < d3.hw=sc55 map number*256+note number
	* < d3.lw=sc55 id number
	tst.b	d3
	bpl	@f
	move.b	sc55_id(pc),d3		*DEFAULTを使う
@@:
	lea	header+2(pc),a2
	move.b	d3,sc55_id-work(a6)
	move.b	d3,(a2)+		*set dev ID
	move.b	#$42,(a2)+		*set model id
	move.l	#$410100,d4		*inst address
	swap	d3
	move.b	d3,d4			*note set
	lsl.w	#4,d3
	andi.w	#$f000,d3
	or.w	d3,d4			*make address
	subq.w	#1,d2			*for dbra
	bmi	t_err_69
sc55drm_set_lp:
	bsr	sc_p_set
	add.w	#$0100,d4
	addq.w	#1,a1
	dbra	d2,sc55drm_set_lp
	bra	t_dat_ok

sc_p_set:				*sc55フォーマット:1パラメータ書き込み
	* < d4=instrument address	*(dev ID,model IDは先に
	* < a1.l=data address		*header+2,+3へ設定しておくこと)
	* < d2.l=data size
	* - all
	movem.l	d0-d2/d4/a0-a2,-(sp)
	lea	exc_addr(pc),a0
	move.b	(a1),sc_p_data-exc_addr(a0)	*1バイトデータセット

	swap	d4
	move.b	d4,(a0)+		*set addr H
	swap	d4
	move.w	d4,(a0)+		*set addr L

	move.b	-(a0),d1		*アドレス加算(for make check sum)
	add.b	-(a0),d1
	add.b	-(a0),d1
	add.b	(a1),d1			*data加算
	moveq.l	#$80,d0
	andi.b	#$7f,d1
	sub.b	d1,d0
	lea	header(pc),a1
	move.b	d0,tail-header(a1)	*save ROLAND check sum
	moveq.l	#11,d2
	bsr	midi_trns	*尻尾を転送
	movem.l	(sp)+,d0-d2/d4/a0-a2
	rts

sc55_print:
	*   cmd=$23
	* < a1.l=data address
	* < d2.l=size
	* < d3.b=sc55 id number
	cmpi.l	#32,d2
	bhi	t_err_69
	tst.b	d3
	bpl	@f
	move.b	sc55_id(pc),d3		*DEFAULTを使う
@@:
	lea	header+2(pc),a2
	move.b	d3,sc55_id-work(a6)
	move.b	d3,(a2)+		*set dev ID
	move.b	#$45,(a2)+		*set model id
	addq.w	#1,a2
	move.b	#$10,(a2)+
	clr.w	(a2)+
	bsr	send_header
	movem.l	d2/a1,-(sp)
	bsr	midi_trns
	movem.l	(sp)+,d2/a1
	moveq.l	#$10,d1			*アドレスのサム
	bsr	calc_chksum
	bra	send_tail

sc55_display:			*SC55の画面にドットパターンを表示する
	*   cmd=$24
	* < a1.l=data address
	* < d3.b=sc55 id number
	lea	v_buffer(pc),a2
	tst.b	d3
	bpl	@f
	move.b	sc55_id(pc),d3		*DEFAULTを使う
@@:
	move.b	d3,sc55_id-v_buffer(a2)
	move.b	d3,-(sp)

	moveq.l	#16-1,d3
mk_ddlp:
	move.w	(a1)+,d1
	move.b	d1,d0
	lsl.b	#4,d0
	andi.b	#$10,d0
	move.b	d0,48(a2)
	move.w	d1,d0
	lsr.w	#1,d0
	andi.b	#$1f,d0
	move.b	d0,32(a2)
	move.w	d1,d0
	lsr.w	#6,d0
	andi.b	#$1f,d0
	move.b	d0,16(a2)
	rol.w	#5,d1			*same as lsr.w	#11,d1
	andi.b	#$1f,d1
	move.b	d1,(a2)+
	dbra	d3,mk_ddlp

	lea	header+2(pc),a2
	move.b	(sp)+,(a2)+		*set dev id
	move.b	#$45,(a2)+		*set model id
	addq.w	#1,a2
	move.b	#$10,(a2)+		*address H
	move.w	#$0100,(a2)+		*address L
	bsr	send_header
	moveq.l	#64,d2			*data length
	lea	v_buffer(pc),a1
	bsr	midi_trns		*データ本体を転送
	moveq.l	#$11,d1			*address値を加算
	moveq.l	#64,d2			*data length
	lea	v_buffer(pc),a1
	bsr	calc_chksum
	bra	send_tail

mt32_p_rsv:			*MT32のパーシャルリザーブを設定
	*   cmd=$25
	* < a1.l=data address
	* < d2.l=size
	* < d3.b=mt32 id number
	tst.b	d3
	bpl	@f
	move.b	mt32_id(pc),d3		*DEFAULTを使う
@@:
	lea	header+2(pc),a2
	move.b	d3,mt32_id-work(a6)
	move.b	d3,(a2)+		*set dev ID
	move.b	#$16,(a2)+		*set model id
	addq.w	#1,a2
	move.b	#$10,(a2)+		*set addr H
	move.w	#$0004,(a2)+		*set addr L
	bsr	send_header
	movem.l	d2/a1,-(sp)
	bsr	midi_trns	*data 本体を転送
	movem.l	(sp)+,d2/a1
	moveq.l	#$14,d1		*アドレスのサム
	bsr	calc_chksum
	bra	send_tail

mt32_reverb:			*MT32のリバーブパラメータ設定
	*   cmd=$26
	* < a1.l=data address
	* < d2.l=size
	* < d3.b=mt32 id number
	tst.b	d3
	bpl	@f
	move.b	mt32_id(pc),d3		*DEFAULTを使う
@@:
	lea	header+2(pc),a2
	move.b	d3,mt32_id-work(a6)
	move.b	d3,(a2)+		*set dev ID
	move.b	#$16,(a2)+		*set model id
	addq.w	#1,a2
	move.b	#$10,(a2)+		*set addr H
	move.w	#$0001,(a2)+		*set addr L
	bsr	send_header
	movem.l	d2/a1,-(sp)
	bsr	midi_trns	*data 本体を転送
	movem.l	(sp)+,d2/a1
	moveq.l	#$11,d1		*アドレスのサム
	bsr	calc_chksum
	bra	send_tail

mt32_setup:			*MT32のパートパラメータ設定
	*   cmd=$27
	* < a1.l=data address
	* < d2.l=size
	* < d3.b=mt32 id number
	tst.b	d3
	bpl	@f
	move.b	mt32_id(pc),d3		*DEFAULTを使う
@@:
	lea	header+2(pc),a2
	move.b	d3,mt32_id-work(a6)
	move.b	d3,(a2)+		*set dev ID
	move.b	#$16,(a2)+		*set model id
	addq.w	#1,a2
	move.b	#$10,(a2)+		*set addr H
	move.w	#$000d,(a2)+		*set addr L
	bsr	send_header

	lea	v_buffer(pc),a2
	move.l	d2,d1
	subq.w	#1,d1			*for dbra
mk_mdch_lp:				*1～16->$00～$0f
	move.b	(a1)+,d0
	subq.b	#1,d0
	cmpi.b	#$0f,d0
	bls	@f
	moveq.l	#$10,d0		*規定外はOFFとみなす
@@:
	move.b	d0,(a2)+
	dbra	d1,mk_mdch_lp
	move.l	d2,-(sp)
	lea	v_buffer(pc),a1
	bsr	midi_trns	*data 本体を転送
	move.l	(sp)+,d2
	moveq.l	#$1d,d1		*アドレスのサム
	lea	v_buffer(pc),a1
	bsr	calc_chksum
	bra	send_tail

mt32_drum:			*MT32のドラムパラメータ設定
	*   cmd=$28
	* < a1.l=data address
	* < d2.l=size
	* < d3.b=mt32 id number
	tst.b	d3
	bpl	@f
	move.b	mt32_id(pc),d3		*DEFAULTを使う
@@:
	lea	header+2(pc),a2
	move.b	d3,mt32_id-work(a6)
	move.b	d3,(a2)+		*set dev ID
	move.b	#$16,(a2)+		*set model id
	addq.w	#1,a2
	move.l	#$030110,d4		*parameter base address
	swap	d3
	cmpi.b	#87,d3
	bhi	t_err_69		*MTでは受信不可
	sub.b	#24,d3
	bmi	t_err_69
	ext.w	d3			*d3.w=d3.b-24
	add.w	d3,d3
	add.w	d3,d3
	add.w	d3,d4			*exact address
	bclr.l	#7,d4
	beq	@f
	add.w	#$0100,d4
@@:
	swap	d4
	move.b	d4,(a2)+		*set addr H
	move.b	d4,d1			*sum
	swap	d4
	move.w	d4,(a2)+		*set addr L
	add.b	d4,d1
	lsr.w	#8,d4
	add.b	d4,d1			*sum
	move.b	d1,-(sp)
	bsr	send_header
	movem.l	d2/a1,-(sp)
	bsr	midi_trns		*data 本体を転送
	movem.l	(sp)+,d2/a1
	move.b	(sp)+,d1		*アドレスのサム
	bsr	calc_chksum
	bra	send_tail

mt32_common:			*MT32の音色コモンパラメータ設定
	*   cmd=$29
	* < a1.l=data address
	* < d2.l=size
	* < d3.hw=program number(1-64)
	* < d3.lw=mt32 id number
	tst.b	d3
	bpl	@f
	move.b	mt32_id(pc),d3		*DEFAULTを使う
@@:
	lea	header+2(pc),a2
	move.b	d3,mt32_id-work(a6)
	move.b	d3,(a2)+		*set dev ID
	move.b	#$16,(a2)+		*set model id
	addq.w	#1,a2
	move.l	#$080000,d4		*base address
	swap	d3
	subq.b	#1,d3		*make prog number 0-63
	cmpi.b	#63,d3
	bhi	t_err_69	*illegal prog number
	ext.w	d3
	ror.w	#7,d3		*d3=d3*512
	move.w	d3,d4			*exact address
	swap	d4
	move.b	d4,(a2)+		*set addr H
	move.b	d4,d1			*sum
	swap	d4
	move.w	d4,(a2)+		*set addr L
	add.b	d4,d1
	lsr.w	#8,d4
	add.b	d4,d1			*address sum
	move.b	d1,sc_p_data-work(a6)	*ワークとして使う
	bsr	send_header		*send header
	lea	v_buffer(pc),a2
	moveq.l	#0,d0
@@:
	addq.l	#1,d0		*文字数カウント
	move.b	(a1)+,(a2)+
	bne	@b
	subq.w	#1,a2
	moveq.l	#10+1,d1
	sub.l	d0,d1
	beq	trns_cmp	*10文字丁度:他のパラメータの処理に
	bmi	t_err_69	*10文字以上:エラー
	subq.b	#1,d1		*for dbra
@@:
	move.b	#" ",(a2)+	*足りない分をスペースで埋める
	dbra	d1,@b
trns_cmp:
	sub.l	d0,d2
	bmi	t_err_69	*異常自体発生
	beq	send_cmdt
	move.l	d2,d0
	subq.l	#1,d0		*for dbra
@@:
	move.b	(a1)+,(a2)+	*数値パラメータをワークへ
	dbra	d0,@b
send_cmdt:
	add.w	#10,d2		*データ総バイト数
	move.l	d2,-(sp)
	lea	v_buffer(pc),a1
	bsr	midi_trns		*data 本体を転送
	move.l	(sp)+,d2
	move.b	sc_p_data(pc),d1	*アドレスのサム
	lea	v_buffer(pc),a1
	bsr	calc_chksum
	bra	send_tail

mt32_partial:			*MT32の音色パーシャルパラメータ設定
	*   cmd=$2a
	* < a1.l=data address
	* < d2.l=size
	* < d3.hw=program number(1-64)*256+partial number(1-4)
	* < d3.lw=mt32 id number
	tst.b	d3
	bpl	@f
	move.b	mt32_id(pc),d3		*DEFAULTを使う
@@:
	lea	header+2(pc),a2
	move.b	d3,mt32_id-work(a6)
	move.b	d3,(a2)+		*set dev ID
	move.b	#$16,(a2)+		*set model id
	addq.w	#1,a2
	move.l	#$080000,d4		*base address
	swap	d3
	move.w	d3,d5		*後でﾊﾟｰｼｬﾙﾅﾝﾊﾞｰとして使う
	lsr.w	#8,d3
	subq.b	#1,d3		*make prog number 0-63
	cmpi.b	#63,d3
	bhi	t_err_69	*illegal prog number
	ext.w	d3
	ror.w	#7,d3		*d3=d3*512
	move.w	d3,d4
	subq.b	#1,d5		*1-4 -> 0-3
	cmpi.b	#3,d5
	bhi	t_err_69
	ext.w	d5
	mulu	#58,d5		*1パーシャルのパラメータ数
	add.w	#$0e,d5		*partial offset
	move.b	d5,d4
	andi.b	#$7f,d4
	add.w	d5,d5		*lsl.w	#1,d5
	andi.w	#$7f00,d5
	add.w	d5,d4		*exact address
	swap	d4
	move.b	d4,(a2)+		*set addr H
	move.b	d4,d1			*sum
	swap	d4
	move.w	d4,(a2)+		*set addr L
	add.b	d4,d1
	lsr.w	#8,d4
	add.b	d4,d1			*address sum
	move.b	d1,-(sp)
	bsr	send_header		*send header
	movem.l	d2/a1,-(sp)
	bsr	midi_trns		*data 本体を転送
	movem.l	(sp)+,d2/a1
	move.b	(sp)+,d1		*アドレスのサム
	bsr	calc_chksum
	bra	send_tail

mt32_patch:			*MT32の音色パッチパラメータ設定
	*   cmd=$2b
	* < a1.l=data address
	* < d2.l=size
	* < d3.hw=patch number(1-128)
	* < d3.lw=mt32 id number
	tst.b	d3
	bpl	@f
	move.b	mt32_id(pc),d3		*DEFAULTを使う
@@:
	lea	header+2(pc),a2
	move.b	d3,mt32_id-work(a6)
	move.b	d3,(a2)+		*set dev ID
	move.b	#$16,(a2)+		*set model id
	addq.w	#1,a2
	move.l	#$050000,d4		*base address
	swap	d3
	subq.b	#1,d3		*make patch number 0-127
	bmi	t_err_69	*illegal patch number
	ext.w	d3
	lsl.w	#3,d3		*d3=d3*8
	move.b	d3,d4
	andi.b	#$7f,d4
	add.w	d3,d3		*lsl.w	#1,d3
	andi.w	#$7f00,d3
	add.w	d3,d4			*exact address
	swap	d4
	move.b	d4,(a2)+		*set addr H
	move.b	d4,d1			*sum
	swap	d4
	move.w	d4,(a2)+		*set addr L
	add.b	d4,d1
	lsr.w	#8,d4
	add.b	d4,d1			*address sum
	move.b	d1,-(sp)
	bsr	send_header		*send header
	movem.l	d2/a1,-(sp)
	bsr	midi_trns		*data 本体を転送
	movem.l	(sp)+,d2/a1
	move.b	(sp)+,d1		*アドレスのサム
	bsr	calc_chksum
	bra	send_tail

mt32_print:
	*   cmd=$2c
	* < a1.l=data address
	* < d2.l=size
	* < d3.b=mt32 id number
	cmpi.l	#20,d2
	bhi	t_err_69
	tst.b	d3
	bpl	@f
	move.b	mt32_id(pc),d3		*DEFAULTを使う
@@:
	lea	header+2(pc),a2
	move.b	d3,mt32_id-work(a6)
	move.b	d3,(a2)+		*set dev ID
	move.b	#$16,(a2)+		*set model id
	addq.w	#1,a2
	move.b	#$20,(a2)+
	clr.w	(a2)+
	bsr	send_header
	movem.l	d2/a1,-(sp)
	bsr	midi_trns
	movem.l	(sp)+,d2/a1
	moveq.l	#$20,d1			*アドレスのサム
	bsr	calc_chksum
	bra	send_tail

u220_setup:			*U220のセットアップパラメータ設定
	*   cmd=$2d
	* < a1.l=data address
	* < d2.l=size
	* < d3.b=u220 id number
	tst.b	d3
	bpl	@f
	move.b	u220_id(pc),d3		*DEFAULTを使う
@@:
	lea	header+2(pc),a2
	move.b	d3,u220_id-work(a6)
	move.b	d3,(a2)+		*set dev ID
	move.b	#$2b,(a2)+		*set model id
	addq.w	#1,a2
	clr.b	(a2)+			*set addr H
	clr.w	(a2)+			*set addr L
	bsr	send_header
	lea	v_buffer(pc),a2
	move.l	#$0e07_0a00,(a2)+	*440.0Hz,LCD=10
	move.b	(a1)+,d0		*chorus sw
	add.b	d0,d0			*lsl.b	#1,d0
	or.b	(a1)+,d0		*reverb sw
	move.b	d0,-1(a2)
	moveq.l	#4,d3		*size
	subq.l	#2,d2
	beq	trns_u2st		*もうデータはない
	bmi	t_err_69		*データの個数が異常
	clr.l	(a2)+			*dummy data
	move.b	(a1)+,d0	*Rx_ctrl channel
	subq.b	#1,d0
	cmpi.b	#$0f,d0
	bls	@f
	moveq.l	#$10,d0		*規定外はOFFとみなす
@@:
	bsr	stu2_dt
	moveq.l	#10,d3
	subq.l	#1,d2
	beq	trns_u2st	*Rx_ctrl channelだけ設定したい( > d2.l=0)
	clr.b	(a2)+		*dummy data
	clr.b	(a2)+
	moveq.l	#12,d3
	subq.l	#1,d2		*for dbra
@@:
	move.b	(a1)+,(a2)+	*patch change,timbre change,rythm change,Rx.R.inst Assign
	addq.l	#1,d3
	dbra	d2,@b
trns_u2st:
	move.l	d3,d2		*d2=new size
	move.l	d2,-(sp)
	lea	v_buffer(pc),a1
	bsr	midi_trns
	move.l	(sp)+,d2
	moveq.l	#0,d1		*address sum
	lea	v_buffer(pc),a1
	bsr	calc_chksum
	bra	send_tail

stu2_dt:			*set data
	* < a2.l=buffer address
	* < d0.b=data
	move.b	d0,-(sp)
	andi.b	#$0f,d0
	move.b	d0,(a2)+
	move.b	(sp)+,d0
	lsr.b	#4,d0
	move.b	d0,(a2)+
	rts

stu2_dt2:			*set data その２
	* < a2.l=buffer address
	* < d0.w=data
	bsr	stu2_dt
	lsr.w	#8,d0
	bra	stu2_dt

u220_common:			*U220のﾃﾝﾎﾟﾗﾘｰﾊﾟｯﾁのｺﾓﾝﾊﾟﾗﾒｰﾀ設定
	*   cmd=$2e
	* < a1.l=data address
	* < d2.l=size
	* < d3.b=u220 id number
	tst.b	d3
	bpl	@f
	move.b	u220_id(pc),d3		*DEFAULTを使う
@@:
	lea	header+2(pc),a2
	move.b	d3,u220_id-work(a6)
	move.b	d3,(a2)+		*set dev ID
	move.b	#$2b,(a2)+		*set model id
	addq.w	#1,a2
	clr.b	(a2)+			*set addr H
	move.w	#$0618,(a2)+		*set addr L
	bsr	send_header
	lea	v_buffer(pc),a2
	moveq.l	#0,d0
	move.b	4(a1),d0	*cho rate
	moveq.l	#0,d1
	move.b	2(a1),d1	*cho level
	lsl.w	#6,d1
	or.w	d1,d0
	move.b	5(a1),d1	*cho depth
	swap	d1		*lsl.l	#16,d1
	lsr.l	#5,d1
	or.w	d1,d0
	bsr	stu2_dt2

	moveq.l	#0,d0
	move.b	8(a1),d0	*rev time
	moveq.l	#0,d1
	move.b	6(a1),d1	*cho FB
	lsl.w	#7,d1
	or.w	d1,d0
	move.b	(a1),d1		*cho type
	swap	d1
	lsr.l	#3,d1
	or.w	d1,d0
	bsr	stu2_dt2

	moveq.l	#0,d0
	move.b	3(a1),d0	*cho delay time
	moveq.l	#0,d1
	move.b	10(a1),d1	*rev FB
	swap	d1
	lsr.l	#5,d1
	or.w	d1,d0
	bsr	stu2_dt2

	moveq.l	#0,d0
	move.b	9(a1),d0	*rev level
	moveq.l	#0,d1
	move.b	11(a1),d1	*pre rev dly time
	lsl.w	#6,d1
	or.w	d1,d0
	move.b	7(a1),d1	*rev type
	swap	d1
	lsr.l	#4,d1
	or.w	d1,d0
	add.w	d0,d0		*lsl.w	#1,d0
	move.b	1(a1),d1	*out mode
	lsr.b	#1,d1
	roxr.w	#1,d0
	bsr	stu2_dt2

	lea	12(a1),a1
	moveq.l	#3-1,d3
@@:
	move.b	(a1)+,d0	*ctrl #1-3
	ror.w	#8,d0
	move.b	(a1)+,d0	*parameter #1-3
	ror.w	#8,d0
	bsr	stu2_dt2
	dbra	d3,@b

	moveq.l	#28,d2
	lea	v_buffer(pc),a1
	bsr	midi_trns
	moveq.l	#28,d2
	lea	v_buffer(pc),a1
	moveq.l	#$1e,d1		*address sum
	bsr	calc_chksum
	bra	send_tail

u220_d_setup:			*U220のﾃﾝﾎﾟﾗﾘｰﾊﾟｯﾁの(DRUM)ﾊﾟﾗﾒｰﾀ設定
	*   cmd=$2f
	* < a1.l=data address
	* < d2.l=size
	* < d3.b=u220 id number
	tst.b	d3
	bpl	@f
	move.b	u220_id(pc),d3		*DEFAULTを使う
@@:
	lea	header+2(pc),a2
	move.b	d3,u220_id-work(a6)
	move.b	d3,(a2)+		*set dev ID
	move.b	#$2b,(a2)+		*set model id
	addq.w	#1,a2
	clr.b	(a2)+			*set addr H
	move.w	#$0634,(a2)+		*set addr L
	bsr	send_header
	lea	v_buffer(pc),a2
	move.b	1(a1),d0	*rythm v rsv
	move.b	(a1),d1		*rythm set number
	lsl.b	#5,d1
	or.b	d1,d0
	bsr	stu2_dt

	move.b	2(a1),d0	*rythm part ch
	subq.b	#1,d0
	cmpi.b	#$0f,d0
	bls	@f
	moveq.l	#$10,d0		*規定外の時はOFFとみなす
@@:
	move.b	6(a1),d1	*rythm part Rx.HOLD
	lsl.b	#5,d1
	or.b	d1,d0
	move.b	5(a1),d1	*rythm part Rx.VOLUME
	lsl.b	#6,d1
	or.b	d1,d0
	bsr	stu2_dt

	move.b	3(a1),d0	*rythm part level
	move.b	4(a1),d1	*rythm part boost sw
	lsl.b	#7,d1
	or.b	d1,d0
	bsr	stu2_dt
	clr.w	(a2)		*dummy

	moveq.l	#8,d2
	lea	v_buffer(pc),a1
	bsr	midi_trns
	moveq.l	#8,d2
	lea	v_buffer(pc),a1
	moveq.l	#$3a,d1		*address sum
	bsr	calc_chksum
	bra	send_tail

u220_p_setup:			*U220のﾃﾝﾎﾟﾗﾘｰﾊﾟｯﾁの(PART)ﾊﾟﾗﾒｰﾀ設定
	*   cmd=$30
	* < a1.l=data address
	* < d2.l=size
	* < d3.hw=part number(1～6)
	* < d3.lw=u220 id number
	tst.b	d3
	bpl	@f
	move.b	u220_id(pc),d3		*DEFAULTを使う
@@:
	lea	header+2(pc),a2
	move.b	d3,u220_id-work(a6)
	move.b	d3,(a2)+	*set dev ID
	move.b	#$2b,(a2)+	*set model id
	addq.w	#1,a2
	moveq.l	#$3c,d4		*base address
	swap	d3
	subq.b	#1,d3		*make 1-6 -> 0-5
	cmpi.b	#5,d3
	bhi	t_err_69	*illegal part number
	ext.w	d3
	lsl.w	#4,d3		*d3=d3*16
	add.w	d3,d4
	move.l	d4,d3
	andi.b	#$7f,d3
	add.w	d4,d4		*lsl.w	#1,d4
	andi.w	#$7f00,d4
	move.b	d3,d4
	ori.w	#$0600,d4	*d4=exact address
	swap	d4
	move.b	d4,(a2)+	*set addr H
	move.b	d4,d1		*sum
	swap	d4
	move.w	d4,(a2)+	*set addr L
	add.b	d4,d1
	lsr.w	#8,d4
	add.b	d4,d1		*address sum
	move.b	d1,sc_p_data-work(a6)	*push address sum
	bsr	send_header
	lea	v_buffer(pc),a2
	move.b	(a1),d0		*timbre number
	subq.b	#1,d0		*1～128→0～127
	bmi	t_err_69	*illegal timbre number
	move.b	10(a1),d1	*Rx.volume
	lsl.b	#7,d1
	or.b	d1,d0
	bsr	stu2_dt

	move.b	1(a1),d0	*v reserve
	move.b	7(a1),d1	*out assign
	lsl.b	#5,d1
	or.b	d1,d0
	bsr	stu2_dt

	moveq.l	#0,d0
	move.b	2(a1),d0	*midi ch
	subq.b	#1,d0
	cmpi.b	#$0f,d0
	bls	@f
	moveq.l	#$10,d0		*規定外OFFとみなす
@@:
	moveq.l	#0,d1
	move.b	8(a1),d1	*part level
	lsl.w	#5,d1
	or.w	d1,d0
	move.b	9(a1),d1	*part pan
	swap	d1
	lsr.l	#4,d1
	or.w	d1,d0
	bsr	stu2_dt2	*2bytes書き込み

	move.b	3(a1),d0	*k.range low
	move.b	12(a1),d1	*Rx.HOLD
	lsl.b	#7,d1
	or.b	d1,d0
	bsr	stu2_dt

	move.b	4(a1),d0	*k.range hi
	move.b	11(a1),d1	*Rx.PAN
	lsl.b	#7,d1
	or.b	d1,d0
	bsr	stu2_dt

	move.b	5(a1),d0	*velo level
	bsr	stu2_dt
	move.b	6(a1),d0	*velo threshold
	bsr	stu2_dt

	moveq.l	#16,d2
	lea	v_buffer(pc),a1
	bsr	midi_trns
	moveq.l	#16,d2
	lea	v_buffer(pc),a1
	move.b	sc_p_data(pc),d1	*get back address sum
	bsr	calc_chksum
	bra	send_tail

u220_print:
	*   cmd=$31
	* < a1.l=data address
	* < d2.l=size
	* < d3.b=u220 id number
	cmpi.l	#12,d2
	bhi	t_err_69
	tst.b	d3
	bpl	@f
	move.b	u220_id(pc),d3		*DEFAULTを使う
@@:
	lea	header+2(pc),a2
	move.b	d3,u220_id-work(a6)
	move.b	d3,(a2)+		*set dev ID
	move.b	#$2b,(a2)+		*set model id
	addq.w	#1,a2
	clr.b	(a2)+			*address H
	move.w	#$0600,(a2)+		*address L
	bsr	send_header
	lea	v_buffer(pc),a2
	move.l	d2,d3
	subq.l	#1,d3		*for dbra
	bmi	t_err_69
@@:
	move.b	(a1)+,d0
	bsr	stu2_dt
	dbra	d3,@b
	moveq.l	#12-1,d1
	sub.w	d2,d1
	bmi	send_u2pr
@@:
	moveq.l	#$20,d0		*SPC
	bsr	stu2_dt
	dbra	d1,@b
send_u2pr:
	moveq.l	#24,d2
	lea	v_buffer(pc),a1
	bsr	midi_trns
	moveq.l	#24,d2
	lea	v_buffer(pc),a1
	moveq.l	#$06,d1		*address sum
	bsr	calc_chksum
	bra	send_tail

u220_timbre:			*U220の音色パラメータ設定
	*   cmd=$32
	* < a1.l=data address
	* < d2.l=size
	* < d3.hw=program number(1-128)
	* < d3.lw=u220 id number
	tst.b	d3
	bpl	@f
	move.b	u220_id(pc),d3		*DEFAULTを使う
@@:
	lea	header+2(pc),a2
	move.b	d3,u220_id-work(a6)
	move.b	d3,(a2)+		*set dev ID
	move.b	#$2b,(a2)+		*set model id
	addq.w	#1,a2
	swap	d3
	subq.b	#1,d3		*make prog number 0-127
	bmi	t_err_69	*illegal prog number
	ext.w	d3
	lsl.w	#6,d3
	move.w	d3,d4
	andi.b	#$7f,d3
	add.l	d4,d4		*lsl.l	#1,d4
	move.b	d3,d4
	add.l	#$020000,d4	*d4=exact address
	swap	d4
	move.b	d4,(a2)+		*set addr H
	move.b	d4,d1			*sum
	swap	d4
	move.w	d4,(a2)+		*set addr L
	add.b	d4,d1
	lsr.w	#8,d4
	add.b	d4,d1			*address sum
	move.b	d1,sc_p_data-work(a6)	*ワークとして使う
	bsr	send_header		*send header
	lea	v_buffer(pc),a2
	moveq.l	#0,d3
@@:
	addq.l	#1,d3		*文字数カウント
	move.b	(a1)+,d0
	beq	@f
	bsr	stu2_dt
	bra	@b
@@:
	moveq.l	#12+1,d1
	sub.l	d3,d1
	beq	trns_cmp_u	*12文字丁度:他のパラメータの処理に
	bmi	t_err_69	*12文字以上:エラー
	subq.b	#1,d1		*for dbra
@@:
	moveq.l	#$20,d0
	bsr	stu2_dt		*足りない分をスペースで埋める
	dbra	d1,@b
trns_cmp_u:
	sub.l	d3,d2
	bmi	t_err_69	*異常自体発生
	beq	send_cmdt_u

	moveq.l	#0,d0
	move.b	1(a1),d0	*tone #
	subq.b	#1,d0
	bmi	t_err_69	*トーンナンバー異常
	moveq.l	#0,d1
	move.b	(a1),d1		*tone Meida
	lsl.w	#7,d1
	or.w	d1,d0
	move.b	17(a1),d1	*detune depth
	swap	d1
	lsr.l	#4,d1
	or.w	d1,d0
	bsr	stu2_dt2

	move.b	3(a1),d0	*level velo sens
	move.b	4(a1),d1	*level ch aft
	lsl.b	#4,d1
	or.b	d1,d0
	bsr	stu2_dt
	move.b	2(a1),d0	*timbre level
	bsr	stu2_dt

	move.b	5(a1),d0	*Env A
	move.b	6(a1),d1	*Env D
	lsl.b	#4,d1
	or.b	d1,d0
	bsr	stu2_dt
	move.b	7(a1),d0	*Env S
	move.b	8(a1),d1	*Env R
	lsl.b	#4,d1
	or.b	d1,d0
	bsr	stu2_dt

	move.b	10(a1),d0	*Pitch fine
	bsr	stu2_dt
	move.b	09(a1),d0	*Pitch coarse
	bsr	stu2_dt

	moveq.l	#0,d0
	move.b	11(a1),d0	*Bend lower
	moveq.l	#0,d1
	move.b	12(a1),d1	*Bend upper
	lsl.w	#5,d1
	or.w	d1,d0
	move.b	15(a1),d1	*Auto Bend depth
	swap	d1
	lsr.l	#7,d1
	or.w	d1,d0
	bsr	stu2_dt2

	moveq.l	#0,d0
	move.b	14(a1),d0	*Pitch poly aft
	moveq.l	#0,d1
	move.b	13(a1),d1	*Pitch ch aft
	lsl.w	#5,d1
	or.w	d1,d0
	move.b	16(a1),d1	*Auto Bend rate
	swap	d1
	lsr.l	#6,d1
	or.w	d1,d0
	bsr	stu2_dt2

	move.b	23(a1),d0	*Mod depth
	lsl.b	#4,d0
	bsr	stu2_dt
	move.b	21(a1),d0	*Vib delay
	move.b	20(a1),d1	*Vib depth
	lsl.b	#4,d1
	or.b	d1,d0
	bsr	stu2_dt

	move.b	18(a1),d0	*Vib rate
	bsr	stu2_dt
	move.b	19(a1),d0	*Vib WF
	bsr	stu2_dt

	move.b	22(a1),d0	*Vib rise
	bsr	stu2_dt
	move.b	24(a1),d0	*Vib ch aft
	move.b	25(a1),d1	*Vib poly aft
	lsl.b	#4,d1
	or.b	d1,d0
	bsr	stu2_dt
	moveq.l	#0,d0		*Dummy
	bsr	stu2_dt2
send_cmdt_u:
	moveq.l	#$40,d2			*データ総バイト数
	lea	v_buffer(pc),a1
	bsr	midi_trns		*data 本体を転送
	move.b	sc_p_data(pc),d1	*アドレスのサム
	moveq.l	#$40,d2			*データ総バイト数
	lea	v_buffer(pc),a1
	bsr	calc_chksum
	bra	send_tail

u220_drum:			*U220のﾃﾝﾎﾟﾗﾘｴﾘｱのﾄﾞﾗﾑの音色を変更する
	*   cmd=$33
	* < a1.l=data address
	* < d2.l=size
	* < d3.hw=patch number(35～99)
	* < d3.lw=u220 id number
	tst.b	d3
	bpl	@f
	move.b	u220_id(pc),d3		*DEFAULTを使う
@@:
	lea	header+2(pc),a2
	move.b	d3,u220_id-work(a6)
	move.b	d3,(a2)+		*set dev ID
	move.b	#$2b,(a2)+		*set model id
	addq.w	#1,a2
	move.l	#$110000,d4		*base address
	swap	d3
	cmpi.b	#35,d3
	bcs	t_err_69
	cmpi.b	#99,d3
	bhi	t_err_69		*ノートナンバーが規定外
	ext.w	d3
	lsl.w	#8,d3		*d3=d3*256
	add.w	d3,d4			*exact address
	swap	d4
	move.b	d4,(a2)+		*set addr H
	move.b	d4,d1			*sum
	swap	d4
	move.w	d4,(a2)+		*set addr L
	add.b	d4,d1
	lsr.w	#8,d4
	add.b	d4,d1			*address sum
	move.b	d1,sc_p_data-work(a6)	*ワークとして使う
	bsr	send_header		*send header
	move.b	1(a1),d0
	subq.b	#1,d0
	bmi	t_err_69		*トーンナンバー異常
	move.b	1(a1),-(sp)		*save parameter
	move.b	d0,1(a1)
	movem.l	d2/a1,-(sp)
	bsr	midi_trns		*data 本体を転送
	movem.l	(sp)+,d2/a1
	move.b	sc_p_data(pc),d1	*アドレスのサム
	bsr	calc_chksum
	move.b	(sp)+,1(a1)		*back parameter
	bra	send_tail

m1_p_buffer:	equ	v_buffer+$08
m1_midi_ch:			*M1の受信MIDI CHの設定
	*   cmd=$34
	* < a1.l=data address
	lea	m1_p_buffer(pc),a2
	lea	m1_p_buffer+8(pc),a3
	moveq.l	#8-1,d2		*dbra count
m1_md_lp:
	moveq.l	#03,d1		*ON
	move.b	(a1)+,d0
	subq.b	#1,d0
	cmpi.b	#$0f,d0
	bls	@f
	moveq.l	#0,d0		*一応チャンネル１にする
	moveq.l	#0,d1		*OFF
@@:
	move.b	d0,(a2)+
	move.b	d1,(a3)+
	dbra	d2,m1_md_lp
	bra	t_dat_ok

send_to_m1:			*M1へパラメータを書き込む
	*   cmd=$35
	* < d3.b=m1 id number
	tst.b	d3
	bpl	@f
	move.b	m1_id(pc),d3		*DEFAULTを使う
@@:
	move.b	d3,m1_id-work(a6)
	lea	v_buffer(pc),a1
	move.l	#$f0420019,(a1)+	*header&maker ID,dummy,M1
	move.b	d3,-2(a1)		*ID
	move.l	#$48_00_0000,(a1)+	*cmd,bank,seq data size
	move.b	#$04,16(a1)		*beat
	move.b	#$78,17(a1)		*tempo/protect
	move.b	#$14,19(a1)		*next song
	clr.b	30(a1)			*nul
	lea	m1_ef_dflt+25(pc),a2
	moveq.l	#25-1,d0
@@:
	move.b	-(a2),31(a1,d0.w)	*エフェクトデータのセット
	dbra	d0,@b

	moveq.l	#96-1,d0			*データ列のならび変え
@@:
	move.l	d0,d1
	divu.w	#7,d1
	add.w	d0,d1
	move.b	(a1,d0.w),1(a1,d1.w)
	dbra	d0,@b

	moveq.l	#0,d2			*MIDIデータへの変換
stmlp:
	moveq.l	#7-1,d1
	moveq.l	#0,d3
@@:
	move.b	1(a1,d1.w),d0
	andi.b	#$7f,1(a1,d1.w)
	lsl.b	#1,d0
	roxl.b	#1,d3
	dbra	d1,@b
	move.b	d3,(a1)
	addq.w	#8,a1
	addq.w	#7,d2
	cmpi.w	#96,d2
	bls	stmlp
@@:
	lea	v_buffer(pc),a1
	move.b	#$f7,$76(a1)		*EOX
	moveq.l	#$77,d2
	bsr	midi_trns

	lea	v_buffer(pc),a1
	move.l	#$f0420019,(a1)		*header&maker ID,dummy,M1
	move.b	m1_id(pc),2(a1)
	move.l	#$4e0600f7,4(a1)	*cmd,mode,bank,EOX
	moveq.l	#$8,d2
	bra	midi_trns

m1_p_setup:			*M1のSEQ SONG0の設定
	*   cmd=$36
	* < a1.l=data address
	lea	m1_p_buffer+56(pc),a2
	moveq.l	#40-1,d2
@@:
	move.b	(a1)+,(a2)+
	dbra	d2,@b
	bra	t_dat_ok

m1_e_setup:			*M1のSEQ SONG0のEFFECTの設定
	*   cmd=$37
	* < a1.l=data address
	lea	m1_ef_dflt(pc),a2
	moveq.l	#25-1,d2	*=dbra counter
@@:
	move.b	(a1)+,(a2)+
	dbra	d2,@b
	bra	t_dat_ok

m1_print:			*M!のSEQ SONG0のNAME設定
	*   cmd=$38
	* < a1.l=data address
	* < d2.l=size
	cmpi.l	#10,d2
	bhi	t_err_69
	lea	m1_p_buffer+20(pc),a2
	move.l	d2,d3
	subq.l	#1,d3		*for dbra
	bmi	t_err_69
@@:
	move.b	(a1)+,(a2)+
	dbra	d3,@b
	moveq.l	#10-1,d3
	sub.w	d2,d3
	bmi	t_dat_ok	*10文字丁度
@@:
	move.b	#$20,(a2)+	*SPC
	dbra	d3,@b
	bra	t_dat_ok

skip_peri:
	* < a0.l=filename addr
@@:
	cmpi.b	#'.',(a0)
	bne	@f
	addq.w	#1,a0
	bra	@b
@@:
	rts

kakuchoshi:			*拡張子を設定
	* < a0=filename address
	* < a1=拡張子アドレス
	* X a0
	bsr	skip_peri
	moveq.l	#91-1,d0
kkchs_lp:
	move.b	(a0)+,d0
	beq	do_kkchs
	cmpi.b	#'.',d0
	beq	find_period
	dbra	d0,kkchs_lp
do_kkchs:
	subq.l	#1,a0
	move.b	#'.',(a0)+
	move.b	(a1)+,(a0)+
	move.b	(a1)+,(a0)+
	move.b	(a1)+,(a0)+
	clr.b	(a0)
	rts
find_period:
	cmpi.b	#' ',(a0)
	bls	do_kkchs	*'.'はあっても拡張子がないケース
	rts

clr_adpb?:
	tst.b	adpb_clr-work(a6)
	beq	@f
_clr_adpb:
	move.l	adpcm_buffer_top(pc),adpcm_buffer_next-work(a6)
	clr.b	adpb_clr-work(a6)
@@:
	rts

adpcm_block_data:		*ADPCMブロックデータの取り込み
	*   cmd=$39
	* < a1.l=filename address
				*拡張子が無い時は'ZPD'を自動的に付ける
	lea	filename(pc),a2
@@:
	move.b	(a1)+,(a2)+
	bne	@b
	lea	filename(pc),a0
	lea	ZPD(pc),a1
	bsr	kakuchoshi

	lea	filename(pc),a2
	bsr	fopen		*(ret:d5=file handle)
	tst.l	d5		*d5=file_handle
	bmi	t_err_62	*read error

	bsr	get_filedate
	move.l	date_buf(pc),d6
	exg.l	d0,d6
	cmp.l	d0,d6
	bne	@f
	bsr	fname_chk
	bne	@f
	bsr	do_fclose
	clr.b	adpb_clr-work(a6)
	bra	t_dat_ok	*同じものは読まない
@@:
	bsr	clr_adpb?
 	bsr	get_fsize	*>d3.l=file size
	bmi	t_err_62
	move.l	d3,d2		*d2=total data size
	addq.l	#1,d3
	bclr.l	#0,d3

	move.l	adpcm_buffer_next(pc),a2
	move.l	adpcm_buffer_end(pc),d1
	sub.l	a2,d1		*d1=vacant size
	cmp.l	d1,d3
	bhi	t_err_60	*pcm buffer is not enough

	clr.b	last_fn-work(a6)	*ファイル名バッファ除去
	cmp.l	adpcm_buffer_top(pc),a2
	bne	@f
	bsr	adpcm_tbl_init	*adpcm_tblの初期化
@@:
	move.l	d2,-(sp)	*push size
	move.l	a2,-(sp)	*push addr
	move.w	d5,-(sp)	*file handle
	DOS	_READ		*サンプリングデータの読み込み
	lea	10(sp),sp
	cmp.l	d0,d2
	bne	t_err_62	*read error

	bsr	do_fclose

	moveq.l	#-1,d0		*エラーと知らせるためのマーク
	cmpi.l	#$105a6d41,(a2)+	*HEADER check
	bne	play_beep	*zmusicには無縁のデータです
	cmpi.l	#$6470436d,(a2)+
	bne	play_beep	*zmusicには無縁のデータです
	lea	-8(a2,d3.l),a0
	move.l	a0,adpcm_buffer_next-work(a6)
	move.l	d2,zpd_size-work(a6)
	move.l	d6,date_buf-work(a6)
	bsr	fn_to_lastfn	*ファイルネーム保存
	bra	abd_lp00
set_adpcm_tbl:
	bsr	adpcm_tbl_init	*adpcm_tblの初期化
set_adpcm_tbl_:
abd_lp00:
	moveq.l	#0,d0
	move.w	(a2)+,d0
	bmi	t_dat_ok	*all end
	cmp.l	adpcm_n_max(pc),d0
	bcc	t_err_38
	lsl.l	#3,d0
	move.l	adpcm_tbl(pc),a0
	add.l	d0,a0		*ワークアドレス
	move.l	(a2)+,d1
	lea	(a2,d1.l),a3
	move.l	a3,(a0)+	*address
	move.l	(a2)+,(a0)+	*size
	bra	abd_lp00

get_trk_tbl:
	*   cmd=$3a
	* > d0.l=real ch tbl
	* > a0.l=play trk tbl
	move.l	#real_ch_tbl,d0
	lea	play_trk_tbl(pc),a0
	rts

set_loop_time:			*ループ回数とジャンプ先の設定
	*   cmd=$3b
	* < a1.l=ジャンプ先
	* < d2.l=loop time(s)
	ori.w	#$700,sr
	lea	done_bit(pc),a0
	move.b	d2,loop_time-done_bit(a0)
	move.l	a1,loop_bsr-done_bit(a0)
	sne	loop_chk-done_bit(a0)
	moveq.l	#0,d0
	move.b	d0,loop_chk_trk-done_bit(a0)
	move.l	d0,(a0)+	*0-31
*	move.l	d0,(a0)+	*32-63
*	move.w	d0,(a0)+	*64-79
	rts			*d0=0

get_play_work:			*演奏ワークアドレスを返す
	*   cmd=$3c
	* < d2.l=trk number(1-80)
	tst.b	d2
	beq	@f
	subq.b	#1,d2		*0-79
@@:
	moveq.l	#0,d0
	move.b	d2,d0
	bsr	calc_wk
	movea.l	a5,a0
*	bsr	calc_cnv_wk
*	move.l	a1,d0
	bra	t_dat_ok

get_tm_mode:			*どのタイマーを使用しているか
	*   cmd=$3d
	tst.b	timer_a_mode-work(a6)
	bne	t_dat_ok	*timer_a->d0.l=0
	moveq.l	#1,d0		*timer_b->d0.l=1
	rts

set_fm_master_vol:		*ＦＭマスターボリュームを設定する
	*   cmd=$3e
	* < d2.b=master voluem value(0-255)

	bra	t_dat_ok

set_timer_value:		*タイマーの値を設定する
	*   cmd=$3f
	* < d2.w=timer value
	lea	timer_value(pc),a1
	move.w	d2,(a1)+	*わざと(a1)+
	bsr	gyakusan_t
	bsr	init_timer
	bra	t_dat_ok

spt_size:	equ	100	*登録名サイズ
release_support:		*サポートプログラムの解除
	*   cmd=$40
	* < a1.l=filename address
	* < d2.l=result code(キャンセル時に使用)
	* > d0.l=result code(-1:error)
	move.l	a1,d0		*d0に意味無し
	bne	spt_set_mode

				*キャンセル
	cmpi.l	#sp_max-1,d2
	bhi	rs_er
	lea	support_mode(pc),a0
	clr.b	(a0,d2.l)	*cansel
	bra	t_dat_ok
spt_set_mode:			*登録
	lea	support_mode+sp_max(pc),a0
	moveq.l	#sp_max-1,d1
@@:
	tst.b	-(a0)
	beq	@f
	dbra	d1,@b
rs_er:
	moveq.l	#-1,d0		*error
	rts
@@:
	st.b	(a0)		*mark
	move.l	d1,d0
	mulu	#spt_size,d1
	lea.l	support_name(pc),a0
	add.w	d1,a0
	moveq.l	#spt_size-1,d1
@@:
	move.b	(a1)+,(a0)+
	dbra	d1,@b
	rts

*jump_active:			*［！］コマンドの有効／無効化
*	*   cmd=$41
*	* < d2.b=0   off
*	* < d2.b=ne  on
*	move.b	d2,act_flg-work(a6)
*	bra	t_dat_ok

set_mclk:			*全音符の絶対音長設定
	*   cmd=$42
	* < d2.l=全音符の絶対音長
	cmpi.l	#254,d2
	bhi	t_err_70
	move.b	d2,mclk-work(a6)
	move.l	#16*4000*60000,d1
	lsl.l	#8,d2		*d2=d2*1024/4=d2*256
	bsr	wari		*d1=d1/d2
	move.l	d1,tmp_base-work(a6)
	bra	t_dat_ok

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

picture_sync:
	*   cmd=$43
	* < d2.l=mode switch
	ori.w	#$0700,sr
	tst.l	d2
	beq	ps_off
	tst.b	ps_flg-work(a6)
	bne	ps_err
	bsr	set_clc_patch2
	moveq.l	#tr_max-1,d0
@@:
	bsr	calc_wk
	clr.l	p_total(a5)
	clr.l	p_total_olp(a5)
	dbra	d0,@b
	st.b	ps_flg-work(a6)
	bra	t_dat_ok
ps_off:
	tst.b	ps_flg-work(a6)
	beq	ps_err
	bsr	back_patch
	cache_flush
	clr.b	ps_flg-work(a6)
	bra	t_dat_ok
ps_err:
	moveq.l	#-1,d0
	rts

mask_channels:
	*   cmd=$44
	* < d2.l=bit pattern of mask channels
	*	1=disable,0=enable
				*cp!!
	ori.w	#$700,sr	*割り込み禁止
	moveq.l	#0,d0
	move.l	d0,done_flg-work(a6)
	move.b	d0,ch_tr_msk-work(a6)
	bsr	rev_ch
	move.w	#NOP,d0		*ALL ENABLEのケース
	move.l	d2,mask_wk-work(a6)
	beq	@f
	move.w	#BRA+(goto_m_int_lp.and.$ff),d0	*１チャンネルでもマスクする場合
	st.b	ch_tr_msk-work(a6)
@@:
	move.w	d0,se_ope-work(a6)
	moveq.l	#ch_max-1,d5
mskc_lp00:
	lea	play_trk_tbl(pc),a0
mskc_lp01:
	move.b	(a0)+,d0
	bmi	mskc_next
	bsr	calc_wk
	cmp.b	p_ch(a5),d5	*check ch
	bne	mskc_lp01
	move.l	d5,d1
	cmpi.b	#8,d1
	bne	en_or_di
npc8_2:				*npc8!
	tst.b	p_extra_ch(a5)
	beq	en_or_di
	move.b	p_extra_ch(a5),d1
	add.b	#24,d1
en_or_di:
	btst.l	d1,d2
	bne	di_ch
	bsr	en_msk
	bra	mskc_lp01
di_ch:
	bsr	do_msk
	bra	mskc_lp01
mskc_next:
	dbra	d5,mskc_lp00
	bra	t_dat_ok
en_msk:					*マスク解除
	tst.b	p_se_mode(a5)
	bmi	@f			*既に有効
	st.b	p_se_mode(a5)		*ダミー解除
	clr.b	p_waon_mark(a5)
*	moveq.l	#0,d0			*音色等の設定のため
*	move.b	p_pgm(a5),d0
*	bsr	init_inst
@@:
	rts
do_msk:					*マスク
	tst.b	p_se_mode(a5)
	bpl	@b			*既に無効(または効果音モード!?)
	move.b	#1,p_se_mode(a5)	*ダミ－
	bra	ms_key_off

buffer_info:			*各バッファのインフォメーション
	*   cmd=$45
	* > 0(a0)=track buffer top
	* > 4(a0)=track buffer size
	* > 8(a0)=track buffer end
	* > 12(a0)=ADPCM buffer top
	* > 16(a0)=ADPCM buffer size
	* > 20(a0)=ADPCM buffer end
	* > 24(a0)=work area top
	* > 28(a0)=work area size
	* > 32(a0)=work area end
	lea	trk_top(pc),a0
	bra	t_dat_ok

set_zpd_tbl:			*ＺＰＤデータのテーブルセット
	*   cmd=$46
	* < a1.l=data address (ヘッダの次から)
	clr.b	last_fn-work(a6)
	movea.l	a1,a2
	bra	set_adpcm_tbl_

set_output_level:		*各チャンネルの出力レベルの設定
	*   cmd=$47
	* < d2.l=bit pattern of channels
	* < d3.l=0～127 (出力レベル)
	*	=minus  (NORMALに戻す)
	* > d0.l=ちゃんと設定が行なわれると対応ビット=1が返される
	ori.w	#$700,sr	*割り込み禁止
	st.b	ch_tr_opl-work(a6)
	bsr	rev_ch
	moveq.l	#0,d4		*戻り値用
	tst.b	d3
	bpl	@f
	moveq.l	#$80,d3		*マイナス値はすべて128へ
	cmpi.l	#-1,d2
	bne	@f
	move.b	d4,ch_tr_opl-work(a6)	*完全に通常に戻るケース
@@:
	move.b	d3,outlvl-work(a6)
	move.l	d2,out_wk-work(a6)
	moveq.l	#ch_max-1,d5
sopl_lp01:
	lea	play_trk_tbl(pc),a0
sopl_lp02:
	move.b	(a0)+,d0
	bmi	sopl_next
	bsr	calc_wk
	cmp.b	p_ch(a5),d5	*check ch
	bne	sopl_lp02
	move.l	d5,d1
	cmpi.b	#8,d1
	bne	sopl_chk
npc8_3:				*npc8!
	tst.b	p_extra_ch(a5)
	beq	sopl_chk
	move.b	p_extra_ch(a5),d1
	add.b	#24,d1
sopl_chk:
	btst.l	d1,d2
	beq	sopl_lp02
	bsr	st_otl
	bra	sopl_lp02
sopl_next:
	dbra	d5,sopl_lp01
	move.l	d4,d0
	rts
st_otl:					*出力レベルセット
	* < d1.b=ch number
	* < d3.b=output level
	* > d4.l=mark bits
	* X d0,d4
	move.b	p_not_empty(a5),d0
	beq	@f
	cmpi.b	#$7f,d0
	bne	sotl_rts
@@:
	move.b	p_fo_mode(a5),d0
	beq	@f
	cmpi.b	#$47,d0
	bne	sotl_rts		*すでにフェードイン/アウト中
@@:
	bset.l	d1,d4			*設定したチャンネルをマーク
	st.b	p_fo_spd(a5)		*func $47であることをマーク
	move.b	d3,p_fo_lvl(a5)		*set output level
*	bmi	@f
	move.b	#$47,p_fo_mode(a5)	*mark
sotl_rts:
	rts
*@@:					*case:output level=128
*	move.b	#1,p_fo_mode(a5)
*	rts

eox_wait:
	*   cmd=$48
	* < d2.w=wait parameter
	move.w	d2,eox_w-work(a6)
	bra	t_dat_ok

set_wave_form1:
	*   cmd=$49
	* < a1.l=data address
	* < d2.l=size(countじゃなくてデータサイズ)
	* < d3.hw=wave number*256+loop type
	* < d3.lw=loop point

	lea	adpcm_work_size(pc),a3
	lea	adpcm_work_now(pc),a4
	cmp.l	(a3),d2
	bhi	t_err_64			*ワークエリアが小さすぎる
	move.l	(a4),a2
	moveq.l	#5,d1
	bsr	trans_dma
	sub.l	d2,(a3)
	add.l	d2,(a4)
	move.l	a2,a1
	lsr.l	#1,d2		*d2=d2/2

set_wave_form2:
	*   cmd=$4a
	* < a1.l=data address
	* < d2.l=count
	* < d3.hw=wave number*256+loop type
	* < d3.lw=loop point count
	tst.l	d2
	beq	t_err_76
	cmpi.l	#$ffff,d2
	bhi	t_err_76
	moveq.l	#0,d1
	move.w	d3,d1		*d0=loop point
	cmp.l	d2,d1
	bhi	t_err_76
	swap	d3
	move.w	d3,d0
	andi.w	#$ff,d0
	cmpi.b	#2,d0
	bhi	t_err_76
	lsr.w	#8,d3
	subq.w	#wv_def_max,d3
	cmpi.w	#wv_max-1,d3
	bhi	t_err_74	*wave form number error
	mulu	#14,d3
	move.l	wave_tbl(pc),a0
	adda.w	d3,a0
	move.l	a1,(a0)+	*save start addr.
	add.l	d2,d2		*count -> size
	adda.l	d2,a1
	move.l	a1,(a0)+	*save end addr.
	suba.l	d2,a1
	add.l	d1,d1
	adda.l	d1,a1
	move.l	a1,(a0)+	*save loop start addr.
	subq.w	#1,d0		*-1,0,+1
	move.w	d0,(a0)+	*set loop mode
	bra	t_dat_ok

mask_tracks:
	*   cmd=$4b
	* < d2.l=track number(1～80:on/-80～-1:off)
	*	=0 all enable
	ori.w	#$700,sr	*割り込み禁止
	moveq.l	#0,d0
	move.l	d0,done_flg-work(a6)
	move.b	d0,ch_tr_msk-work(a6)
	move.b	d2,d0
	beq	all_tr_en
	bpl	@f
	neg.b	d0
@@:
	subq.b	#1,d0
	bsr	calc_wk

	pea	chk_all_en?(pc)
	tst.b	d2
	bmi	do_msk
	bra	en_msk
all_tr_en:				*全解除
	moveq.l	#tr_max-1,d1
	movea.l	seq_wk_tbl(pc),a5
@@:
	bsr	en_msk
	lea	wk_size(a5),a5
	dbra	d1,@b
	move.w	#NOP,se_ope-work(a6)	*ALL ENABLEのケース
	bra	t_dat_ok
chk_all_en?:
	lea	play_trk_tbl(pc),a1
	moveq.l	#tr_max-1,d1
@@:
	move.b	(a1)+,d0
	bmi	@f
	bsr	calc_wk
	tst.b	p_se_mode(a5)
	bpl	cae1
	dbra	d1,@b
@@:
	move.w	#NOP,se_ope-work(a6)	*ALL ENABLEのケース
	bra	t_dat_ok
cae1:						*１チャンネルでもマスクする場合
	move.w	#BRA+(goto_m_int_lp.and.$ff),se_ope-work(a6)
	bra	t_dat_ok

set_output_level2:		*各トラックの出力レベルの設定
	*   cmd=$4c
	* < d2.l=track number(1-80)
	*	=0 (all)
	* < d3.l=0～127 (出力レベル)
	*	=minus  (NORMALに戻す)
	ori.w	#$700,sr	*割り込み禁止
	clr.b	ch_tr_opl-work(a6)
	tst.b	d3
	bpl	@f
	moveq.l	#$80,d3		*マイナス値はすべて128へ
@@:
	move.l	d2,d0
	beq	sopl2_all_nml	*全トラック正常へ
	subq.b	#1,d0
	bsr	calc_wk
	pea	t_dat_ok(pc)
	bra	st_otl		*X d4破壊
sopl2_all_nml:
	moveq.l	#tr_max-1,d1
	movea.l	seq_wk_tbl(pc),a5
@@:
	bsr	st_otl
	lea	wk_size(a5),a5
	dbra	d1,@b
	bra	t_dat_ok

get_loop_time:			*演奏トラック全体のループ回数を得る
	*   cmd=$4d
	* > d0.l=loop time
	lea	play_trk_tbl(pc),a0
	moveq.l	#-1,d0
	moveq.l	#0,d1
glt_lp:
	move.b	(a0)+,d3
	bmi	exit_glt
	move.l	seq_wk_tbl(pc),a5
	lsl.w	#wk_size2,d3
	adda.w	d3,a5
	tst.b	p_not_empty(a5)
	bne	glt_lp
	moveq.l	#0,d3
	move.b	p_do_loop_flag(a5),d3
	tst.b	d1
	beq	@f
	cmp.b	d0,d3
	bcc	glt_lp
@@:
	move.l	d3,d0
	st.b	d1
	bra	glt_lp
exit_glt:
	tst.l	d0
	bne	@f
	moveq.l	#1,d0
@@:
	rts

get_1st_comment:			*最初のコメントを取り出す
	*   cmd=$4e
	* > a0.l=loop time
	lea	first_cmt(pc),a0
	bra	t_dat_ok

int_start:			*割り込みの停止(kill driver)
	*   cmd=$4f
	bsr	m_stop_all
	ori.b	#$08,$00e88015	*MFP FM_int ON
	bra	t_dat_ok

zm_status:			*ZMUSICの常駐状況
	*   cmd=$50
	* > a0.l=status work address
	* 0(a0).b=MIDIチャンネル有効か (ne=有効,0=無効)
	* 1(a0).b=RS232C-MIDI使用中か  (ne=使用,0=未使用)
	* 2(a0).b='U'オプションによる強制MIDIチャンネル有効モードか (ne=YES,0=NO)
	* 3(a0).b=ADPCMの発音にPCM8.Xを使用中か (ne=YES,0=NO)
	* 4(a0).w=PCM8モード (minus=独立チャンネルモード,plus=ポリモード)
	* 6(a0).b=ジュークボックス類が常駐しているか？(00=no,ne=yes)
	* 7(a0).b=reserved
	* 8(a0).b:直前に読んだファイルのファイル名(ZPD,MDD,CNFなど)
	*	  ファイル名はASCII文字列。0が終端コード
	lea	zmusic_stat(pc),a0
	bra	t_dat_ok

sc55_init:			*SC55の初期化
	*   cmd=$51
	* < d3.b=sc55 id number
	tst.b	d3
	bpl	@f
	move.b	sc55_id(pc),d3		*DEFAULTを使う
@@:
	move.b	d3,sc55_id-work(a6)
	lea	header(pc),a1
	moveq.l	#11,d2
	move.b	d3,2(a1)
	move.b	#$42,3(a1)
	move.b	#$40,5(a1)
	move.l	#$007f0041,6(a1)
	bra	midi_trns

mt32_init:			*MT32の初期化
	*   cmd=$52
	* < d3.b=mt32 id number
	tst.b	d3
	bpl	@f
	move.b	mt32_id(pc),d3		*DEFAULTを使う
@@:
	move.b	d3,mt32_id-work(a6)
	lea	header(pc),a1
	moveq.l	#11,d2
	move.b	d3,2(a1)
	move.b	#$16,3(a1)
	move.b	#$7f,5(a1)
	move.l	#$00000001,6(a1)
	bra	midi_trns

*relative_uv:			*相対音量／相対ベロシティコマンドの記号交換
*	*   cmd=$53
*	* < d2.l=mode number (0=volume,1=velocity)
*	tst.l	d2
*	sne	rltv_uv_mode-work(a6)
*	bra	t_dat_ok

intercept_play:
	*   cmd=$54
	* < d2.l<>0 interception mode
	* < d2.l＝0 release interception & play
	* < d2.l＝1 release interception
	tst.l	d2
	bpl	@f
	move.l	#BRA*65536+((tmf_1-m_play00-2).and.$ffff),m_play00-work(a6)
	cache_flush
	rts
@@:
	move.l	m_play00_bak(pc),m_play00-work(a6)
	cache_flush
	tst.l	d2
	beq	m_play00
	bra	t_dat_ok
tmf_1:
	move.b	#1,timer_flg-work(a6)
	bra	t_dat_ok

midi_inp1:				*nmdb!!
	*   cmd=$55
	* < d2.l=0 single mode
	* < d2.l<>0 loop mode
	* > d0.b=recieved data
	* > d0.l=minus:error
	tst.b	emulate_mode-work(a6)
	bne	t_err_68
	set_a3a4_
	if	(type<>3.and.type<>4)

	tst.l	d2
	bne	loop_inp
	ori.w	#$700,sr	*割り込み禁止
	move.b	#3,-6(a4)
	*midiwait
	moveq.l	#-1,d0
	tst.b	(a4)
	bpl	@f
	*midiwait
	moveq.l	#0,d0
	move.b	(a3),d0
	*midiwait
@@:
	rts
loop_inp:
	moveq.l	#0,d0
@@:
	move.w	sr,-(sp)
	ori.w	#$700,sr	*割り込み禁止
	move.b	#3,-6(a4)
	*midiwait
	tst.b	(a4)
	bmi	@f
	move.w	(sp)+,sr
	bsr	v_wait
	subq.l	#1,d2
	bne	@b
	moveq.l	#-1,d0
	rts
@@:
	*midiwait
	move.b	(a3),d0
	*midiwait
	move.w	(sp)+,sr
	rts

	elseif	type=3

	tst.l	d2
	bne	loop_inp
	moveq.l	#-1,d0
	btst.b	#0,(a3)
	beq	@f
	moveq.l	#0,d0
	move.b	(a4),d0
@@:
	rts
loop_inp:
	moveq.l	#0,d0
@@:
	btst.b	#0,(a3)
	bne	@f
	bsr	v_wait
	subq.l	#1,d2
	bne	@b
	moveq.l	#-1,d0
	rts
@@:
	move.b	(a4),d0
	rts

	elseif	type=4

	tst.l	d2
	bne	loop_inp
	moveq.l	#-1,d0
	bsr	polyphon_midi_in
	bmi	@f
	moveq.l	#0,d0
	move.b	(a3),d0
@@:
	rts
loop_inp:
	moveq.l	#0,d0
@@:
	bsr	polyphon_midi_in
	bpl	@f
	bsr	v_wait
	subq.l	#1,d2
	bne	@b
	moveq.l	#-1,d0
	rts
@@:
	move.b	(a3),d0
	rts
	endif

midi_out1:			*nmdb!!
	*   cmd=$56
	* < d2.b=midi data
	set_a3a4
	moveq.l	#0,d0
	bra	m_out_d2

occupied_size:
	*   cmd=$57
	* < a1.l=storage address
	move.l	zmd_size(pc),(a1)+
	move.l	adpcm_buffer_next(pc),d0
	sub.l	adpcm_buffer_top(pc),d0
	bne	@f
	move.l	zpd_size(pc),d0
@@:
	move.l	d0,(a1)+
	move.l	adpcm_work_now(pc),d0
	sub.l	adpcm_work_top(pc),d0
	move.l	d0,(a1)+
	bra	t_dat_ok

call_int_play_ope:
	*   cmd=$58
	* < a5.l=仮想seq_wk_tbl
	movem.l	d0-d6/a0-a6,-(sp)
	bsr	int_play_ope
	movem.l	(sp)+,d0-d6/a0-a6
	rts

func_end:

copy_key:			*COPY key operation
	move.l	d0,-(sp)
	move.w	sr,d0
	andi.w	#$700,d0
	movem.l	(sp)+,d0	*わざとmovem
	bne	@f
	move.l	copy_org(pc),-(sp)
	rts
@@:
	rte

init_timer:			*タイマーBの初期化
	bra.s	init_timer_a	*ここは-aスイッチしないとNOPになる
	moveq.l	#$12,d1
	move.b	timer_value+1(pc),d2	*timer B value
	bsr	opmset
	moveq.l	#$14,d1
	moveq.l	#%0011_1010,d2	*int mask
	bsr	opmset
	st.b	timer_flg-work(a6)
	rts

init_timer_a:			*タイマーAの初期化
	move.w	timer_value(pc),d2	*timer A value
	moveq.l	#$11,d1
	bsr	opmset
	moveq.l	#$10,d1
	lsr.w	#2,d2
	bsr	opmset
	moveq.l	#$14,d1
	moveq.l	#%0011_0101,d2	*int mask
	bsr	opmset
	st.b	timer_flg-work(a6)
	rts

stop_timer2:			*タイマー割り込みの停止(割り込み処理期間以外を狙う)
stop_timer:			*タイマー割り込みの停止
	moveq.l	#$14,d1
	moveq.l	#0,d2
	bra	opmset		*int mask

init_play_wk:			*演奏ワークの初期化
	movem.l	d0/d2/a5,-(sp)
	movea.l	seq_wk_tbl(pc),a5
	moveq.l	#tr_max-1,d2
@@:
	clr.b	p_ch(a5)	*初期化はFMOPM1
	bsr	init_wks
	st.b	p_not_empty(a5)	*off
	lea	wk_size(a5),a5
	dbra	d2,@b
	movem.l	(sp)+,d0/d2/a5
	rts

init_inst1:			*音色番号１をセット
	moveq.l	#1,d0
init_inst:			*音色・パン・ボリュームを設定
	* < d0.b=neiro number(0-199)
	* < d0.w=-1の場合は音色以外の設定をする
	* < a5.l=seq wk n
	* - all
	subq.w	#1,d0
init_inst_:
	andi.w	#$ff,d0
	movem.l	d0-d2/a3-a4,-(sp)
	move.b	p_ch(a5),d1
	move.l	done_flg(pc),d2
	bset.l	d1,d2
	movem.l	d2,done_flg-work(a6)	*わざとmovem
	bne	exit_stn
	cmpi.b	#7,d1
	bhi	midi_stn1
	bne	@f
	opmset	#15,noise_mode(pc)	*fm ch=8ならnoiseパラメータも設定
@@:
	cmpi.b	#tone_max-1,d0
	bhi	@f
	bsr	pan_save_fmvset	*<d0=pgm number
	bra	exit_stn
@@:
	bsr	init_rr
	bra	exit_stn
midi_stn1:			*MIDIのケース
	sub.b	#9,d1		*make real midi ch(0-15)
	bmi	exit_stn	*ADPCMは無視
nmdb8:				*nmdb!!
	set_a3a4
	move.b	d1,d2		*d1=d2
	cmpi.w	#127,d0
	bhi	@f		*MIDIは0-127が有効
	ori.b	#$c0,d2		*program change
	bsr	m_out_d2
	bsr	m_out_d0		*音色切り換え
@@:
	ori.b	#$b0,d1		*ctrl change
	bsr	m_out_d1
*	bsr	m_out_d1		*reset all controllers
	m_out_	#$79
	bsr	m_out0
	m_out_	#$65		*RPN H
	bsr	m_out0
*	bsr	m_out_d1
	m_out_	#$64		*RPN L
	bsr	m_out0
*	bsr	m_out_d1		*PITCH BEND RANGEを設定する
	m_out_	#$06
	m_out_	p_@b_range(a5)
*	bsr	m_out_d1
	bsr	m_out7
	moveq.l	#127,d0
	sub.b	p_vol(a5),d0
	bsr	m_out_d0		*volume set

*	bsr	m_out_d1
	bsr	m_out10
	m_out_	p_pan2(a5)	*panpot set

*	bsr	m_out_d1
	bsr	m_out1
	bsr	m_out0			*modulation off

*	bsr	m_out_d1
	bsr	m_out64
	m_out_	p_dumper(a5)	*dumper

	ori.b	#$e0,d2
	bsr	m_out_d2
	move.w	p_detune_m(a5),d0
	add.w	#8192,d0
	move.w	d0,d1
	andi.b	#127,d0		*lower.b
	lsr.w	#7,d1		*higher.b
	bsr	m_out_d0		*lower @b
	bsr	m_out_d1		*higher @b

exit_stn:
	movem.l	(sp)+,d0-d2/a3-a4
	rts

all_key_off:			*全チャンネルをノートオフする
	* - all
	movem.l	d0-d2/d4/a3-a4,-(sp)
nmdb9:				*nmdb!!
	set_a3a4
	moveq.l	#25-1,d4	*loop cnt
	bra	ako_lp
ako00:
	moveq.l	#9-1,d4		*内蔵音源から
ako_lp:
	bsr	ako_sub
	dbra	d4,ako_lp
	movem.l	(sp)+,d0-d2/d4/a3-a4
	rts

top_ptr_set_all:		*top_ptr_setを全てのトラックについて行う
	movem.l	d0/a0/a5,-(sp)
	moveq.l	#tr_max-1,d0
	movea.l	trk_po_tbl(pc),a0
	movea.l	seq_wk_tbl(pc),a5
@@:
	move.l	(a0)+,p_data_pointer(a5)
	lea	wk_size(a5),a5
	dbra	d0,@b
	movem.l	(sp)+,d0/a0/a5
	rts

top_ptr_set:			*データ領域の先頭アドレスをワークへセットする
	* < d0.b=trk_number
	movem.l	d0/a0/a5,-(sp)
	andi.w	#$ff,d0
	movea.l	trk_po_tbl(pc),a0
	add.w	d0,d0
	add.w	d0,d0
	adda.w	d0,a0
	movea.l	seq_wk_tbl(pc),a5
	lsl.w	#wk_size2-2,d0
	adda.w	d0,a5
	move.l	(a0),p_data_pointer(a5)
	movem.l	(sp)+,d0/a0/a5
	rts

trk_top_set_:			*各トラックの先頭アドレスをワークへセット
	movem.l	d0-d1/a0,-(sp)
	movea.l	trk_po_tbl(pc),a0
	move.l	trk_top(pc),d0
	moveq.l	#tr_max-1,d1
@@:
	move.l	d0,(a0)+
	dbra	d1,@b
	movem.l	(sp)+,d0-d1/a0
	rts

tr_end_set:			*データ領域にエンドコードをセット
	movem.l	d0/a0-a1,-(sp)
	movea.l	trk_po_tbl(pc),a0
	moveq.l	#tr_max-1,d0
tes_lp:
	movea.l	(a0)+,a1
	st.b	(a1)		*end code
	dbra	d0,tes_lp
	movem.l	(sp)+,d0/a0-a1
	rts

init_play_trk_tbl:		*どのﾁｬﾝﾈﾙがどのﾄﾗｯｸへｱｻｲﾝされているかを初期化
	movem.l	d0-d1/a0,-(sp)
	lea	play_trk_tbl(pc),a0
	moveq.l	#tr_max/4-1,d0	*pl_max
	moveq.l	#-1,d1
@@:
	move.l	d1,(a0)+
	dbra	d0,@b
	movem.l	(sp)+,d0-d1/a0
	rts

neiro_init:			*音色バッファの初期化
	lea	neiro_(pc),a1	*src
	movea.l	neiro(pc),a2	*dest
	moveq.l	#5,d1		*mode
	moveq.l	#neiro_size,d2
	bsr	trans_dma

	movea.l	a2,a1		*src
	lea	neiro_size(a2),a2
	move.l	#neiro_size*(tone_max-1),d2
	bsr	trans_dma
*	bsr	fm_init
	rts

fclose:
	* < d5.w=file handle
	* - all
	move.l	d0,-(sp)
	bsr	do_fclose
	move.l	(sp)+,d0
	rts

do_fclose:
	move.w	d5,-(sp)	*close
	DOS	_CLOSE
	addq.w	#2,sp
	rts

*----------------------------------------
	*t_系のコマンドのエラーは
	*d0.l=エラーコード
	*a0.l=0
	*で表現される
*----------------------------------------
t_err_2:	t_err	#02	*illegal track number
t_err_5:	t_err	#05	*track buffer is too small
t_err_6:	t_err	#06	*illegal ch number
t_err_28:	t_err	#28	*track buffer is full
t_err_38:	t_err	#38	*illegal note number
t_err_39:	t_err	#39	*illegal tone number
t_err_60:	bsr	_clr_adpb
		t_err	#60	*pcm buffer is not enough
t_err_61:	t_err	#61	*can't use pcm
t_err_62:	t_err	#62	*read error
t_err_64:	t_err	#64	*work area is too small
t_err_65:	t_err	#65	*正体不明のエラー
t_err_66:	t_err	#66	*バージョン不一致
t_err_67:	t_err	#67	*ADPCM定義コマンドエラー
t_err_68:	t_err	#68	*MIDIボードがありません
t_err_69:	t_err	#69	*楽器に対するパラメータが異常です
t_err_70:	t_err	#70	*パラメータが異常です
t_err_73:	t_err	#73	*録音されてません
t_err_74:	t_err	#74	*波形番号が異常
t_err_76:	t_err	#76	*波形メモリ設定コマンドエラー
t_dat_ok:	moveq.l	#0,d0	*正常終了
		cache_flush
		rts
disp_t_err?:
	bsr	set_err_code		*set error code
	suba.l	a0,a0			*error mark
	bsr	fclose			*openしているファイルがあればそれを閉じる

*	cmpi.b	#64,d0
*	beq	play_beep	*警告音を鳴らす
*	cmpi.b	#60,d0
*	beq	play_beep	*警告音を鳴らす
*	rts
	bra	play_beep

set_err_code:				*エラーコードのストック
	movem.l	d1/a0,-(sp)
	lea	err_code+7(pc),a0
	moveq.l	#8-1-1,d1
@@:
	move.b	-(a0),1(a0)
	dbra	d1,@b
	move.b	d0,(a0)
*	move.b	d0,err_exit_f-work(a6)
	movem.l	(sp)+,d1/a0
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
*	move.b	#$20,(a0)+
	bra	nml_lp_ope
nml_ktset:
	st	d4
	add.b	#'0',d2
	move.b	d2,(a0)+
nml_lp_ope:
	dbra	d1,ex_loop0
	lea	suji(pc),a1
	cmpa.l	a1,a0
	bne	set_suji_end
	move.b	#'0',(a0)+
set_suji_end:
	clr.b	(a0)		*end flg
	movem.l	(sp)+,d1-d4/a0-a1
	rts

calc_wk:
	* < d0=trk number
	* > a5=work addr.
	move.l	d0,-(sp)
	lsl.w	#wk_size2,d0
	movea.l	seq_wk_tbl(pc),a5
	adda.w	d0,a5
	move.l	(sp)+,d0
	rts

*_calc_wk:			*効果音用
*	* < d0=trk number
*	* > a4=work addr.
*	move.l	d0,-(sp)
*	lsl.w	#wk_size2,d0
*	movea.l	seq_wk_tbl2(pc),a4
*	adda.w	d0,a4
*	move.l	(sp)+,d0
*	rts

*get_real_ch:			*便宜上のチャンネルから本来のチャンネルへ
*	* < d0.l=ch
*	* > d0.l=real ch
*	* - all
*	move.l	a0,-(sp)
*	lea	real_ch_tbl(pc),a0
*	move.b	(a0,d0.w),d0
*	move.l	(sp)+,a0
*	rts

calc_tm_b:			*テンポ値(20～300)をタイマーBの設定値へ変換しワークへセット
	* - all
	bra.s	calc_tm_a	*ここは-aスイッチしないとNOPになる
	movem.l	d0-d1,-(sp)
	bsr	do_calc_tmb
	bmi	@f
	move.w	d1,timer_value-work(a6)
@@:
	movem.l	(sp)+,d0-d1
	rts

calc_tm_a:			*テンポ値(77～300)をタイマーAの設定値へ変換しワークへセット
	* - all
	movem.l	d0-d1,-(sp)
	bsr	do_calc_tma
	bmi	@f
	move.w	d1,timer_value-work(a6)
@@:
	movem.l	(sp)+,d0-d1
	rts

do_calc_tmm:
calc_tm_m_:
	* < m_tmp_buf.w=tempo
	* > d1.w=timer m value
	* a1,a6使用禁止
	move.l	d0,-(sp)
	move.l	#30*1000*1000,d1	*１分=60*1000000μｓ
	moveq.l	#0,d0
	move.b	mclk(pc),d0
	mulu	m_tmp_buf(pc),d0
	divu	d0,d1
	bvs	tmp_err
	andi.w	#$3fff,d1
	movem.l	(sp)+,d0	*わざとmovem
	rts

calc_tm_b_:			*テンポ値(20～300)をタイマーBの設定値へ変換する
	* < m_tmp_buf.w=tempo
	* > d1.b=timer b value
	bra.s	calc_tm_a_	*ここは-aスイッチしないとNOPになる

do_calc_tmb:
	* a1,a6使用禁止
	move.l	d0,-(sp)
	move.w	m_tmp_buf(pc),d0
	lsl.w	#4,d0
	move.l	tmp_base(pc),d1	*d1=16*4000*60000/(1024*48)
	divu	d0,d1
	bvs	tmp_err
	swap	d1
	tst.w	d1		*余りがあるか
	beq	@f
	add.l	#$0001_0000,d1	*余りがあるなら切り上げ
@@:
	swap	d1		*d1.w=answer
	move.w	#256,d0
	sub.w	d1,d0
	move.w	d0,d1
	movem.l	(sp)+,d0	*わざとmovem
	rts
tmp_err:
	move.l	(sp)+,d0
	moveq.l	#-1,d1
	rts

do_calc_tma:
calc_tm_a_:			*テンポ値(77～300)をタイマーAの設定値へ変換する
	* < m_tmp_buf.w=tempo
	* > d1.w=timer a value
	* a1,a6使用禁止
	move.l	d0,-(sp)
	move.l	tmp_base(pc),d1	*d1=16*4000*60000/(1024*48)
	divu	m_tmp_buf(pc),d1
	bvs	tmp_err
	move.w	#1024,d0
	sub.w	d1,d0
	bpl	@f
	moveq.l	#0,d0		*tempo77以下は77へ
@@:
	move.w	d0,d1
	movem.l	(sp)+,d0	*わざとmovem
	rts

*fm_init:
*	movem.l	d1-d2/a1-a2,-(sp)
*	lea	preset_tones(pc),a1
*	movea.l	neiro(pc),a2
*	moveq.l	#5,d1
*	move.l	#end_pt-preset_tones,d2
*	bsr	trans_dma	*プリセット音色のコピー
*	movem.l	(sp)+,d1-d2/a1-a2
*	rts

play_beep:				*BEEP音を鳴らす
	move.w	sr,-(sp)
	ori.w	#$700,sr
	movem.l	d0-d7/a0-a6,-(sp)
	move.w	#$07,-(sp)
	clr.w	-(sp)
	DOS	_CONCTRL
	addq.w	#4,sp
	movem.l	(sp)+,d0-d7/a0-a6
	move.w	(sp)+,sr
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

support_name:
pname:
dev_init:				*デバイスドライバとしての初期化
	movem.l	d1-d7/a0-a6,-(sp)
	lea	work(pc),a6
	move.l	sp,_sp_buf-work(a6)
	movea.l	18(a5),a4		*parameter pointer
	lea	-260(sp),sp
	move.l	sp,dmy_seq_wk-work(a6)
di_lp01:
	tst.b	(a4)+
	bne	di_lp01			*まずプログラム名をスキップ
	clr.b	cmd_or_dev-work(a6)	*'device='mode
	bsr	chk_board		*CHECK MIDI BOARD
					*スイッチ処理
	clr.b	zm_opt-work(a6)
	bsr	chk_dev

	bsr	set_patch
*	bsr	non_cnv_patch
	cache_flush
	bsr	get_work_area
	bne	resigned
	bsr	set_vect		*拡張IOCSとしての登録
	bne	already_kept		*ベクタの書き換えに失敗した
	move.l	dev_end_adr(pc),14(a5)	*本ドライバの終了アドレス
	bsr	prt_bf_mes
	bsr	init_all
	cache_flush

	tst.b	adpcm_read_flg-work(a6)
	beq	@f
	bsr	do_block_read
@@:
	tst.b	su_flg-work(a6)		*スタートアップファイルの読み込み
	beq	@f
	bsr	read_start_up
@@:
	lea	260(sp),sp
	movem.l	(sp)+,d1-d7/a0-a6
	bra	ok_com

exec:				*コマンドラインから実行した時
	lea	work(pc),a6
	clr.l	-(sp)
	DOS	_SUPER			*スーパーバイザーへ
	addq.w	#4,sp
	move.l	d0,ssp-work(a6)
	lea	-260(sp),sp
	move.l	sp,dmy_seq_wk-work(a6)

	move.l	a0,a0work-work(a6)
	move.l	a1,a1work-work(a6)
	lea	$c4(a0),a0	*a0+$c4はexecされたfile名
	lea	pname(pc),a1
lop1:
	move.b	(a0)+,(a1)+	*a0+$c4はexecされたfile名
	bne	lop1		*file名をpnameへ転送

	movea.l	a2,a4
	addq.w	#1,a4
	move.l	a4,-(sp)
	st.b	cmd_or_dev-work(a6)	*'cmd' mode
	bsr	chk_board		*CHECK MIDI BOARD
					*スイッチ処理
	tst.b	-1(a4)
	bne	@f
	pea	zm_opt(pc)
	jsr	search_env-work(a6)
	addq.w	#4,sp
	tst.l	d0
	bmi	@f
	move.l	a4,-(sp)
	move.l	d0,a4
	bsr	chk_dev
	move.l	(sp)+,a4
@@:
	clr.b	zm_opt-work(a6)
	bsr	chk_dev

	bsr	set_patch
*	bsr	non_cnv_patch
	cache_flush
	bsr	get_work_area
	movem.l	(sp)+,a4		*わざと
	bne	resigned
	bsr	set_vect
	bne	exit_oo			*(既に登録住み)
	bsr	prt_bf_mes
	bsr	find_dev_name
	bmi	unknown_err
	bsr	init_all
	cache_flush

	tst.b	adpcm_read_flg-work(a6)
	beq	@f
	bsr	do_block_read
@@:
	tst.b	su_flg-work(a6)		*スタートアップファイルの読み込み
	beq	@f
	bsr	read_start_up
@@:
	move.l	dev_end_adr(pc),d1
	sub.l	#device_driver,d1

	move.l	ssp(pc),a1
	IOCS	_B_SUPER

	clr.w	-(sp)
	move.l	d1,-(sp)
	DOS	_KEEPPR			*常駐終了

do_block_read:			*立ち上げ時にデータをリード
	lea	h_work(pc),a1
	ZM	#$39
	tst.l	d0
	bne	exit_bdr
dbrp:
	bsr	mes_head
	lea	default_adp(pc),a0	*読み込み正常完了
	bra	prta0
exit_bdr:
	bsr	mes_head
	lea	cannot_read(pc),a0
	bra	prta0

mes_head:
	movem.l	a0-a1,-(sp)
	lea	h_work(pc),a0
	lea	read_mes1(pc),a1
	bra	@f
mes_head2:
	movem.l	a0-a1,-(sp)
	lea	stup_fnsv(pc),a0
	lea	read_mes2(pc),a1
@@:
	bsr	prta1
	bsr	prta0
	movem.l	(sp)+,a0-a1
	rts

read_start_up:			*立ち上げ時にデータをリード(その２)
	lea.l	stup_fnsv(pc),a2
	bsr	fopen		*(ret:d5=file handle)
	bsr	get_fsize	*>d3.l=size
	move.l	d3,d4
	addq.l	#1,d3
	bclr.l	#0,d3		*.even

	move.l	adpcm_work_end(pc),d1	*ワークサイズを一時的に縮小する
	sub.l	d3,d1			*read address
	move.l	adpcm_work_size(pc),-(sp)
	move.l	adpcm_work_end(pc),-(sp)
	sub.l	d3,adpcm_work_size-work(a6)
	move.l	d1,adpcm_work_end-work(a6)
	move.l	d1,a4			*addr

	move.l	d3,-(sp)	*push size
	move.l	d1,-(sp)	*push addr
	move.w	d5,-(sp)	*file handle
	DOS	_READ
	lea	10(sp),sp

	bsr	do_fclose

	*.mddかどうかチェックする
@@:
	move.b	(a2)+,d0
	beq	not_mdd
	cmpi.b	#'.',d0
	bne	@b
	move.b	(a2)+,d0
	andi.b	#$df,d0
	cmpi.b	#'M',d0
	bne	not_mdd
	move.b	(a2)+,d0
	andi.b	#$df,d0
	cmpi.b	#'D',d0
	bne	not_mdd
	move.b	(a2)+,d0
	andi.b	#$df,d0
	cmpi.b	#'D',d0
	bne	not_mdd

nmdb10:				*nmdb!!
	pea	su_ok(pc)
	movem.l	d0-d1/d4/a0/a4/a6,-(sp)
	bra	do2_lp01
not_mdd:
	pea	su_ok(pc)
	movem.l	d1-d7/a0-a6,-(sp)
	lea	-512(sp),sp
	cmpi.b	#$10,(a4)	*ヘッダチェック
	beq	exec_cmp	*コンパイルデータの実行
	bra	dev_out_end
su_ok:
	move.l	(sp)+,adpcm_work_end-work(a6)
	move.l	(sp)+,adpcm_work_size-work(a6)
surp:
	bsr	mes_head2
	move.w	#2,-(sp)
	pea	default_adp(pc)	*読み込み正常完了
	DOS	_FPUTS
	addq.w	#6,sp
	rts

su_not_ok:
	move.l	(sp)+,adpcm_work_end-work(a6)
	move.l	(sp)+,adpcm_work_size-work(a6)

	bsr	mes_head2
	move.w	#2,-(sp)
	pea	cannot_read(pc)
	DOS	_FPUTS
	addq.w	#6,sp
	rts

init_all:			*ドライバの初期化
	tst.b	no_init_mode-work(a6)
	beq	@f
	move.w	#RTS,d0
	move.w	d0,init_inst-work(a6)
	move.w	d0,init_midi-work(a6)
@@:
	bsr	init_midibd
	moveq.l	#0,d0
*	move.b	d0,compile_mode-work(a6)	*compile mode off
	move.b	d0,last_fn-work(a6)
	bsr	wave_tbl_init
	bsr	adpcm_tbl_init
	bsr	neiro_init
	bsr	m_init
	bsr	init_se_wk
	bsr	init_voices
	move.w	#$0403,d1		*frq/pan
	moveq.l	#num_of_80,d2			*length
	lea	dummy_data(pc),a1
	IOCS	_ADPCMOUT		*ダミー再生
	rts

init_se_wk:
*	movem.l	d0/d2/a5,-(sp)
	movea.l	seq_wk_tbl2(pc),a5
	moveq.l	#32-1,d2	*pl_max
@@:
	bsr	init_wks
	lea	wk_size(a5),a5
	dbra	d2,@b
*	movem.l	(sp)+,d0/d2/a5
	rts

init_voices:			*音色等の初期設定
	clr.l	done_flg-work(a6)
	moveq.l	#ch_max-1,d3
	move.l	dmy_seq_wk(pc),a5	*一応
fmvi_lp:
	move.b	d3,p_ch(a5)	*d3=ch
	bsr	init_wks
	bsr	init_inst1
inmd1:	nop			*-Jで"bsr.w f7_wait"が書き込まれる
	nop
	dbra	d3,fmvi_lp
	rts

prta0:
	move.w	#2,-(sp)
	pea	(a0)
@@:
	DOS	_FPUTS
	addq.w	#6,sp
	rts
prta1:
	move.w	#2,-(sp)
	pea	(a1)
	bra	@b

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

	.include	work.s

	.include	zmsc2.s

	.data
work_start:			*使い捨てのワーク群(後にグローバルワークとして使用される)
NUL:		dc.b	'NUL     '
neiro_:					*音色バッファの初期データ
	.dc.b	$3a,$0f,$02,$00,$dc,$00,$00,$00
	.dc.b	$00,$03,$00,$1c,$04,$00,$05,$01
	.dc.b	$25,$02,$01,$07,$00,$00,$16,$09
	.dc.b	$01,$02,$01,$2f,$02,$0c,$00,$00
	.dc.b	$00,$1d,$04,$03,$06,$01,$25,$01
	.dc.b	$03,$03,$00,$00,$0f,$07,$00,$05
	.dc.b	$0a,$00,$02,$01,$00,$00,$01
neiro_end_:
keep_mes:
	dc.b	$1b,'[37m'
	if	(mpu=30.and.type<>3)
	dc.b	$F3,'U',$F3,'N',$F3,'I',$F3,'V',$F3,'E',$F3,'R',$F3,'S',$F3,'A',$F3,'L '
	elseif	(type<>3.and.type<>4)
	dc.b	$F3,'1',$F3,'6',$F3,'b',$F3,'i',$F3,'t '
	endif
	if	type=3
	dc.b	$F3,'R',$F3,'S',$F3,'-',$F3,'M',$F3,'I',$F3,'D',$F3,'I '
	endif
	if	type=4
	dc.b	$F3,'P',$F3,'O',$F3,'L',$F3,'Y',$F3,'P',$F3,'H',$F3,'O',$F3,'N '
	endif
	dc.b	$F3,'V',$F3,'E',$F3,'R',$F3,'S',$F3,'I',$F3,'O',$F3,'N'
	version
	dc.b	$1b,'[m (C) 1992,1993,1994 '
	dc.b	$1b,'[36mZENJI SOFT',$1b,'[m',13,10,0
mzl_mes:dc.b	9,9,9,9
	dc.b	$F3,'2',$F3,'6',$F3,'/',$F3,'D',$F3,'e',$F3,'c',$F3,'/',$F3,'1',$F3,'9',$F3,'9',$F3,'9'
	dc.b	$F3,' ',$F3,'m',$F3,'o',$F3,'d',$F3,'e',$F3,'f',$F3,'i',$F3,'e',$F3,'d',$F3
	dc.b	' ',$F3,'b',$F3,'y',$F3,' ',$F3,'M',$F3,'Z',$F3,'L',$F3,'.',13,10,0
yes_midi:	dc.b	"MIDI/"
no_midi:	dc.b	"FM･OPM/ADPCM are under the control of ZMUSIC.",13,10,0
	if	type=3
already_mes:	dc.b	$1b,'[47mThe interrupt-vector has already been occupied by some other applications.',$1b,'[m',13,10,0
	else
already_mes:	dc.b	$1b,'[47mMUSIC BIOS has already been included in your system.',$1b,'[m',13,10,0
	endif
already_mes2:	dc.b	$1b,"[47mDidn't you include a special ADPCM driver?",$1b,'[m',13,10,0
secure_mes:	dc.b	'Secure ',0
pcmbf_mes:	dc.b	'for ADPCM data buffer.',13,10,0
wkbf_mes:	dc.b	'for work area.',13,10,0
trkbf_mes:	dc.b	'for track buffer.',13,10,0
kakuho_mes:	dc.b	'kByte(s)',$1b,'[m ',0
atrb_0010:	dc.b	$1b,'[32m',0
tm_a_mes:	dc.b	'Interrupt source:TIMER A',13,10,0
sq_m_mes:	dc.b	'The external MIDI sequencer synchronizes with ZMSC.X',13,10,0
	if	type=4
pcm8_mode_mes_:	dc.b	'MIDI and ADPCM are managed by PCM8SB.X (C)H.ETOH',13,10,0
	endif
pcm8_mode_mes:	dc.b	'ADPCM is managed by PCM8.X (C)H.ETOH',13,10,0
zmt:		dc.b	$1b,'[36mΖ',$1b,'[35mm',$1b,'[37m','∫',$1b,'[35mc ',0
kaijo:		dc.b	$1b,'[mhas been released from your system.',13,10,0
not_kep_mes:	dc.b	$1b,'[mis not kept in your system.',13,10,0
rls_er_mes:	dc.b	$1b,'[mis unable to release.',13,10,0
ver_er_mes:	dc.b	$1b,'[47mIllegal version number. Unable to release.',$1b,'[m',13,10,0
illegal_p:	dc.b	$1b,'[47mERROR',$1b,'[m',13,10,0
unknown_mes:	dc.b	$1b,'[47mUnknown error!'
		dc.b	$1b,'[m',13,10,0
err_header:	dc.b	$1b,'[47m',0
help_mes:	dc.b	$1b,'[37m< USAGE >'
		dc.b	$1b,'[m ZMSC.X [Optional Switches]',13,10
		dc.b	$1b,'[37m< OPTIONAL SWITCHES >',13,10,$1b,'[m'
		dc.b	'-? or H      Display the list of optional switches.',13,10
		dc.b	'-A	     Make ZMSC.X work on TIMER-A.',13,10
		dc.b	'-B<filename> Include ADPCM block data.',13,10
	if	(type<>3.and.type<>4)
		dc.b	'-E	     Synchronize an external MIDI sequencer.',13,10
	endif
		dc.b	'-P<n>	     Secure n kBytes for ADPCM data buffer.(default=256kB)',13,10
		dc.b	'-R	     Release ZMSC.X from the system.',13,10
		dc.b	'-S<filename> Include start-up file.',13,10
		dc.b	'-T<n>	     Secure n kBytes for the MML track buffer.(default=32kB)',13,10
		dc.b	'-W<n>	     Secure n kBytes for a work area.(default=0kB)',13,10
	if	type=3
		dc.b	'-Y<n>	     Set RS-MIDI bitrate generator value at n.(default=3)',13,10
		dc.b	"	     (1 <= n <= 15 / '-Y0' calculates the value automatically.)",13,10
	endif
		dc.b	0
zm_opt:		dc.b	'zm_opt',0
	.text
	.even
already_kept:
	tst.l	d0
	bpl	@f
	lea	already_mes(pc),a0	*COULDN'T SET ZMUSIC VECT
	bra	ak_prt
@@:
	lea	already_mes2(pc),a0	*COULDN'T SET INT ADPCM VECT
ak_prt:
	bsr	prta0
	movem.l	(sp)+,d1-d7/a0-a6
	bra	not_com

chk_dev:				*スイッチ処理
	* < cmd_or_dev  0:device / $ff:command
chk_dev_lp:
	cache_flush
	move.b	(a4)+,d0
	beq	no_more?
	cmpi.b	#' ',d0
	beq	chk_dev_lp
	cmpi.b	#'-',d0		*その他スイッチ処理へ
	beq	other_sw
	cmpi.b	#'/',d0
	beq	other_sw
	cmpi.b	#'#',d0
	beq	get_bfsz	＃ならば次にはトラックバッファサイズ
	bra	chk_dev_lp

no_more?:
	tst.b	cmd_or_dev-work(a6)
	bne	tbf32kb		*コマンドラインからならもうスイッチはあり得ない
	tst.b	(a4)
	beq	tbf32kb		*ゼロが２個続くと言うことはもうスイッチは無し
	bra	chk_dev_lp

other_sw:			*その他のスイッチ
	move.b	(a4)+,d0
	beq	no_more?
	cmpi.b	#'?',d0		*diplay help message
	beq	prt_help
*	cmpi.b	#'C',d0		*最適化の有無
*	seq	no_optmz-work(a6)
	andi.b	#$df,d0
	cmpi.b	#'A',d0		*timer a mode
	beq	set_timer_a_mode
	cmpi.b	#'B',d0		*ADPCM DATA READ
	beq	block_read
*	cmpi.b	#'C',d0		*COMPILE MODE
*	beq	compile
	if	(type<>3.and.type<>4)
	cmpi.b	#'E',d0		*外部同期モード
	beq	synchro_on
	endif
	cmpi.b	#'G',d0		*タイトル表示なしモード
	beq	non_title
	cmpi.b	#'H',d0		*diplay help message
	beq	prt_help
*	cmpi.b	#'I',d0		*ADPCMはPCM8で鳴らす
*	beq	pcm8_mode
	cmpi.b	#'J',d0		*初期化を低速に行なう
	beq	slow_init
	cmpi.b	#'M',d0		*多重割り込み対応モード
	beq	mfp_mode_on
	cmpi.b	#'N',d0		*初期化無しモード
	beq	no_init
	cmpi.b	#'O',d0		*pcm8 poly mode
	beq	poly_mode
	cmpi.b	#'P',d0		*pcm buffer
	beq	get_pcmbf
*	cmpi.b	#'Q',d0		*compile終了時にトータルカウントを表示する
*	beq	calc_total_mode
	cmpi.b	#'R',d0		*解除
	beq	release
	cmpi.b	#'S',d0		*start up
	beq	stup
	cmpi.b	#'T',d0		*track buffer
	beq	get_bfsz
	cmpi.b	#'U',d0		*special mode
	beq	chk_dev_lp
	cmpi.b	#'W',d0		*pcm work buffer
	beq	get_wkbf
	cmpi.b	#'X',d0		*EOX wait
	beq	get_eoxw
	if	type=3
	cmpi.b	#'Y',d0		*RS-MIDI 時定数
	beq	get_rsmidi
	endif
	bra	prt_help

poly_mode:
	moveq.l	#7,d2
	move.w	d2,poly_ch-work(a6)
	move.w	#BRA+((poly_play-poly_-2).and.$ff),poly_-work(a6)
	move.w	#RTS,PCM8KOFF-work(a6)
*	move.l	#adpcm_n_max_default,adpcm_n_max-work(a6)
	moveq.l	#$7f,d4
	jsr	chk_num-work(a6)
	bmi	chk_dev_lp
	jsr	asc_to_n-work(a6)
	subq.w	#1,a4
	subq.l	#1,d1
	bmi	chk_dev_lp
	cmp.l	d2,d1
	bhi	chk_dev_lp
	move.w	d1,poly_ch-work(a6)
	bra	chk_dev_lp

slow_init:
	move.l	#BSR*65536+((f7_wait-inmd0-2).and.$ffff),inmd0-work(a6)
	move.l	#BSR*65536+((f7_wait-inmd1-2).and.$ffff),inmd1-work(a6)
	bra	chk_dev_lp

no_init:
	st.b	no_init_mode-work(a6)
	bra	chk_dev_lp

non_title:
	move.l	#$7000_4e75,d0
	move.l	d0,prt_title-work(a6)
	move.w	d0,prt_bf_mes-work(a6)
	move.w	d0,dbrp-work(a6)
	move.w	d0,surp-work(a6)
	move.w	d0,release_mes-work(a6)
	bra	chk_dev_lp

mfp_mode_on:
	st.b	mfp_mode-work(a6)
	bra	chk_dev_lp

set_timer_a_mode:
	st.b	timer_a_mode-work(a6)
	bra	chk_dev_lp

synchro_on:				*nmdb!!
	st.b	synchro_mode-work(a6)
	bra	chk_dev_lp

*calc_total_mode:
*	st.b	clcttl_mode-work(a6)
*	bra	compile

block_read:
brlp00:
	move.b	(a4)+,d0	*SPCをスキップ
	bne	@f
	tst.b	cmd_or_dev-work(a6)	*command or dev?
	bne	no_more?	*手抜き(case:command)
	tst.b	(a4)
	beq	no_more?	*手抜き(case:dev,came to an end)
	bra	brlp00		*まだ何かある
@@:
	cmpi.b	#' ',d0
	bls	brlp00
	subq.w	#1,a4
	moveq.l	#$7f,d4		*dummy length
	lea	h_work(pc),a0
	jsr	copy_fn-work(a6)
	lea	h_work(pc),a0
	lea	ZPD(pc),a1
	bsr	kakuchoshi

	lea	adpcm_buffer_size(pc),a1
	lea	h_work(pc),a2
	tst.b	psw_est?-work(a6)	*-Pスイッチが使用された？
	bne	@f
	clr.l	(a1)
@@:
	bsr	open_and_size
	bmi	non_jochu
	st.b	adpcm_read_flg-work(a6)	*flag set
*	move.l	#adpcm_n_max_default,adpcm_n_max-work(a6)
	bra	chk_dev_lp

non_jochu:
	bsr	mes_head
	lea	cannot_read(pc),a0	*エラーメッセージを表示して終了
	tst.b	cmd_or_dev-work(a6)	*device=の場合は組み込まない
	beq	resigned1
	bra	__end
resigned:
	lea	illegal_p(pc),a0
	tst.b	cmd_or_dev-work(a6)	*device=の場合は組み込まない
	bne	__end
resigned1:
	bsr	prta0
	move.l	_sp_buf(pc),sp
	movem.l	(sp)+,d1-d7/a0-a6
	move.w	#$700c,d0
	rts

open_and_size:
	* < a1.l=size save addr
	* < a2.l=file name pointer
	* X d0,d3,d5
	bsr	fopen		*<a2.l=file name,(ret:d5=file handle)
	tst.l	d5		*d5=file_handle
	bmi	oas_err
	bsr	get_fsize2	*>d3.l=file size
	bmi	oas_err
	cmp.l	(a1),d3
	bls	@f		*バッファがデータサイズより大きいか小さいか
	move.l	d3,(a1)
	beq	oas_err
@@:
	bsr	do_fclose
	moveq.l	#0,d0
	rts
oas_err:
	moveq.l	#-1,d0
	rts

stup:
sulp00:
	move.b	(a4)+,d0	*SPCをスキップ
	bne	@f
	tst.b	cmd_or_dev-work(a6)	*command or dev?
	bne	no_more?	*手抜き(case:command)
	tst.b	(a4)
	beq	no_more?	*手抜き(case:dev,came to an end)
	bra	sulp00		*まだ何かある
@@:
	cmpi.b	#' ',d0
	bls	sulp00
	subq.w	#1,a4
	lea.l	stup_fnsv(pc),a0
sulp01:				*ファイルネームのゲット
	move.b	(a4)+,d0
	cmpi.b	#' ',d0
	bls	@f
	move.b	d0,(a0)+
	bra	sulp01
@@:
	subq.w	#1,a4
	clr.b	(a0)		*end code

	lea	adpcm_work_size(pc),a1
	lea.l	stup_fnsv(pc),a2
	bsr	open_and_size
	bmi	_non_jochu
	st.b	su_flg-work(a6)		*flag set
	bra	chk_dev_lp

_non_jochu:
	tst.b	cmd_or_dev-work(a6)	*commandの場合はやむをえない
	beq	chk_dev_lp
	bsr	mes_head2
	lea	cannot_read(pc),a0	*エラーメッセージを表示して終了
	bra	__end

get_pcmbf:
*	move.l	#adpcm_n_max_default,adpcm_n_max-work(a6)
	moveq.l	#$7f,d4
	jsr	skip_spc-work(a6)
	cmpi.b	#',',(a4)
	beq	get_bnkn
	jsr	asc_to_n-work(a6)
	subq.w	#1,a4
	moveq.l	#10,d0
	lsl.l	d0,d1		*byte単位にする
	tst.b	adpcm_read_flg-work(a6)	*'-B'?
	beq	@f
	cmp.l	adpcm_buffer_size(pc),d1
	bls	get_bnkn
@@:
	move.l	d1,adpcm_buffer_size-work(a6)
	st.b	psw_est?-work(a6)
get_bnkn:
	jsr	skip_spc-work(a6)
	cmpi.b	#',',(a4)
	bne	chk_dev
	addq.w	#1,a4
	jsr	chk_num-work(a6)
	bmi	chk_dev_lp
	jsr	asc_to_n-work(a6)
	subq.w	#1,a4
	cmpi.l	#bank_max,d1		*最大バンクをこえていないか？
	bls	@f
	moveq.l	#bank_max,d1
@@:
	lsl.l	#7,d1
	beq	@f
*	cmp.l	adpcm_n_max(pc),d1
*	bls	chk_dev_lp
set_bnkn:
	move.l	d1,adpcm_n_max-work(a6)
	bra	chk_dev_lp
@@:
	tst.l	adpcm_buffer_size-work(a6)
	beq	set_bnkn
	bra	chk_dev_lp

chk_num:			*数字かどうかチェック
	* > eq=number
	* > mi=not num
	move.l	d0,-(sp)
	bsr	skip_spc
	tst.l	d4
	beq	not_num
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

get_wkbf:
	moveq.l	#$7f,d4
	jsr	asc_to_n-work(a6)
	subq.w	#1,a4
	moveq.l	#10,d0
	lsl.l	d0,d1		*byte単位にする
	bne	@f
	moveq.l	#126,d1		*最低取っておく
@@:
	move.l	d1,adpcm_work_size-work(a6)
	bra	chk_dev_lp

get_eoxw:
	moveq.l	#$7f,d4
	jsr	asc_to_n-work(a6)
	subq.w	#1,a4
	move.w	d1,eox_w-work(a6)
	bra	chk_dev_lp

	if	type=3
get_rsmidi:
	moveq	#$7f,d4
	jsr	asc_to_n-work(a6)
	subq.w	#1,a4
	tst.b	d1
	bne	@f

	jsr	set_rsmidi_rate-work(a6)
	move.b	d1,rs_data_clock-work(a6)
	bra	chk_dev_lp
@@:
	move.l	#13<<24+10<<16+0,rsmidi_mes2-work(a6)	;CR,LF,0
	move.b	d1,rs_data_clock-work(a6)
	add.b	#'0',d1
	cmpi.b	#'9'+1,d1
	bcs	@f
	add.b	#'A'-'0',d1
@@:
	move.b	d1,rsmidi_mes1-work(a6)
	bra	chk_dev_lp
	endif

get_bfsz:
	moveq.l	#$7f,d4		*dummy(最高何桁の数字を取ってくるかみたいなもの)
	jsr	asc_to_n-work(a6)
	subq.w	#1,a4
	bsr	sv_dea
	bra	chk_dev_lp
tbf32kb:			*defaultは32kBバッファを取る
	tst.b	zm_opt-work(a6)
	bne	exit_sv_dea	*環境変数zm_opt処理のケース
	move.l	#(dflt_trkbf.shr.10),d1
sv_dea:				*バッファサイズ等を求めワークへしまう
	tst.l	trk_buf_size-work(a6)
	bne	exit_sv_dea
	moveq.l	#10,d0
	lsl.l	d0,d1		*byte単位にする
	bne	@f
	moveq.l	#126,d1		*最低取っておく
@@:
	move.l	d1,trk_buf_size-work(a6)
exit_sv_dea:
	rts

set_vect:				*本デバイスドライバを拡張IOCSとして登録
	* > d0.l≠0ならエラー
*	tst.b	midi_board-work(a6)
*	bne	mdintst
*@@:					*OPM
	lea	$fe0000,a2
	cmp.l	$10c.w,a2
	bcs	set_opmv
the_end:
	moveq.l	#-1,d0
	rts				*すでに何かが占有中
set_opmv:
	lea	int_entry(pc),a1
	IOCS	_OPMINTST

	if	(type<>3.and.type<>4)	*MIDIデータ受信割り込みの設定
	move.l	$228.w,d0
	cmp.l	a2,d0
	bcs	already_bye		*すでに何かが占有中
	lea	rec_int(pc),a1
	move.l	a1,$228.w
	move.l	d0,rec_vect-work(a6)

	elseif	type=3			*RS-MIDI
	move.l	$170.w,d0
	cmp.l	a2,d0
	bcs	already_bye		*すでに何かが占有中
	lea	rec_int(pc),a1
	move.l	a1,$170.w
	move.l	d0,rec_vect-work(a6)

	move.l	$174.w,d0
	cmp.l	a2,d0
	bcs	already_bye		*すでに何かが占有中
	lea	rec_int(pc),a1
	move.l	a1,$174.w
	move.l	d0,rec_vect_-work(a6)
	endif

*	move.w	#$1f0,d1
*	lea	Z_MUSIC(pc),a1
*	IOCS	_B_INTVCS		*set new IOCS call
*	move.l	d0,vect_1f0-Z_MUSIC(a1)
*	cmp.l	a2,d0
*	bcs	already_bye
					*ADPCMの停止処理の書き換え
	lea	int_adpcm_stop(pc),a1
	tst.b	pcm8_flg-int_adpcm_stop(a1)
	bne	@f
	moveq.l	#$6a,d1
	IOCS	_B_INTVCS
	move.l	d0,adpcm_stop_v-int_adpcm_stop(a1)
	cmp.l	a2,d0
	bcs	already_bye_

	move.l	$88.w,d0
	cmp.l	a2,d0
	bcs	already_bye_
	move.l	d0,dummy_vect-work(a6)
	clr.l	$88.w

	lea	adpcmout(pc),a1
	move.w	#$160,d1	*IOCS	_ADPCMOUTを乗っ取る
	IOCS	_B_INTVCS
	move.l	d0,adpcmout_v-adpcmout(a1)

	lea	adpcmmod(pc),a1
	move.w	#$167,d1	*IOCS	_ADPCMMODを乗っ取る
	IOCS	_B_INTVCS
	move.l	d0,adpcmmod_v-adpcmmod(a1)
@@:

	moveq.l	#35,d1		*trap #3を乗っ取る
	lea	Z_MUSIC_t3(pc),a1
	IOCS	_B_INTVCS
	move.l	d0,sv_trap3-Z_MUSIC_t3(a1)

	move.l	int_rte(pc),rte_src-work(a6)
	move.l	m_play00(pc),m_play00_bak-work(a6)
prt_title:				*タイトル表示
	tst.b	cmd_or_dev-work(a6)
	bne	@f
	lea.l	CRLF(pc),a0
	bsr	prta0
@@:
	bsr	prt_zmt
	lea.l	keep_mes(pc),a0
	bsr	prta0
	lea.l	mzl_mes(pc),a0
	bsr	prta0
	if	type=3
	lea.l	rsmidi_mes-work(a6),a0
	bsr	prta0
	endif

	moveq.l	#0,d0			*no problem
	rts

already_bye_:
	move.l	#1,-(sp)
	bra	@f
already_bye:
	move.l	#-1,-(sp)
@@:
	suba.l	a1,a1
	IOCS	_OPMINTST

	if	(type<>3.and.type<>4)
	move.l	rec_vect(pc),d0
	beq	@f
	move.l	d0,$228.w

	elseif	type=3
	move.l	rec_vect(pc),d0
	beq	@f
	move.l	d0,$170.w
@@:
	move.l	rec_vect_(pc),d0
	beq	@f
	move.l	d0,$174.w
	endif
@@:
*	move.l	vect_1f0(pc),d0		*タイニー版では_OPMDRVコールは使えない
*	beq	@f
*	move.w	#$1f0,d1
*	move.l	d0,a1
*	IOCS	_B_INTVCS		*set new IOCS call
					*ADPCMの停止処理の書き換え
	move.l	adpcm_stop_v(pc),d0
	beq	@f
	move.l	d0,$1a8.w
@@:
	move.l	(sp)+,d0		*error code
	rts

chk_board:			*MIDIボードのチェック

	if	type=0		*MIDI I/F
	bsr	rewrt_ff
	move.w	#1,-(sp)
	pea	icr
	pea	isr
	DOS	_BUS_ERR
	lea	10(sp),sp
	tst.l	d0
	seq	midi_board-work(a6)
	bsr	kill_md_chk
	bra	mdbd_patch

	elseif	type=3		*RS-MIDI
	bsr	rewrt_ff
	st.b	midi_board-work(a6)
	bra	mdbd_patch

	elseif	type=4		*POLYPHON

	bsr	rewrt_ff
	st.b	midi_board-work(a6)
	rts

	endif

find_dev_name:				*コマンドからドライバが実行された場合に
	lea	$6800,a0		*デバイス名 "ＯＰＭ" を強制的に登録する
fdn_lp01:
	lea	NUL(pc),a2
	pea	(a2)
	bsr	do_find
	cmpa.l	(sp)+,a0
	bcc	err_fdn
	cmpi.w	#$8024,-18(a0)	*本当にNULか
	bne	fdn_lp01
	lea	-22(a0),a0
fdn_lp02:
	movea.l	a0,a1
	movea.l	(a1),a0		*最後尾を見付ける
	cmpa.l	#$ffffffff,a0
	bne	fdn_lp02
	lea	device_driver-work(a6),a0
	move.l	a0,(a1)
	moveq.l	#0,d0
	rts
err_fdn:
	moveq.l	#-1,d0
	rts

do_find:			*特定のデバイス名を探し出す
	move.b	(a2),d0
dfn_lp01:			*一文字目が見付かるまでループ
	cmp.b	(a0)+,d0
	bne	dfn_lp01

	move.l	a0,d0		*save a0 to d0
	lea	1(a2),a1
	moveq.l	#7-1,d1
dfn_lp02:
	cmpm.b	(a0)+,(a1)+
	bne	next_dfn
	dbra	d1,dfn_lp02
	rts
next_dfn:
	movea.l	d0,a0		*get back a0
	bra	do_find

	if	(type<>3.and.type<>4)
kill_md_chk:			*MIDIボードチェック無効化
	tst.b	midi_board-work(a6)
	bne	exit_kmcp
	st.b	midi_board-work(a6)
	st.b	emulate_mode-work(a6)
*	st.b	timer_a_mode-work(a6)
	lea	device_driver-work(a6),a1
	lea	kill_md_chk(pc),a3
kmlp0:
	lea	damasu+1(pc),a0
	lea	$eafa09,a2
	move.l	(a1),d0
	cmp.l	a2,d0		*grp4
	beq	kill_it
	subq.w	#1,a0		*a0=damasu
	subq.w	#6,a2
	cmp.l	a2,d0		*rgr
	bcs	@f
	lea	12(a2),a2
	cmp.l	a2,d0		*grp7
	bls	kill_it
@@:
	addq.w	#2,a1
kmcp:
	cmpa.l	a3,a1
	bls	kmlp0
exit_kmcp:
	rts
kill_it:
	move.l	a0,(a1)+
	bra	kmcp
	endif

rw_tbl:
	dc.w	rwff1-rw_tbl
	dc.w	rwff3-rw_tbl
	dc.w	0

rewrt_ff:			*$ff50～$ff7fを$ff80～$ffafに移動する
	movem.l	d0/a0-a1,-(sp)
	DOS	_VERNUM
	cmpi.w	#$0300,d0
	bcs	exit_rwff
	lea	rw_tbl(pc),a0
@@:
	move.w	(a0)+,d0
	beq	exit_rwff
	lea	rw_tbl(pc,d0.w),a1
	add.w	#$30,(a1)
	bra	@b
exit_rwff:
	movem.l	(sp)+,d0/a0-a1
	rts

set_patch:			*-a,-e,-i,-m スイッチ処理
	movem.l	d0/a0-a1,-(sp)
	movea.l	$88.w,a0
	move.l	-8(a0),d0
	move.b	#$20,d0
	cmpi.l	#'PCM ',d0
	seq.b	pcm8_flg-work(a6)
	if	type=4		*POLYPHON
	seq	midi_board-work(a6)
	endif
	bne	@f
	move.w	#$1fe,d0
	trap	#2		*ZMUSIC.XがいなくなるまでPCM8.Xは常駐解除を禁止する。
@@:
	if	type=4		*POLYPHON
	bsr	mdbd_patch
	endif
	move.b	pcm8_flg-work(a6),d0	*PCM8モードの時は強制的に多重割り込み対応モード
	or.b	mfp_mode-work(a6),d0
	bne	@f
	move.w	#BRA+((sr_restore_e-sr_restore-2).and.$ff),sr_restore-work(a6)
	move.w	#RTE,int_rte-work(a6)
*	bra	set_tm_patch		*!2.04
@@:
	lea	copy_key(pc),a0
	move.l	$b0.w,copy_org-work(a6)
	move.l	a0,$b0.w			*copy key vect kill
set_tm_patch:
	tst.b	timer_a_mode-work(a6)		*タイマＡモードか
	bne	@f
*	tst.b	midi_board-work(a6)		*MIDIボードがあるならMIDIタイマ
*	bne	midi_tm
	move.w	#NOP,d0				*タイマＢモード
	move.w	d0,init_timer-work(a6)
	move.w	d0,calc_tm_b-work(a6)
	move.w	d0,calc_tm_b_-work(a6)
	move.w	d0,gyakusan_t-work(a6)
	move.b	#%0011_1010,reset_tm+3-work(a6)
	move.w	#$d7,timer_i_v+2-work(a6)
	move.l	#BSR*65536+((set_timer-wrt_tmp-2).and.$ffff),wrt_tmp-work(a6)
@@:
	tst.b	synchro_mode-work(a6)		*外部シーケンサ同期モードか
	bne	@f				*非同期モード
	move.l	#BRA*65536+((t_dat_ok-m_tempo_patch-2).and.$ffff),m_tempo_patch-work(a6)
	move.l	#BRA*65536+((t_dat_ok-m_play_patch-2).and.$ffff),m_play_patch-work(a6)
	move.l	#BRA*65536+((t_dat_ok-m_stop_patch-2).and.$ffff),m_stop_patch-work(a6)
	move.l	#BRA*65536+((t_dat_ok-m_cont_patch-2).and.$ffff),m_cont_patch-work(a6)
	move.l	#BRA*65536+((next_cmd-_@t_midi_clk-2).and.$ffff),_@t_midi_clk-work(a6)
@@:
	bsr	pcm8_patch
	movem.l	(sp)+,d0/a0-a1
	rts

mdbd_patch:				*ＭＩＤＩボードの有無の考慮
	movem.l	d0-d1/a0,-(sp)
	tst.b	midi_board-work(a6)
	beq	nmdb_patch
mdb_patch:
	lea	mdb_tbl(pc),a0
	bra	do_ncp
nmdb_patch:
	lea	nmdb_tbl(pc),a0
	bra	do_ncp
*cnv_patch:				*コンパイルモードのパッチ
*	movem.l	d0-d1/a0,-(sp)
*	move.w	#NOP,cf_patch-work(a6)
*	lea	cp_tbl(pc),a0
*	bra	do_ncp
*non_cnv_patch:
*	movem.l	d0-d1/a0,-(sp)
*	lea	ncp_tbl(pc),a0
*	bra	do_ncp
pcm8_patch:				*ＰＣＭ８モードのパッチ
	movem.l	d0-d1/a0,-(sp)
	tst.b	pcm8_flg-work(a6)
	beq	npcm8_patch
	lea	pcm8_tbl(pc),a0
	bra	do_ncp
npcm8_patch:
	lea	npcm8_tbl(pc),a0
*	bra	do_ncp
do_ncp:
	move.l	#BRA*65536,d1
@@:
	move.w	(a0)+,d0
	beq	exit_ncp
	move.w	(a0)+,d1
	move.l	d1,(a6,d0.w)
	bra	@b
exit_ncp:
	movem.l	(sp)+,d0-d1/a0
	rts

nmdb_tbl:
	dc.w	dev_inp-work,ok_com-dev_inp-2
	dc.w	nmdb0-work,ok_com-nmdb0-2
	dc.w	init_midi-work,skip_im-init_midi-2

	dc.w	m_tempo_patch-work,t_dat_ok-m_tempo_patch-2
	dc.w	m_play_patch-work,t_dat_ok-m_play_patch-2
	dc.w	m_stop_patch-work,t_dat_ok-m_stop_patch-2
	dc.w	nmdb1-work,msko0-nmdb1-2
	dc.w	m_cont_patch-work,t_dat_ok-m_cont_patch-2
	dc.w	init_midibd-work,exit_i_mdbd-init_midibd-2
	dc.w	nmdb2-work,pcd_err-nmdb2-2
	dc.w	nmdb3-work,ako2_lp-nmdb3-2
	dc.w	nmdb4-work,ako_sub_exit-nmdb4-2
*	dc.w	nmdb5-work,t_err_68-nmdb5-2
*	dc.w	nmdb6-work,t_err_68-nmdb6-2
	dc.w	nmdb7-work,t_dat_ok-nmdb7-2
	dc.w	nmdb8-work,exit_stn-nmdb8-2
	dc.w	nmdb9-work,ako00-nmdb9-2
	dc.w	nmdb10-work,su_not_ok-nmdb10-2
	dc.w	synchro_on-work,chk_dev_lp-synchro_on-2
	dc.w	nmdb11-work,exit_pbm-nmdb11-2
	dc.w	midi_inp1-work,t_err_68-midi_inp1-2
	dc.w	midi_out1-work,t_err_68-midi_out1-2
	dc.w	0

mdb_tbl:

	dc.w	0

pcm8_tbl:	*pcm8 mode
	dc.w	adpcmout-work,PCM8KON-adpcmout-2
	dc.w	adpcm_end-work,PCM8KOFF-adpcm_end-2
	dc.w	adpcm_keyon-work,PCM8_keyon-adpcm_keyon-2
	dc.w	pc8_0-work,fo_pcm8-pc8_0-2
	dc.w	opmd_y14_ope-work,jump_plus1-opmd_y14_ope-2
	dc.w	pan_a0-work,pan_adpcm_8-pan_a0-2
	dc.w	sea1-work,sea1_-sea1-2
	dc.w	sea2-work,sea2_-sea2-2
	dc.w	0

npcm8_tbl:	*not pcm8 mode

	dc.w	npc8_0-work,mkp8-npc8_0-2
	dc.w	mst_pcm8-work,mstlp-mst_pcm8-2
	dc.w	inc_pcmch-work,_pex0-inc_pcmch-2
	dc.w	npc8_1-work,adof-npc8_1-2
	dc.w	npc8_2-work,en_or_di-npc8_2-2
	dc.w	npc8_3-work,sopl_chk-npc8_3-2
	dc.w	npc8_4-work,fo_1-npc8_4-2
	dc.w	pcm8_fie-work,fie0-pcm8_fie-2
	dc.w	adpcm_volume-work,next_cmd-adpcm_volume-2
	dc.w	npc8_5-work,stot1-npc8_5-2
	dc.w	npc8_6-work,ac0-npc8_6-2
	dc.w	0

prt_help:			*簡易ヘルプの表示
	tst.b	cmd_or_dev-work(a6)	*deviceの時はコンパイルは無効
	beq	chk_dev_lp
	bsr	prt_title
	lea	help_mes(pc),a0
	bra	__end

*compile:			*コンパイルモード
*	tst.b	cmd_or_dev-work(a6)	*deviceの時はコンパイルは無効
*	beq	chk_dev_lp
*	clr.b	adpcm_read_flg-work(a6)	*コンパイルだから不要
*	bra	go_compile

get_work_area:			*ワークエリアの確保(+8はダミー)
trk_po_tbl__:	equ	tr_max*4	*+8
seq_wk_tbl__:	equ	wk_size*tr_max	*+8
seq_wk_tbl2__:	equ	wk_size*32	*+8	*pl_max
neiro__:	equ	neiro_size*tone_max	*+8
uwv__:		equ	wv_max*14	*+8
ttlwksz:	equ	trk_po_tbl__+seq_wk_tbl__+seq_wk_tbl2__+neiro__+uwv__
	lea.l	work_start(pc),a1

	move.l	a1,trk_po_tbl-work(a6)
	lea	trk_po_tbl__(a1),a1

	move.l	a1,seq_wk_tbl-work(a6)
	lea	seq_wk_tbl__(a1),a1

	move.l	a1,seq_wk_tbl2-work(a6)
	lea	seq_wk_tbl2__(a1),a1

	move.l	a1,dmy_seq_wk-work(a6)
	lea	wk_size(a1),a1

	move.l	a1,neiro-work(a6)
	lea	neiro__(a1),a1

	move.l	a1,wave_tbl-work(a6)
	lea	uwv__(a1),a1

	move.l	adpcm_n_max(pc),d0
	beq	@f
	move.l	a1,adpcm_tbl-work(a6)
	lsl.l	#3,d0			*8倍
	add.l	d0,a1
@@:
	move.l	a1,adpcm_buffer_top-work(a6)
	move.l	a1,adpcm_buffer_next-work(a6)
	clr.b	(a1)			*for mzp
	add.l	adpcm_buffer_size(pc),a1
	move.l	a1,adpcm_buffer_end-work(a6)

	move.l	a1,adpcm_work_top-work(a6)
	tst.l	adpcm_work_size-work(a6)
	bne	@f
	move.l	#at_least_wk,adpcm_work_size-work(a6)	*最低限取っておく
@@:
	add.l	adpcm_work_size(pc),a1
	move.l	a1,adpcm_work_end-work(a6)
	move.l	adpcm_work_size(pc),adpcm_work_true_size-work(a6)

						*トラックバッファはワークの最後にする
	move.l	a1,trk_top-work(a6)		*トラックバッファの先頭アドレス
	adda.l	trk_buf_size(pc),a1		*トラックバッファのエンドアドレス
	move.l	a1,dev_end_adr-work(a6)
	move.w	#1,-(sp)
	pea	(a1)
	pea	(a1)
	DOS	_BUS_ERR
	lea	10(sp),sp
	tst.l	d0
	rts

prt_bf_mes:			*何のために何バイトメモリを確保したのか等を表示
	move.l	trk_buf_size(pc),d1
	moveq.l	#10,d0
	lsr.l	d0,d1
	move.l	d1,d0
	bsr	num_to_str
	bsr	_ookisa
	lea	trkbf_mes(pc),a0	*メッセージ表示
	bsr	prta0

	move.l	adpcm_buffer_size(pc),d1
	moveq.l	#10,d0
	lsr.l	d0,d1
	move.l	d1,d0
	beq	@f
	bsr	num_to_str
	bsr	_ookisa
	lea	pcmbf_mes(pc),a0	*メッセージ表示
	bsr	prta0
@@:
	move.l	adpcm_work_size(pc),d1
	moveq.l	#10,d0
	lsr.l	d0,d1
	move.l	d1,d0
	cmpi.l	#at_least_wk,adpcm_work_size-work(a6)
	bls	@f
	bsr	num_to_str
	bsr	_ookisa
	lea	wkbf_mes(pc),a0	*メッセージ表示
	bsr	prta0
@@:
	tst.b	timer_a_mode-work(a6)
	beq	@f
	lea	tm_a_mes(pc),a0
	bsr	prta0
@@:
	tst.b	synchro_mode-work(a6)
	beq	@f
	lea	sq_m_mes(pc),a0
	bsr	prta0
@@:
	tst.b	pcm8_flg-work(a6)
	beq	pbm0
	lea	pcm8_mode_mes(pc),a0
	if	type=4
	tst.b	midi_board-work(a6)
	beq	@f
	lea	pcm8_mode_mes_(pc),a0
@@:
	endif
	bsr	prta0
pbm0:
	lea	no_midi(pc),a0
nmdb11:				*nmdb!!
	lea	yes_midi(pc),a0
exit_pbm:
	bra	prta0
_ookisa:
	lea	secure_mes(pc),a0
	bsr	prta0
	lea	atrb_0010(pc),a0
	bsr	prta0
	lea	suji(pc),a0
	bsr	prta0
	lea	kakuho_mes(pc),a0
	bra	prta0

unknown_err:
	lea	unknown_mes(pc),a1
err_exit:
	bsr	prta1

	move.l	ssp(pc),a1
	IOCS	_B_SUPER	*ユーザーモードへ戻る

	move.w	#1,-(sp)
	DOS	_EXIT2

exit_oo:			*すでに常駐または常駐不可
	tst.l	d0
	bpl	prt_am2_
	lea	already_mes(pc),a0
	bsr	prta0
	bra	go_user_bye
prt_am2_:
	lea	already_mes2(pc),a1	*COULDN'T SET INT ADPCM VECT
	bra	err_exit

release:			*解除処理
	bsr	kep_chk		*常駐check
	bmi	not_kep		*常駐していない

	movea.l	a0work(pc),a0
	move.l	a2work(pc),d7
	sub.l	a0,d7

	lea	ver_num-work(a6),a1	*バージョンチェック
	move.w	(a1,d7.l),d0
	cmp.w	(a1),d0
	bne	illegal_ver

	movea.l	a1work(pc),a1
	lea	$10(a0),a0	*メモリブロックの変更
	suba.l	a0,a1
	pea	(a1)
	pea	(a0)
	DOS	_SETBLOCK
	addq.w	#8,sp

	move.l	#$0d,d1		*init_all
	trap	#3

	lea	support_mode(pc),a1	*サポートプログラムの解除
	adda.l	d7,a1
	moveq.l	#0,d0
	moveq.l	#sp_max-1,d1
sprl_lp:
	tst.b	(a1)+
	beq	@f
	lea	support_name(pc),a1
	movem.l	d0-d7/a0-a7,h_work-support_name(a1)
	clr.l	-(sp)
	pea	int_entry(pc)		*使い捨てとなるわけだからどこでもいい
	mulu	#spt_size,d0
	add.l	d7,a1
	pea	(a1,d0.l)		*filename
	move.w	#2,-(sp)		*mode
	DOS	_EXEC
	addq.w	#2,sp
	clr.w	-(sp)			*mode
	DOS	_EXEC
	lea	14(sp),sp
	movem.l	h_work(pc),d0-d7/a0-a7
@@:
	addq.w	#1,d0
	dbra	d1,sprl_lp

	bsr	kill_OPM	*デバイス名をシステムワークより除去
	bmi	release_err

back_asv:
	lea	pcm8_flg(pc),a1
	tst.b	(a1,d7.l)
	beq	@f
	move.w	#$1ff,d0
	trap	#2			*常駐解除許可
	if	type=4
	move.w	#$0302,d0
	trap	#2			*常駐解除許可
	endif
	bra	gb_abort
@@:
	lea	adpcm_stop_v(pc),a1
	move.l	(a1,d7.l),$1a8.w	*get back int adpcm stop vector

	lea	adpcmout_v(pc),a1
	move.w	#$160,d1
	movea.l	(a1,d7.l),a1
	IOCS	_B_INTVCS	*get back int adpcmout vector

	lea	adpcmmod_v(pc),a1
	move.w	#$167,d1
	movea.l	(a1,d7.l),a1
	IOCS	_B_INTVCS	*get back int adpcmmod vector

	lea	dummy_vect(pc),a1
	move.l	(a1,d7.l),$88.w
gb_abort:
*	lea	vect_1f0(pc),a1
*	move.w	#$1f0,d1
*	movea.l	(a1,d7.l),a1
*	IOCS	_B_INTVCS	*get back $1f0 vector

	if	(type<>3.and.type<>4)
	lea	rec_vect(pc),a1
	move.l	(a1,d7.l),d0
	beq	@f
	move.l	d0,$228.w	*get back $8a vector

	elseif	type=3
	lea	rec_vect(pc),a1
	move.l	(a1,d7.l),d0
	beq	@f
	move.l	d0,$170.w
@@:
	lea	rec_vect_(pc),a1
	move.l	(a1,d7.l),d0
	beq	@f
	move.l	d0,$174.w
	endif
@@:
	suba.l	a1,a1
	IOCS	_OPMINTST
*bk_sv:
	bsr	back_copy_key

	lea	sv_trap3(pc),a1
	move.l	(a1,d7.l),$8c.w	*get back trap #3 vector

	move.l	a2work(pc),a2
	clr.b	$c4(a2)
	pea	$10(a2)
	DOS	_MFREE
	addq.w	#4,sp

	bsr	release_mes

go_user_bye:
	move.l	ssp(pc),a1
	IOCS	_B_SUPER	*ユーザーモードへ戻る

	moveq.l	#$e0,d1
	moveq.l	#-1,d2
	moveq.l	#32-1,d3
@@:
	IOCS	_OPMSET
	addq.l	#1,d1
	dbra	d3,@b

	DOS	_EXIT

prt_zmt:
	move.w	#$0002,-(a7)
	pea	zmt(pc)
	DOS	_FPUTS
	addq.w	#6,a7
	rts

release_mes:
	bsr	prt_zmt
	move.w	#$0002,-(a7)
	pea	kaijo(pc)
	DOS	_FPUTS
	addq.w	#6,a7
	rts

illegal_ver:			*VERSIONが違う
	lea	ver_er_mes(pc),a1
	bra	err_exit
release_err:			*解除不能の状態
	bsr	prt_zmt
	lea	rls_er_mes(pc),a1
	bra	err_exit
not_kep:			*常駐していない
	bsr	chk_drv
	beq	release_err	*device=で登録された?
	bsr	prt_zmt
	lea	not_kep_mes(pc),a1
	bra	err_exit

chk_drv:			*デバイス名のcheck
	* > eq=no error
	* > mi=error
	move.l	$8c.w,a1
	subq.w	#8,a1
	cmpi.l	#'ZmuS',(a1)+
	bne	chk_drv_err
	cmpi.w	#'iC',(a1)+
	bne	chk_drv_err
	rts
chk_drv_err:
	moveq.l	#-1,d0
	rts

back_copy_key:			*COPY KEY VECTOR復元
	movem.l	d0-d1/a0-a3,-(sp)
	move.l	a0work(pc),a0
@@:
	move.l	(a0),d0
	beq	@f
	move.l	d0,a0
	bra	@b
@@:
	lea	copy_org(pc),a1
	move.l	(a1,d7.l),d1
	move.l	a2work(pc),a2
bck_lp:
	move.l	4(a2),d0
	beq	exit_bck
	andi.l	#$ff_ffff,d0
	move.l	d0,a2

	move.l	a0,a3
@@:
	cmp.l	a3,a2		*ちゃんと生きている
	beq	@f
	move.l	12(a3),d0
	beq	bck_lp
	move.l	d0,a3
	bra	@b
@@:
	move.l	d1,$54(a2)
	bra	bck_lp
exit_bck:
	movem.l	(sp)+,d0-d1/a0-a3
	rts

kep_chk:			*分身が常駐しているか
	* > eq=exists
	* > ne=none
	move.l	a0work(pc),a0
klop0:
	move.l	(a0),d0
	beq.s	klop1
	move.l	d0,a0
	bra.s	klop0
klop1:
	move.l	12(a0),d0	*次のメモリ管理ポインタ
	beq.s	err_chk
	movea.l	d0,a2
	cmpa.l	a0work(pc),a2
	beq.s	klop_nxt
	bsr.s	str_chk
	beq.s	end_chk		*find it
klop_nxt:
	move.l	a2,a0
	bra.s	klop1		*どんどんさかのぼる

end_chk:
	move.l	a2,a2work-work(a6)
	moveq.l	#0,d0
	rts
err_chk:
	moveq.l	#-1,d0
	rts

str_chk:
	* > ne=none
	* > eq=exists
	lea	pname(pc),a0	*このプロセスの名前
	lea	$c4(a2),a1
slop1:				*同じプロセス名が存在するか
	move.b	(a0)+,d2
	andi.b	#$df,d2		*小文字から大文字へ変換
	move.b	(a1)+,d3
	andi.b	#$df,d3		*小文字から大文字へ変換
	cmp.b	d3,d2
	bne.s	noteq		*違う
	tst.b	(a0)
	bne	slop1
equal:
	moveq.l	#0,d0		*d0= 0で同じものが存在と知らせる
	rts
noteq:
	moveq.l	#-1,d0
	rts			*d0<>0で同じものが存在しなかったと知らせる

kill_OPM:			*デバイス名の除去
	* > eq=no error
	* > mi=error
	lea	$6800,a0		*デバイス名”ＯＰＭ”を強制的に削除する
KO_lp01:
	lea	NUL(pc),a2
	pea	(a2)
	bsr	do_find
	cmpa.l	(sp)+,a0
	bcc	KO_err_KO
	cmpi.w	#$8024,-18(a0)	*本当にNULか
	bne	KO_lp01

	lea	-22(a0),a1
	move.l	a1,nul_address-work(a6)

	lea	dev_name-work(a6),a2
	bsr	rmk_heap			*ヒープ調査(return:a1,a0)
	bmi	KO_err_KO			*case:error
	move.l	dev_header2-work(a6),(a0)	*ヒープを再構成して自分は抜ける

	moveq.l	#0,d0
	rts
KO_err_KO:
	moveq.l	#-1,d0
	rts
rmk_heap
	* < a1.l=NUL
	* < a2.l=抜けたいデバイス名
KO_lp02:
	bsr	same_dev?
	beq	KO_OK
	cmpi.l	#$ffffffff,(a1)	*最後尾に来てしまったか
	beq	err_KO
	movea.l	a1,a0		*一つ前のをキープ
	move.l	(a1),a1		*次へ
	bra	KO_lp02
KO_OK:
	moveq.l	#0,d0
	rts
err_KO:
	moveq.l	#-1,d0
	rts

same_dev?:			*同じ名前かどうか
	* < a2.l=source name
	* > mi=not same
	* > eq=same
	movem.l	a1-a2,-(sp)
	lea	14(a1),a1
	moveq.l	#8-1,d1
sdv_lp02:
	cmp.b	(a1)+,(a2)+
	bne	exit_sdv
	dbra	d1,sdv_lp02
	moveq.l	#0,d0
	movem.l	(sp)+,a1-a2
	rts
exit_sdv:
	moveq.l	#-1,d0
	movem.l	(sp)+,a1-a2
	rts
__end:
	bsr	prta0
	bra	go_user_bye

end_of_prog:

	end	exec
