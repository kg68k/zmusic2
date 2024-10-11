*	ZMDデータ中のMIDI関係のコマンドをテキスト化してファイルに書き出す
*
*		ZMD18.R	version 2.00
*
	.include	iocscall.mac
	.include	doscall.mac


prog_start:
	lea	work(pc),a6

	lea	$10(a0),a0		*メモリブロックの変更
	lea	end_of_prog(pc),a1
	suba.l	a0,a1
	pea	(a1)
	pea	(a0)
	DOS	_SETBLOCK
	addq.w	#8,sp

	bsr	print_title

	tst.b	(a2)+
	beq	print_help		*簡易ヘルプを表示
LL1:
	move.b	(a2)+,d0
	beq	print_help
	cmpi.b	#' ',d0
	bls	LL1
	subq.w	#1,a2

	lea	sorce_name(pc),a0
LL2:
	move.b	(a2)+,d0
	cmpi.b	#' ',d0
	bls	LL3
	move.b	d0,(a0)+
	bra	LL2
LL3:
	clr.b	(a0)+			*end
	subq.w	#1,a2

	bra	main_ope

kakuchoshi:			*拡張子を設定
	* < a0=filename address
	* X a0
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
	move.b	#'Z',(a0)+
	move.b	#'M',(a0)+
	move.b	#'D',(a0)+
	clr.b	(a0)
	rts
find_period:
	cmpi.b	#' ',(a0)
	bls	do_kkchs	*'.'はあっても拡張子がないケース
	rts

main_ope:
	lea	sorce_name(pc),a0
	bsr	kakuchoshi

	lea	sorce_name(pc),a2
	bsr	fopen
	tst.b	d5
	bmi	open_err
	bsr	read		*>d3.l=size a5.l=address
	bsr	process
	DOS	_EXIT

process:
	* < a5.l=sorce zmd data address
	* < a4.l=書き込み先バッファ
	move.l	(a5)+,d0
	cmpi.l	#$105a_6d75,d0	*ヘッダチェック
	bne	unid_err
	move.l	(a5)+,d0
	clr.b	d0
	cmpi.l	#$5369_4300,d0	*ヘッダチェック
	bne	unid_err
	moveq.l	#0,d2		*clear flag
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
	beq	single_adpcm	*adpcm data cnf
	cmpi.b	#$4a,d1
	beq	cnv_wvf		*wave form setting
	cmpi.b	#$60,d1
	beq	skip_until0	*#com0:read opmd.cnf
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
	bra	unid_err	*unknown command
exit_ccd:
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
	move.b	(a5)+,d3
	lsl.w	#8,d3
	move.b	(a5)+,d3	*get how many
cmlp:
	move.b	(a5)+,d1
	tst.b	d2
	bne	LL4
	moveq.l	#16,d2
	pea	atx(pc)
	DOS	_PRINT		*@X
	addq.w	#4,sp
LL4:
	bsr	wrt_1b
	subq.w	#1,d2
	beq	LL5
	cmpi.b	#$f7,d1
	bne	camma
	moveq.l	#0,d2
LL5:
	pea	crlf(pc)
	DOS	_PRINT
	addq.w	#4,sp
	subq.w	#1,d3
	bne	cmlp
	bra	pcd_lp01
camma:
	move.w	#',',-(sp)
	DOS	_PUTCHAR
	addq.w	#2,sp
	subq.w	#1,d3
	bne	cmlp
	bra	pcd_lp01

wrt_1b:
	move.w	d1,-(sp)
	move.w	#'$',-(sp)
	DOS	_PUTCHAR
	addq.w	#2,sp
	move.b	d1,d0
	lsr.b	#4,d0
	add.b	#'0',d0
	cmpi.b	#'9',d0
	bls	LL6
	addq.b	#7,d0
LL6:
	move.w	d0,-(sp)
	DOS	_PUTCHAR	*upper
	addq.w	#2,sp
	andi.w	#$0f,d1
	add.b	#'0',d1
	cmpi.b	#'9',d1
	bls	LL7
	addq.b	#7,d1
LL7:
	move.w	d1,-(sp)
	DOS	_PUTCHAR	*lower
	addq.w	#2,sp
	move.w	(sp)+,d1
	rts

*cnv_vset2:	*ボイスセット２
*	skip	50

single_adpcm:
	add.l	#2+4+4+4+4+1,a5

	tst.b	(a5)
	bne	srch_fn_end
	addq.w	#4,a5		*直接値の場合
	bra	pcd_lp01
srch_fn_end:			*ファイル名の場合
	tst.b	(a5)+
	bne	srch_fn_end	*ファイルネームのエンドを捜して
	bra	pcd_lp01	*ループに戻る

cnv_wvf:
	moveq.l	#0,d0
	move.b	(a5)+,d0
	lsl.w	#8,d0
	move.b	(a5)+,d0	*get number of data
	add.l	d0,d0
	lea	4(a5,d0.l),a5	*skip wave_n,loop_type,loop_addr.
	bra	pcd_lp01

cnv_midi:	*#com22:trans midi data dump
cnv_abdt:	*#com35:read adpcm block data
skip_until0:
	moveq.l	#0,d2
adbt_lp:
	tst.b	(a5)+
	bne	adbt_lp
	bra	pcd_lp01

fopen:				*ファイルのオープン(環境変数参照モード)
	* < a2=file name
	* > d5=file handle (error:d5<0)
	* - all 
	movem.l	d0/a0-a2,-(sp)

	clr.w	-(sp)
	pea     (a2)
	DOS	_OPEN
	addq.w	#6,sp
	move.l	d0,d5		*d5.w=file handle
	bpl	exit_fopen	*no problem

	movea.l	a2,a0		*a0=file name only(non pathed)

	pea.l	env_work(pc)
	clr.l	-(sp)
	pea	getname(pc)
	DOS	_GETENV
	lea	12(sp),sp
	tst.l	d0
	bmi	exit_fopen

	lea.l	env_work(pc),a1
fopen_lp01:
	lea	open_fn(pc),a2
fopen_lp02:
	move.b	(a1)+,d0
	beq	do_fopen
	cmpi.b	#';',d0
	beq	do_fopen
	move.b	d0,(a2)+
	bra	fopen_lp02
do_fopen:
	moveq.l	#'\',d0
	cmp.b	-1(a2),d0
	beq	sva0
	cmp.b	(a0),d0
	beq	sva0_
	move.b	d0,(a2)+
sva0:
	cmp.b	(a0),d0		*'\'が続いた場合
	bne	sva0_
	addq.w	#1,a0
	bra	sva0
sva0_:
	pea	(a0)
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
	tst.b	-1(a1)		*まだ環境変数が残ってるか
	bne	fopen_lp01
exit_fopen:
	movem.l	(sp)+,d0/a0-a2
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
	addq.w	#8,sp

	move.l	d3,-(sp)	*push size
	pea	(a5)		*push addr
	move.w	d5,-(sp)	*file handle
	DOS	_READ
	lea	10(sp),sp
	tst.l	d0
	bmi	read_err	*読み込み失敗

	move.w	d5,-(sp)	*close
	DOS	_CLOSE
	addq.l	#2,sp
	rts

print_title:
	move.w	#2,-(sp)
	pea	title(pc)
	DOS	_FPUTS
	addq.w	#6,sp
	rts

print_help:
	lea	help_mes(pc),a1
	bsr	mes_prt
bye_bye:
	DOS	_EXIT

err_bye:
	bsr	mes_prt
	move.w	#-1,-(sp)
	DOS	_EXIT2

prt_sfn:
	lea	sorce_name(pc),a1
	bra	mes_prt

read_err:
	bsr	prt_sfn
	lea	read_err_mes(pc),a1
	bra	err_bye

out_mem:
	lea	out_mem_mes(pc),a1
	bra	err_bye

open_err:
	bsr	prt_sfn
	lea	open_err_mes(pc),a1
	bra	err_bye

fsize0:
	lea	fsize0_mes(pc),a1
	bra	err_bye

unid_err:
	bsr	prt_sfn
	lea	unid_err_mes(pc),a1
	bra	err_bye

mes_prt:
	pea	(A1)
	DOS	_PRINT
	addq.w	#4,sp
	rts

work:
title:		dc.b	$1b,'[35mZMD18.R',$1b,'[m version 2.00 (C) 1992 '
		dc.b	$1b,'[36mZENJI SOFT',$1b,'[m',13,10,0
help_mes:	dc.b	$1b,'[37m(使い方)',13,10
		dc.b	$1b,'[m ZMD18 <読み込みファイルネーム[.ZMD]>',13,10
		dc.b	$1b,'[37m(機能)',13,10
		dc.b	$1b,'[m'
		dc.b	'  ＺＭＤファイル中のＭＩＤＩ出力関係の共通コマンド',13,10
		dc.b	'をＭＭＬの＠Ｘコマンドに変換してテキスト出力する。',13,10
		dc.b	'ＭＩＤＩ出力をしていないＺＭＤデータには全く無意味。',13,10
		dc.b	'もちろんＺＭＳファイルに対しても無意味。あしからず',13,10
		dc.b	0
read_err_mes:	dc.b	'が読めなかったよ',13,10,0
out_mem_mes:	dc.b	'メモリが不足しているよ! 増設しよう',13,10,0
open_err_mes:	dc.b	'が見つからなかったよ',13,10,0
fsize0_mes:	dc.b	'ファイルサイズが変だ!!',13,10,0
unid_err_mes:	dc.b	'はZMDデータじゃないみたいだね。',13,10
		dc.b	'バージョンが違うって言うはなしもある'
crlf:		dc.b	13,10,0
getname:	dc.b	'zmusic',0
atx:		dc.b	'(T1)@X',0
	.bss
sorce_name:	ds.b	91
open_fn:	ds.b	91
env_work:	ds.b	256
	.even
end_of_prog:
