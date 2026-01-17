
go_compile:			*コンパイル動作
	lea	b_clr_st(pc),a0
	bsr	prta0
	bsr	prt_title

	movea.l	a0work(pc),a0
	movea.l	a1work(pc),a1
	lea	$10(a0),a0	*メモリブロックの変更
	suba.l	a0,a1
	pea	(a1)
	pea	(a0)
	DOS	_SETBLOCK
	addq.w	#8,sp

	pea	stack.w
	DOS	_MALLOC
	move.l	d0,sp
	lea	stack(sp),sp		*スタックエリアの確保

*	move.l	a4,-(sp)
*	bsr	kep_chk			*常駐check
*	bmi	@f
*	move.l	a2work(pc),d1
*	sub.l	a0work(pc),d1
*	lea	timer_a_mode(pc),a1	*バージョンチェック
*	tst.b	(a1)
*	bne	@f
*	move.b	(a1,d1.l),(a1)
*@@:
*	move.l	(sp)+,a4

.ifdef POLYPHON
	st.b	midi_board-work(a6)
	bsr	mdbd_patch
.endif
	bsr	cnv_patch
	bsr	pcm8_patch

	st.b	compile_mode-work(a6)		*compile mode on

	moveq.l	#$7f,d4		*dummy length
	jsr	skip_spc-work(a6)
	lea	sr_filename(pc),a0
	jsr	copy_fn-work(a6)	*get source file name
	lea	sr_filename(pc),a0
	lea	ZMS(pc),a1
	jsr	kakuchoshi-work(a6)
	jsr	skip_sep-work(a6)
	tst.b	(a4)
	bne	copy_dest_fn
	bsr	mk_default_fn	*デフォルトのファイルネームを持ってくる
	bra	do_compile
copy_dest_fn:
	lea	sv_filename(pc),a0
	jsr	copy_fn-work(a6)	*get destination file name
	lea	sv_filename(pc),a0
	lea	ZMD(pc),a1
	jsr	kakuchoshi-work(a6)	*拡張子を設定
do_compile:
	bsr	read_source	*ソースファイルのリード(a4=data address/d4=data size)
	bsr	set_work_area	*ワークを設定
	bsr	init_zmusic	*コンパイルするための準備

	move.l	adpcm_work_end(pc),compile_p-work(a6)	*init compiled data pointer
	move.l	#1,line_number-work(a6)			*init. line number
	clr.l	num_of_err-work(a6)
	move.w	#compile_p-cmpp_patch1-2,cmpp_patch1+2-work(a6)
	move.w	#compile_p-cmpp_patch2-2,cmpp_patch2+2-work(a6)

	pea	compile_end(pc)		*set return address
	movem.l	d1-d7/a0-a6,-(sp)	*dev_out:のｼﾐｭﾚｰｼｮﾝ
	lea	-512(sp),sp
*	lea	work(pc),a6		*上で設定済み
	jmp	dev_o_lop-work(a6)
compile_end:				*コンパイルが終わるとここへ帰ってくる
	* < d0.l=error code
	move.l	d0,d1
	bne	compile_error
	tst.b	err_code-work(a6)	*コンパイル中にエラーを起こした
	bne	exit_compile
					*m_allocだけしてm_assignしていない場合をチェック
	moveq.l	#tr_max-1,d1
	moveq.l	#0,d2
	movea.l	trk_po_tbl(pc),a1
cns_lp:
	move.l	(a1)+,a0
	cmpi.b	#$ff,(a0)
	beq	cns_next
	lea	play_trk_tbl(pc),a2
@@:
	move.b	(a2)+,d0
	bmi	@f
	cmp.b	d0,d2
	beq	cns_next
	bra	@b
@@:
	lea	no_as_er_mes1(pc),a0
	bsr	prta0
	move.l	d2,d0
	addq.l	#1,d0
	bsr	num_to_str
	lea	suji(pc),a0
	bsr	prta0
	lea	no_as_er_mes2(pc),a0
	bsr	prta0
cns_next:
	addq.w	#1,d2
	lea	wk_size(a2),a2
	dbra	d1,cns_lp

	tst.b	no_optmz-work(a6)
	bmi	mk_zmd
kill_dummy_trk:			*確保して使用しなかったトラックを除去
	lea	play_trk_tbl(pc),a4
	moveq.l	#0,d0
@@:
	move.b	(a4)+,d0
	bmi	mk_zmd
	movea.l	trk_po_tbl(pc),a1
	add.w	d0,d0
	add.w	d0,d0
	move.l	(a1,d0.w),a1	*a1=trk start addr.
	cmpi.b	#$ff,(a1)
	bne	@b
@@:
	move.b	(a4)+,-2(a4)
	bmi	kill_dummy_trk
	bra	@b
mk_zmd:
	moveq.l	#0,d6			*total size
	movea.l	adpcm_work_top(pc),a1
	bsr	set_zm_id		*ID SET

	move.l	adpcm_work_end(pc),a2	*MMLデータ以外のコマンドを転送
	move.l	a2,d0
	sub.l	compile_p(pc),d0	*d0=size
	beq	set_endcd
	add.l	d0,d6		*d6=total size
	subq.l	#1,d0		*for dbra
dosv_lp:
	move.b	-(a2),(a1)+
	dbra	d0,dosv_lp
set_endcd:
	addq.l	#1,d6		*end codeの分
	st.b	(a1)+		*end code

	lea	play_trk_tbl(pc),a0
	moveq.l	#0,d0
dosv_lp00:			*有効なトラック数を数える
	tst.b	(a0)+
	bmi	exit_dosv_lp00
	addq.b	#1,d0
	bra	dosv_lp00	*d0=enable channels
exit_dosv_lp00:
	* < d0.l=numbers of enable tracks
	move.l	a1,d1
	btst.l	#0,d1
	beq	@f
	st.b	(a1)+		*.even処理
	addq.l	#1,d6
@@:
	move.w	d0,(a1)+	*set num of trk
	addq.l	#2,d6		*2=num of trk
	lea	4(a1),a0	*a0=trk_po_pointer+4
	mulu	#6,d0		*d0=d0*6 (offset.l,0,ch)ここ!!
	add.w	d0,a1		*a1=mml data save area
	add.w	d0,d6		*total size
	move.l	a1,compile_p-work(a6)

	lea	play_trk_tbl(pc),a4
	moveq.l	#5,d1
dosv_lp01:
	moveq.l	#0,d0
	move.b	(a4)+,d0
	bmi	exit_dosv	*$ffなら未使用
	bsr	calc_cnv_wk	*a1=cnv_wk_tbl n
	bsr	calc_wk		*a5=seq_wk_tbl n
	move.l	(a1),d2		*d2=trk data end addr.
	add.w	d0,d0
	add.w	d0,d0
	movea.l	trk_po_tbl(pc),a1
	move.l	(a1,d0.w),a1	*a1=trk start addr.(source)
	sub.l	a1,d2		*d2=trk data size
	addq.l	#1,d2		*include end code($FF)
	add.l	d2,d6		*d6=copmiled data total size
	cmp.l	adpcm_work_size(pc),d6
	bhi	_work_small	*work area is too small
	movea.l	compile_p(pc),a2	*destination
	bsr	trans_dma
	suba.l	a0,a2
	move.l	a2,-4(a0)	*offset
	move.b	p_ch(a5),d0
	move.w	d0,(a0)		*ch (6bytes)ここを変えたら上も変える
	cmpi.b	#8,d0
	bne	addq_a0
	move.b	p_extra_ch(a5),d0	*pcm8考慮
	beq	@f
	add.b	#24,d0
	move.w	d0,(a0)
	bra	addq_a0
@@:
	ori.w	#$8000,(a0)
addq_a0:
	addq.w	#6,a0
	add.l	d2,compile_p-work(a6)	*make next p
	bra	dosv_lp01
exit_dosv:			*コンパイルデータのセーブ
	* < d6=copmiled data total size
				*ファイルの書き出し
	move.w	#32,-(sp)
	pea	sv_filename(pc)
	DOS	_CREATE
	addq.w	#6,sp
	move.l	d0,d5		*d5.w=file handle
	bmi	_open_err

	move.l	d6,-(sp)	*data size
	move.l	adpcm_work_top(pc),-(sp)		*data addr
	move.w	d5,-(sp)
	DOS	_WRITE
	lea	10(sp),sp
	cmp.l	d0,d6
	bne	_write_err

	move.l	date_buf(pc),-(sp)
	move.w	d5,-(sp)
rwff2:
	DOS	_V2_FILEDATE
	addq.w	#6,sp

	jsr	do_fclose-work(a6)

	lea	no_err_mes(pc),a1
	bsr	prta1

	tst.b	clcttl_mode-work(a6)
	beq	exit_compile_

	lea	print_calc(pc),a1
	bsr	prta1

	move.l	#NOP_NOP,wrt_tmp-work(a6)
	moveq	#$19,d1
	moveq.l	#0,d2
	jsr	Z_MUSIC-work(a6)
	lea	b_era_st(pc),a0
	bsr	prta0

	bra	exit_compile_

init_zmusic:			*初期化(FOR COMPILE MODE)
	movem.l	d0-d2/a0-a6,-(sp)

	jsr	init_trk_len-work(a6)
	jsr	trk_top_chk-work(a6)
	bmi	_out_trk_err
	jsr	trk_top_set-work(a6)
	jsr	init_play_trk_tbl-work(a6)
	jsr	tr_top_set-work(a6)
	jsr	tr_end_set-work(a6)
	jsr	top_ptr_set_all-work(a6)

	move.l	#bank_max*128,adpcm_n_max-work(a6)
	move.w	#120,m_tmp_buf-work(a6)
	jsr	init_cnv_wk-work(a6)
	jsr	init_play_wk-work(a6)
*	move.l	#$0001_007e,d2
*	bsr	m_alloc			*default
	movem.l	(sp)+,d0-d2/a0-a6
	rts

read_source:			*SOURCE FILEの読み込み
	* > a4.l=data address
	* > d4.l=data size

	clr.w	-(sp)
	pea     sr_filename(pc)
	DOS	_OPEN
	addq.w	#6,sp
	move.l	d0,d5		*d5.w=file handle
	bpl	get_sr_fsize
	lea	sr_filename(pc),a4
@@:
	tst.b	(a4)+
	bne	@b
	subq.l	#4,a4
	cmpi.b	#'Z',(a4)+
	bne	_read_err
	cmpi.b	#'M',(a4)+
	bne	_read_err
	cmpi.b	#'S',(a4)+
	bne	_read_err
	move.b	#'M',-(a4)
	move.b	#'P',-(a4)
	move.b	#'O',-(a4)
	bra	read_source
get_sr_fsize:
	jsr	get_fsize-work(a6)	*>d3.l=file size
	bmi	_read_err
	move.l	d3,d4

	move.l	d4,-(sp)	*data size
	DOS	_MALLOC
	addq.w	#4,sp
	tst.l	d0
	bmi	_out_mem_err
	movea.l	d0,a4		*a4=address

	move.l	d4,-(sp)	*size
	pea	(a4)		*address
	move.w	d5,-(sp)
	DOS	_READ
	lea	10(sp),sp
	tst.l	d0
	bmi	_read_err

	jsr	get_filedate-work(a6)
	move.l	d0,date_buf-work(a6)

	jmp	do_fclose-work(a6)

set_zm_id:			*(a1)からＩＤをセット
	* < a1.l=address
	move.l	d0,-(sp)
	move.l	#$105a6d75,(a1)+	*header number
	move.l	#$53694300,d0
	move.b	ver_num-work(a6),d0		*version number
	move.l	d0,(a1)+
	addq.l	#8,d6
	move.l	(sp)+,d0
	rts

set_work_area:			*コンパイルワークを設定
	* X d0 a0
	* neiro,adpcm_tbl,adpcm_buffer,seq_wk_tbl2は確保しない

	move.l	#trk_len_tbl__,d0
	bsr	sw_malloc
	move.l	d0,trk_len_tbl-work(a6)

	move.l	#trk_po_tbl__,d0
	bsr	sw_malloc
	move.l	d0,trk_po_tbl-work(a6)

	move.l	#seq_wk_tbl__,d0
	bsr	sw_malloc
	move.l	d0,seq_wk_tbl-work(a6)

	move.l	#cnv_wk_tbl__,d0
	bsr	sw_malloc
	move.l	d0,cnv_wk_tbl-work(a6)

	tst.l	adpcm_work_size-work(a6)
	bne	kkho_awt
	move.l	#at_least_wk2,adpcm_work_size-work(a6)	*最低限取っておく
kkho_awt:
	move.l	adpcm_work_size(pc),d0
	bsr	sw_malloc
	move.l	d0,adpcm_work_top-work(a6)
	move.l	d0,adpcm_work_now-work(a6)
	add.l	adpcm_work_size(pc),d0
	move.l	d0,adpcm_work_end-work(a6)
	move.l	adpcm_work_size(pc),adpcm_work_true_size-work(a6)

	tst.l	trk_buf_size-work(a6)
	bne	kkho_tbs
	move.l	#dflt_trkbf,trk_buf_size-work(a6)	*トラックバッファ確保
kkho_tbs:
	move.l	trk_buf_size(pc),d0
	bsr	sw_malloc
	move.l	d0,trk_top-work(a6)		*d0=トラックバッファの先頭アドレス
	rts

sw_malloc:
	* < d0.l=size
	* > d0.l=address
	move.l	d0,-(sp)
	DOS	_MALLOC
	addq.w	#4,sp
	tst.l	d0
	bmi	_out_mem_err
	rts

mk_default_fn:			*デフォルトのファイルネームを作る
	movem.l	d0-d1/a0-a1,-(sp)
	lea	sr_filename(pc),a0
	jsr	skip_peri-work(a6)
mdf_:
	lea	sv_filename(pc),a1
	moveq.l	#0,d1
	tst.b	(a0)
	bmi	@f
	cmpi.b	#':',1(a0)
	bne	@f
	addq.w	#2,a0
@@:
mdf_lp:
	move.b	(a0)+,d0
	beq	exit_mdf
	bpl	@f
	cmpi.b	#$a0,d0
	bcs	mdfknj
	cmpi.b	#$df,d0
	bls	@f
mdfknj:
	move.b	d0,(a1)+	*漢字はペア
	move.b	(a0)+,(a1)+
	bra	mdf_lp
@@:
	cmpi.b	#'\',d0
	beq	mdf_
	cmpi.b	#'.',d0
	beq	exit_mdf
*	bsr	mk_capital
mdf0:
	move.b	d0,(a1)+
	bra	mdf_lp
exit_mdf:
	move.b	#'.',(a1)+
	move.b	#'Z',(a1)+
	move.b	#'M',(a1)+
	move.b	#'D',(a1)+
	clr.b	(a1)
	movem.l	(sp)+,d0-d1/a0-a1
	rts

_read_err:			*read error
	lea	sr_filename(pc),a0
	bsr	prt_err_header
	lea	read_er_mes(pc),a0
	bra	__end
_work_small:			*work too small
	lea	work_er_mes(pc),a0
	bra	__end
_unknown_err:			*unknown error
	lea	unknown_mes(pc),a0
	bra	__end
_open_err:			*OPEN ERROR
	lea	sv_filename(pc),a0
	bsr	prt_err_header
	lea	open_er_mes(pc),a0
	bra	__end
_write_err:			*WRITE ERROR
	lea	sv_filename(pc),a0

	DOS	_ALLCLOSE
	pea	(a0)
	DOS	_DELETE
	addq.w	#4,sp

	bsr	prt_err_header
	lea	write_er_mes(pc),a0
	bra	__end
_out_mem_err:
	lea	out_mem_mes(pc),a0
	bra	__end
_out_trk_err:			*TRACK BUFFER IS TOO SMALL
	lea	out_trk_mes(pc),a0
	bra	__end

compile_error:			*コンパイルエラー
	bsr	prt_err_code	*エラーメッセージ表示
	bra	exit_compile
__end:
	bsr	prta0
exit_compile:
	move.l	num_of_err(pc),d0
	beq	exit_compile_
	bsr	num_to_str
	lea	suji(pc),a0
	bsr	prta0
	lea	how_many_err(pc),a0
	bsr	prta0
	bsr	play_beep	*警告音を鳴らす
	bra	bye_bye

prt_err_header:			*ERROR HEADER
	* < a0=file name
	lea	err_header(pc),a1
	bsr	prta1
	bra	prta0
