*-------------------------------------------------------
*
*		割り込み処理部分ソースリスト
*
*		 Programmed by Z.Nishikawa
*
*-------------------------------------------------------
		.text
num_of_80:	equ	32

m_out_d0:	m_out_	d0
		rts
m_out_d1:	m_out_	d1
		rts
m_out_d2:	m_out_	d2
		rts
m_out_d3:	m_out_	d3
		rts
m_out_d4:	m_out_	d4
		rts
m_out_d5:	m_out_	d5
		rts
m_out_d6:	m_out_	d6
		rts
m_out0:		m_out0
		rts
m_out1:		m_out_	#1
		rts
m_out7:		m_out_	#7
		rts
m_out10:	m_out_	#10
		rts
m_out64:	m_out_	#64
		rts
m_outa0:	m_out_	(a0)+
		rts
opmset:
	opmset	d1,d2
	rts

chk_opm_wait:
	tst.b	fm_data_port	*busy check
	bmi	chk_opm_wait
	rts

f7_wait:				*EOX後のウェイト
	move.w	d2,-(sp)
	move.w	eox_w(pc),d2
	subq.w	#1,d2
	bcs	exit_ew
@@:
	bsr	v_wait
	dbra	d2,@b
exit_ew:
	move.w	(sp)+,d2
	rts

v_wait:
@@:
	btst.b	#4,$e88001
	beq	@b
@@:
	btst.b	#4,$e88001
	bne	@b
	rts

h_wait:				*単なるウェイト
	* < d0.w=loop counter	*d0*(1/60/512)秒のウェイト
hsy_lp01:
	tst.b	$e88001
	bpl	hsy_lp01
hsy_lp02:
	tst.b	$e88001
	bmi	hsy_lp02
	dbra	d0,hsy_lp01
	rts

	if	mpu=30		*XVI24MHz/X68030対応用サブルーチン

wait_24:
	tst.b	$e9a001		*ダミー
	rts

	endif

	if	type=4		*POLYPHON
polyphon_midi_out:
	movem.l	d0-d2,-(sp)
	moveq.l	#0,d1
	move.b	(a3),d2
@@:
	move.w	#$0303,d0
	trap	#2
	tst.l	d0
	bmi	@b
	movem.l	(sp)+,d0-d2
	rts

polyphon_midi_in:
	movem.l	d0-d1,-(sp)
	moveq.l	#0,d1
	move.w	#$0306,d0
	trap	#2
	tst.l	d0
	bmi	@f
	move.b	d0,(a3)
	moveq.l	#0,d0		*正しく受信された場合はplus(dummy)
@@:
	movem.l	(sp)+,d0-d1
	rts
	endif

	if	(type<>3.and.type<>4)	*MIDI I/F
rec_int:			*データを受信するとここへ来る
	movem.l	d0/a0-a2,-(sp)
	lea	rgr,a2
	move.b	#$03,(a2)
					midiwait
@@:
	tst.b	grp4-rgr(a2)
	bpl	@f
	move.b	grp6-rgr(a2),d0
					*midiwait
	cmpi.b	#$fe,d0
	beq	@f
	cmpi.b	#$f8,d0
	beq	@f
	lea	rec_data_end(pc),a1
	movea.l	(a1),a0
	move.b	d0,(a0)+
	move.l	a0,(a1)
	cmpa.l	adpcm_work_end(pc),a0
	bcs	@b
					*バッファ溢れを起こしたら警告音を鳴らす
	clr.b	(a2)
					midiwait
	clr.b	grp6-rgr(a2)		*int end
					midiwait
	bsr	play_beep
@@:
	move.b	#$20,icr-rgr(a2)	*int clr
					midiwait
	movem.l	(sp)+,d0/a0-a2
	rte

	elseif	type=3		*RS-MIDI
rec_int:			*データを受信するとここへ来る
	movem.l	d0/a0-a2,-(sp)
	lea	$e98005,a2
@@:
	btst.b	#0,(a2)
	beq	@f
	move.b	2(a2),d0
	cmpi.b	#$fe,d0
	beq	@f
	cmpi.b	#$f8,d0
	beq	@f
	lea	rec_data_end(pc),a1
	movea.l	(a1),a0
	move.b	d0,(a0)+
	move.l	a0,(a1)
	cmpa.l	adpcm_work_end(pc),a0
	bcs	@b
				*バッファ溢れを起こしたら警告音を鳴らす
	bsr	init_midibd	*int end (記録強制終了)
	bsr	play_beep
@@:
	move.b	#$38,(a2)	*割り込みリセット
	movem.l	(sp)+,d0/a0-a2
	rte
	endif

int_entry:			*割り込み処理のエントリー
	* 汎用レジスタ群       d0 d1 d2 d3 d4 d5 d6/ a1 a2
	* ﾌｧﾝｸｼｮﾝ汎用レジスタ  d3=tie flg,d4=ch,d5=find same note
	* グローバルレジスタ群                    a0 a3 a4 a5 a6
	* ↓ entry sr=$26**
				opmwait
	move.b	#$14,fm_addr_port
				opmwait
reset_tm:			*ここは-aスイッチで書き変わる
	move.b	#%0011_0101,fm_data_port
reset_int_e:
	movem.l	d0-d6/a0-a6,-(sp)
sr_restore:
	andi.b	#$f7,$00e88015	*MFP FM_int off
	move.w	$0038(sp),d0
	ori.w	#$2000,d0
	move.w	d0,sr			*SRを割り込み発生前に戻す
sr_restore_e:
	lea	play_trk_tbl(pc),a6
	clr.l	fo_flg-play_trk_tbl(a6)
	addq.l	#1,zmusic_int-play_trk_tbl(a6)
mb_adr:				*変更したらset_patchも変える
	set_a3a4
m_int_lp:
	move.b	(a6)+,d0
	bmi	exit_int	*$ffなら未使用
	lsl.w	#wk_size2,d0
	movea.l	seq_wk_tbl(pc),a5
	adda.w	d0,a5		*a5=trk n seq_wk_tbl
	move.b	p_fo_mode(a5),d5	*FADE OUT/IN MODEか？
	bne	fo_ope			*yes
go_ipo:				*フェードイン／アウト処理からの帰還先
	tst.b	p_not_empty(a5)
	bne	@f		*case:track is dead
	bsr	int_play_ope	*実際の演奏処理へ
@@:
	tst.b	p_se_mode(a5)	*効果音モードかどうかチェック
	bmi	m_int_lp
se_ope:				*効果音モード処理
	nop			*パッチが当たることがある(func$19,$44)
	move.b	-1(a6),d0	*現在実行中のトラックナンバー(se mode)
	lsl.w	#wk_size2,d0
	movea.l	seq_wk_tbl2(pc),a5
	adda.w	d0,a5		*a5=trk n seq_wk_tbl(se専用の)
	tst.b	p_not_empty(a5)
	bne	semd_off
	bsr	int_play_ope	*SE演奏
	bra	m_int_lp
exit_int:
	movem.l	(sp)+,d0-d6/a0-a6
int_rte:			*ここはファンクション$19や[@]処理で書き変わる
	ori.w	#$0700,sr	*全マスク(ZMUSICへの二重割り込み防止)
	ori.b	#$08,$00e88015	*MFP FM_int ON
	rte			*割り込み処理終了

semd_off:			*ＳＥモード終了
	move.b	p_ne_buff(a5),d4
	movea.l	seq_wk_tbl(pc),a5
	adda.w	d0,a5		*a5=trk n seq_wk_tbl
	st.b	p_se_mode(a5)	*se mode off
	move.b	d4,p_not_empty(a5)
	bne	m_int_lp
	move.b	p_ch(a5),d4
	cmpi.b	#7,d4
	bhi	m_int_lp	*case:MIDI&ADPCM(なにもしない)
				*case:FM
	bne	@f
	moveq.l	#15,d1		*noise mode reset
	move.b	noise_mode(pc),d2
	bsr	opmset
@@:
	moveq.l	#8,d1
	move.b	d4,d2
	bsr	opmset		*=fmkey_off	d4
	moveq.l	#0,d0
	move.b	p_pgm(a5),d0	*set last pgm
	bmi	m_int_lp	*音色未定義のケース
	bsr	pan_save_fmvset	*d0=pgm number
	bra	m_int_lp

fo_ope:				*FADE OUT/IN OPERATION
	* < d5.b=p_fo_mode(a5) (-1 or +1)
	move.b	p_not_empty(a5),d0
	bmi	m_int_lp	*死んでいるならばなにもしない
	subq.b	#1,d0		*演奏終了しているならばなにもしない(case:1)
	beq	m_int_lp
	*それ以外(ex.=$7f(同期待ち))はたとえトラックが死んでいてもフェードアウト処理を行う
	tst.b	p_marker(a5)
	bmi	go_ipo		*se trackはフェードアウト無し
	cmpi.b	#$47,d5		*func $47の特別処理か
	bne	@f
	moveq.l	#0,d5		*func $47ならフェード方向±0(dummy)
@@:
	move.b	p_ch(a5),d4
	move.b	p_fo_spd(a5),d0
	beq	go_ipo		*case:func $47
	bpl	@f
	clr.b	p_fo_spd(a5)	*case:func $47
	bra	fo_0		*最初の１回だけボリューム変更
@@:
	cmpi.b	#8,d4
	beq	@f
	bcs	fo_en?
	add.b	d0,d0		*MIDIは２倍の速度で
	bra	fo_en?
@@:
	add.b	d0,d0		*ADPCMは
	tst.b	d5
	bpl	fo_en?
	add.b	p_fo_spd(a5),d0	*fade outの場合は３倍の速度で
fo_en?:
	add.b	d0,1+p_marker(a5)
	bcc	go_ipo
fo_0:				*フェードイン／アウトメイン
	move.l	d4,d1
	lea	fo_or_not(pc),a2
	move.l	fo_flg(pc),d0		*一度設定したチャンネルについてはノンタッチ
	cmpi.b	#8,d1
	bne	fo_1
npc8_4:					*npc8!
	tst.b	p_extra_ch(a5)
	beq	fo_1			*PCM8でもCH=0の時は...
	move.b	p_extra_ch(a5),d1	*PCM8の時は
	add.b	#24,d1			*25ビット以降を使用
fo_1:
	bset.l	d1,d0
	sne	(a2)
	move.l	d0,fo_flg-fo_or_not(a2)
					*fade outを考慮したボリューム値を決める
	moveq.l	#127,d1
	sub.b	p_vol(a5),d1
	moveq.l	#0,d0
	move.b	p_fo_lvl(a5),d0
	mulu	d0,d1
	lsr.w	#7,d1			*d1=0～127

	cmpi.b	#8,d4
	beq	fo_adpcm
	bhi	fo_midi
				*FM
	moveq.l	#127,d0
	sub.b	d1,d0		*127-0
	tst.b	(a2)
	bne	fmfo0
				*主チャンネルは絶対処理する
	bsr	do_volume	*(< d0.b=volume/d4=fm ch)
	lea	p_note+1(a5),a1
@@:
	addq.b	#1,d4
	cmpi.b	#7,d4
	bhi	fmfo0
	cmpi.b	#$ff,(a1)+
	beq	fmfo0
	bsr	do_volume	*(< d0.b=volume/d4=fm ch)
	bra	@b
fmfo0:
	add.b	d5,p_fo_lvl(a5)	*calc %
	tst.b	d5
	beq	case_47_rel	*func $47の解除
	bpl	fi_end?
	cmpi.b	#80,d0
	bls	go_ipo
	bra	fo_end
fi_end?:
	cmpi.b	#128,p_fo_lvl(a5)
	bls	go_ipo
	bra	fi_end		*128以上になったらおわり
case_47_rel:
	cmpi.b	#128,p_fo_lvl(a5)
	beq	fi_end
	bra	go_ipo

fio47:				*func $47 ADPCMのPCM8モードでないケース
	cmpi.b	#63,p_fo_lvl(a5)
	bls	go_ipo
	bra	fi_end

fo_adpcm:			*ADPCM
	* < d1.b=0-127
pc8_0:				*pc8!
@@:
	add.b	d5,p_fo_lvl(a5)
	tst.b	d5
	beq	fio47
	bpl	fi_end?
	cmpi.b	#91,p_fo_lvl(a5)
	bhi	go_ipo
	bra	fo_end
fo_pcm8:			*PCM8の場合はADPCM音もフェードアウト
	cmpi.w	#RTS,PCM8KOFF-fo_or_not(a2)
	beq	fo_poly8	*-Oスイッチモード時のフェードアウト
	tst.b	(a2)
	bne	fomdchk		*やることは同じだからMIDI用ルーチンを使用
	moveq.l	#$70,d0
	or.b	p_extra_ch(a5),d0
	lsr.b	#3,d1		*0～f
	swap	d1
	move.w	#-1,d1		*pan/frq保存
	trap	#2
	bra	fomdchk
fo_poly8:
	move.w	poly_ch(pc),d4
	lsr.b	#3,d1		*0～f
	swap	d1
	move.w	#-1,d1		*pan/frq保存
@@:
	moveq.l	#$70,d0
	add.b	d4,d0
	trap	#2
	dbra	d4,@b
	bra	fomdchk

fo_midi:			*MIDI
	* < d1.b=0-127
	tst.b	(a2)
	bne	fomdchk
	add.b	#$b0-9,d4
	bsr	m_out_d4		*ctrl chg
	bsr	m_out7			*volume cmd
	bsr	m_out_d1		*vol value
fomdchk:
	add.b	d5,p_fo_lvl(a5)
	tst.b	d5
	beq	case_47_rel		*func $47の解除
	bpl	fi_end?
	tst.b	p_fo_lvl(a5)
	bne	go_ipo

fi_end:
fo_end:					*FO終了処理
	clr.b	p_fo_mode(a5)
	tst.b	d5
	bpl	@f
	bsr	play_end
@@:
	lea.l	play_trk_tbl(pc),a1
fe_lp:					*同じチャンネルにアサインされているものも殺す
	move.b	(a1)+,d0
	bmi	go_ipo
	lsl.w	#wk_size2,d0
	movea.l	seq_wk_tbl(pc),a2
	adda.w	d0,a2
	move.b	p_not_empty(a2),d0	*しんでいるならなにもしない
	bmi	fe_lp
	subq.b	#1,d0			*終わっているなら何もしない
	beq	fe_lp
	move.b	p_ch(a5),d0
	cmp.b	p_ch(a2),d0
	bne	fe_lp
	cmpi.b	#8,d0
	bne	fie0
pcm8_fie:				*npc8!
	move.b	p_extra_ch(a5),d0
	cmp.b	p_extra_ch(a2),d0
	bne	fe_lp
fie0:
	clr.b	p_fo_mode(a2)
	tst.b	d5
	bpl	fe_lp
	exg.l	a5,a2
	movem.l	a1-a2,-(sp)
	bsr	play_end
	movem.l	(sp)+,a1-a2
	exg.l	a5,a2
	bra	fe_lp

chk_vol_big:			*fade outを考慮したボリューム値を決める
	* < d0.b=source vol(127-0)
	* > d0.l=true volume(127-0)
	* X d1,d0
	tst.b	p_fo_mode(a5)	*FADE OUT MODEか？
	beq	@f		*no
	moveq.l	#127,d1
	sub.b	d0,d1
	moveq.l	#0,d0
	move.b	p_fo_lvl(a5),d0
	mulu	d0,d1
	lsr.w	#7,d1
	moveq.l	#127,d0
	sub.b	d1,d0
@@:
	rts

port_ope:				*ポルタメント処理
	* < d5.w=initialized kf
	* > d5.w=calculated kf value
	* X d0-d1
	move.w	p_port_work2(a5),d1
	add.w	p_port_step(a5),d1
	move.b	p_port_rvs(a5),d0
	add.b	d0,p_port_work(a5)	*補正値の番か?
	bcc	@f
	add.w	p_port_flg(a5),d1		*carry set=補正パラメータが有効
@@:
	move.w	d1,p_port_work2(a5)
	add.w	d1,d5
	bset.l	#31,d5			*mark
	rts

bend_ope:				*オートベンド処理
	* < d5.w=initialized kf
	* > d5.w=calculated kf value
	* X d0-d1
	move.w	p_port_work2(a5),d1
	add.w	p_port_step(a5),d1
	move.b	p_port_rvs(a5),d0
	add.b	d0,p_port_work(a5)	*補正値の番か?
	bcc	@f
	add.w	p_bend_flg(a5),d1		*carry set=補正パラメータが有効
@@:
	move.w	d1,p_port_work2(a5)
	add.w	d1,d5
	bset.l	#31,d5			*mark
	rts

pmod_ope:				*ピッチモジュレーション処理
	* < d5.w=port/bend calculated kf value
	* > d5.w=calculated kf value
	* X d0-d3,a1
	move.w	p_pmod_work5(a5),d0	*1/8モードか?
	beq	fmpmod1			*no
	move.b	p_pmod_chain(a5),d3	*波形接続状態
	bmi	pmod_chain
	subq.w	#1,p_pmod_work6(a5)	*dec 1/8 work
	bne	fmpmod1
	bra	@f
pmod_chain:				*波形の繋ぎ目を滑らかにする
	subq.w	#1,p_pmod_work6(a5)	*dec 1/8 work
	bne	pmc0
@@:
	move.w	d0,p_pmod_work6(a5)	*set new 1/8
	addq.b	#1,p_pmod_n(a5)
pmc0:
	moveq.l	#0,d0
	move.b	p_pmod_n(a5),d0
	cmpi.b	#7,d0
	bhi	pmod8_off
	btst.b	d0,p_pmod_omt(a5)	*省略の場合は前回のものを継続
	beq	fmpmod1
	add.w	d0,d0
	move.w	p_pmod_tbl(a5,d0.w),d2
	moveq.l	#0,d1
	move.b	p_pmod_wf(a5),d1	*波形タイプ検査
	add.w	d1,d1
	move.w	smtp(pc,d1.w),d1
	jmp	smtp(pc,d1.w)
smtp:	dc.w	smt_swp-smtp		*case:saw
	dc.w	smt_sqp-smtp		*case:squ
	dc.w	smt_trp-smtp		*case:trp
	dc.w	smt_swp2-smtp		*case:sp_saw
smt_trp:				*三角波の接続チェック
	subq.b	#1,d3
	beq	@f			*初めから省略の場合は波形接続なし
	tst.w	p_pmod_work2(a5)	*中点でないなら
	bne	pm_chain
	move.w	p_pmod_flg(a5),d1
	eor.w	d2,d1			*方向が逆だと見送る
	bmi	pm_chain
@@:
	move.w	p_pmod_spd(a5),d1
	lsr.w	d1
	move.w	d1,p_pmod_work4(a5)
smtp0:
	move.w	p_mstep_tbl(a5,d0.w),p_pmod_step2(a5)
	lsr.w	#1,d0
	move.b	p_mrvs_tbl(a5,d0.w),p_pmod_rvs(a5)
	moveq.l	#0,d0
	moveq.l	#1,d1
	move.b	d0,p_pmod_work3(a5)
	tst.w	d2			*振幅0のケースは
	beq	@f
	bpl	smtp1
	move.w	#-1,p_pmod_flg(a5)
	move.b	d0,p_pmod_chain(a5)
	bra	fmpmod1
@@:
	move.b	d1,p_pmod_chain(a5)
	bra	fmpmod1
smtp1:
	move.w	d1,p_pmod_flg(a5)
	move.b	d0,p_pmod_chain(a5)
	bra	fmpmod1
smt_sqp:				*矩形波の接続チェック
	move.w	p_pmod_spd(a5),d1
	subq.b	#1,d3
	beq	@f			*初めから省略の場合は波形接続なし
	cmp.w	p_pmod_work4(a5),d1
	bne	pm_chain		*波長がいまいち
	move.w	p_pmod_work2(a5),d3
	eor.w	d2,d3			*矩形波は±が交互にならなければ見送り
	bmi	pm_chain
@@:
	move.w	d1,p_pmod_work4(a5)
	moveq.l	#0,d1
	move.w	d2,p_pmod_work2(a5)	*振幅0のケースは
	bne	@f
	moveq.l	#1,d1
@@:
	move.b	d1,p_pmod_chain(a5)
	bra	fmpmod1
smt_swp:				*ノコギリ波の接続チェック
	subq.b	#1,d3
	beq	@f			*初めから省略の場合は波形接続なし
	tst.w	p_pmod_work2(a5)	*ノコギリ波ならばピッチが中心にあればすぐ接続
	bne	pm_chain
@@:
	move.w	p_pmod_spd(a5),p_pmod_work4(a5)
	bra	smtp0
smt_swp2:				*ノコギリ波2の接続チェック
	subq.b	#1,d3
	beq	@b			*初めから省略の場合は波形接続なし
	tst.w	p_pmod_work4(a5)	*１回やり終えてから
	beq	@b
pm_chain:				*今回は接続不能,次回へ期待
	st.b	p_pmod_chain(a5)
	bra	fmpmod1
pmod8_off:				*最後まで来たなら1/8モードオフ
	clr.w	p_pmod_work5(a5)	*p_pmod_work5=0
fmpmod1:
	bset.l	#31,d5			*mark
*-------実際のモジュレーション処理-------
	move.b	p_pmod_wf(a5),d1	*波形タイプチェック
	bmi	@f
	ext.w	d1
	add.w	d1,d1
	move.w	fmp1(pc,d1.w),d1
	jmp	fmp1(pc,d1.w)
fmp1:	dc.w	pmod_noko-fmp1
	dc.w	pmod_kukei-fmp1
	dc.w	pmod_sankaku-fmp1
	dc.w	pmod_noko2-fmp1
@@:					*波形メモリ
pmod_wvmm:
	* d0 破壊禁止
	tst.b	p_altp_flg(a5)
	bne	alternative_ope
	move.l	p_wvpm_point(a5),a1
	MOVEW	(a1)+,d1
	tst.b	p_pmod_sw(a5)
	bpl	@f
	neg.w	d1
@@:
	bsr	reduce_@mv
	move.w	d1,p_pmod_work2(a5)
	add.w	d1,d5
	subq.w	#1,p_pmod_work4(a5)
	bne	exit_pmod_ope
	move.w	p_pmod_spd(a5),d1
	lsr.w	#1,d1			*d1=d1/2
	move.w	d1,p_pmod_work4(a5)	*init speed work
	cmp.l	p_wvpm_end(a5),a1
	bne	svwvpm
	tst.w	p_wvpm_lpmd(a5)		*-1 one shot,0 repeat,+1 alternative
	bmi	oneshot_pm
	beq	repeat_pm
alternative_pm:
	st.b	p_altp_flg(a5)
oneshot_pm:
	subq.w	#2,a1
svwvpm:
	move.l	a1,p_wvpm_point(a5)
	rts
repeat_pm:
	move.l	p_wvpm_loop(a5),p_wvpm_point(a5)
	rts
alternative_ope:
	move.l	p_wvpm_point(a5),a1
	move.b	-(a1),d1
	ror.w	#8,d1
	move.b	-(a1),d1
	ror.w	#8,d1
	tst.b	p_pmod_sw(a5)
	bpl	@f
	neg.w	d1
@@:
	bsr	reduce_@mv
	move.w	d1,p_pmod_work2(a5)
	add.w	d1,d5
	subq.w	#1,p_pmod_work4(a5)
	bne	exit_pmod_ope
	move.w	p_pmod_spd(a5),d1
	lsr.w	#1,d1			*d1=d1/2
	move.w	d1,p_pmod_work4(a5)	*init speed work
	cmp.l	p_wvpm_loop(a5),a1
	bne	svwvpm
	clr.b	p_altp_flg(a5)
	addq.w	#2,a1
	bra	svwvpm
pmod_noko:				*鋸歯波
	move.w	p_pmod_work2(a5),d1
	add.w	p_pmod_step2(a5),d1
	move.b	p_pmod_rvs(a5),d3
	add.b	d3,p_pmod_work3(a5)
	bcc	@f
	add.w	p_pmod_flg(a5),d1
@@:
	move.w	d1,p_pmod_work2(a5)
	add.w	d1,d5
	subq.w	#1,p_pmod_work4(a5)
	bne	exit_pmod_ope
	neg.w	p_pmod_work2(a5)
	move.w	p_pmod_spd(a5),d1
	add.w	d1,d1
	move.w	d1,p_pmod_work4(a5)
	clr.b	p_pmod_work3(a5)
	rts
pmod_noko2:				*鋸歯波2
	move.w	p_pmod_work2(a5),d1
	tst.w	p_pmod_work4(a5)
	beq	pnp2_
	add.w	p_pmod_step2(a5),d1
	move.b	p_pmod_rvs(a5),d3
	add.b	d3,p_pmod_work3(a5)
	bcc	@f
	add.w	p_pmod_flg(a5),d1
@@:
	move.w	d1,p_pmod_work2(a5)
	add.w	d1,d5
	subq.w	#1,p_pmod_work4(a5)
	rts
pnp2_:
	add.w	d1,d5
	rts
pmod_kukei:				*矩形波
	add.w	p_pmod_work2(a5),d5
	subq.w	#1,p_pmod_work4(a5)
	bne	exit_pmod_ope
	neg.w	p_pmod_work2(a5)
	move.w	p_pmod_spd(a5),p_pmod_work4(a5)
	rts
pmod_sankaku:				*三角波
	move.w	p_pmod_work2(a5),d1
	add.w	p_pmod_step2(a5),d1
	move.b	p_pmod_rvs(a5),d3
	add.b	d3,p_pmod_work3(a5)
	bcc	@f
	add.w	p_pmod_flg(a5),d1
@@:
	move.w	d1,p_pmod_work2(a5)
	add.w	d1,d5
	subq.w	#1,p_pmod_work4(a5)
	bne	exit_pmod_ope
	move.w	p_pmod_spd(a5),p_pmod_work4(a5)
	neg.w	p_pmod_step2(a5)
	neg.w	p_pmod_flg(a5)
	clr.b	p_pmod_work3(a5)
exit_pmod_ope:
	rts

amod_ope:				*ＡＭ処理
	* < d6.b=now volume
	* X d0-d3,a1
	move.w	p_amod_work5(a5),d0	*1/8モードか?
	beq	fmamod1			*no
	move.b	p_amod_chain(a5),d3
	bmi	amod_chain
	subq.w	#1,p_amod_work6(a5)	*dec 1/8 work
	bne	fmamod1
	bra	@f
amod_chain:				*波形の繋ぎ目を滑らかにする
	subq.w	#1,p_amod_work6(a5)	*dec 1/8 work
	bne	amc0
@@:
	move.w	d0,p_amod_work6(a5)	*set new 1/8
	addq.b	#1,p_amod_n(a5)
amc0:
	moveq.l	#0,d0
	move.b	p_amod_n(a5),d0
	cmpi.b	#7,d0
	bhi	amod8_off
	btst.b	d0,p_amod_omt(a5)	*省略の場合は前回のものを継続
	beq	fmamod1
	move.b	p_amod_tbl(a5,d0.w),d2
	moveq.l	#0,d1
	move.b	p_amod_wf(a5),d1
	add.w	d1,d1
	move.w	smta(pc,d1.w),d1
	jmp	smta(pc,d1.w)
smta:	dc.w	smt_swa-smta
	dc.w	smt_sqa-smta
	dc.w	smt_tra-smta
	dc.w	smt_swa2-smta
smt_tra:				*三角波の接続チェック
	move.w	p_amod_spd(a5),d1
	subq.b	#1,d3
	beq	@f
	cmp.w	p_amod_work4(a5),d1
	bne	am_chain		*波長がいまいち
	move.b	p_amod_work2(a5),d3
	eor.b	d2,d3
	bmi	am_chain
@@:
	move.w	d1,p_amod_work4(a5)
smta0:
	move.b	p_astep_tbl(a5,d0.w),p_amod_step2(a5)
	move.b	p_arvs_tbl(a5,d0.w),p_amod_rvs(a5)
smta00:
	moveq.l	#0,d0
	moveq.l	#1,d1
	tst.b	d2
	beq	@f
	bpl	smta1
	move.b	d2,p_amod_work2(a5)
	move.b	d0,p_amod_work3(a5)	*init p_amod_work3
	move.b	d2,p_amod_work7(a5)
	st.b	p_amod_flg(a5)
	move.b	d0,p_amod_chain(a5)
	bra	fmamod1
@@:
	move.b	d1,p_amod_chain(a5)
	bra	fmamod1
smta1:
	move.w	d0,p_amod_work2(a5)	*init p_amod_work2,3
	move.b	d0,p_amod_work7(a5)	*init p_amod_work7
	move.b	d1,p_amod_flg(a5)
	move.b	d0,p_amod_chain(a5)
	bra	fmamod1
smt_sqa:				*矩形波の繋ぎを滑らかに
	move.w	p_amod_spd(a5),d1
	subq.b	#1,d3			*初めから省略の場合は波形接続なし
	beq	@f
	cmp.w	p_amod_work4(a5),d1
	bne	am_chain		*波長がいまいち
	move.b	p_amod_work2(a5),d3
	eor.b	d2,d3
	bmi	am_chain
@@:
	move.w	d1,p_amod_work4(a5)
	move.b	d2,p_amod_step2(a5)
	bra	smta00
smt_swa:				*ノコギリ波
	move.w	p_amod_spd(a5),d1
	add.w	d1,d1
	subq.b	#1,d3
	beq	@f
	cmp.w	p_amod_work4(a5),d1
	bne	am_chain
@@:
	move.w	d1,p_amod_work4(a5)
	bra	smta0
smt_swa2:				*ノコギリ波2
	subq.b	#1,d3
	beq	@f
	tst.w	p_amod_work4(a5)
	bne	am_chain
@@:
	move.w	p_amod_spd(a5),d1
	add.w	d1,d1
	move.w	d1,p_amod_work4(a5)
	bra	smta0
am_chain:					*今回は接続不能,次回に期待。
	st.b	p_amod_chain(a5)
	bra	fmamod1
amod8_off:					*最後まで来たなら1/8モードオフ
	clr.w	p_amod_work5(a5)
fmamod1:
	bset.l	#31,d6			*mark and check
*-------実際のモジュレーション処理-------
	move.b	p_amod_wf(a5),d1
	bmi	@f
	ext.w	d1
	add.w	d1,d1
	move.w	fma1(pc,d1.w),d1
	jmp	fma1(pc,d1.w)
fma1:	dc.w	amod_noko-fma1
	dc.w	amod_kukei-fma1
	dc.w	amod_sankaku-fma1
	dc.w	amod_noko2-fma1
@@:					*波形メモリ
amod_wvmm:
	* d0 破壊禁止
	tst.b	p_alta_flg(a5)
	bne	alternative_ope_a
	move.l	p_wvam_point(a5),a1
	MOVEW	(a1)+,d6
	tst.b	p_amod_sw(a5)
	bpl	@f
	neg.w	d6
@@:
	bsr	reduce_@av
	move.b	d6,p_amod_work2(a5)
	subq.w	#1,p_amod_work4(a5)
	bne	exit_amod_ope
	move.w	p_amod_spd(a5),d1
	lsr.w	#1,d1			*d1=d1/2
	move.w	d1,p_amod_work4(a5)	*init speed work
	cmp.l	p_wvam_end(a5),a1
	bne	svwvam
	tst.w	p_wvam_lpmd(a5)
	bmi	oneshot_am
	beq	repeat_am
alternative_am:
	st.b	p_alta_flg(a5)
oneshot_am:
	subq.w	#2,a1
svwvam:
	move.l	a1,p_wvam_point(a5)
	rts
repeat_am:
	move.l	p_wvam_loop(a5),p_wvam_point(a5)
	rts
alternative_ope_a:
	move.l	p_wvam_point(a5),a1
	move.b	-(a1),d6
	ror.w	#8,d6
	move.b	-(a1),d6
	ror.w	#8,d6
	tst.b	p_amod_sw(a5)
	bpl	@f
	neg.w	d6
@@:
	bsr	reduce_@av
	move.b	d6,p_amod_work2(a5)
	subq.w	#1,p_amod_work4(a5)
	bne	exit_amod_ope
	move.w	p_amod_spd(a5),d1
	lsr.w	#1,d1			*d1=d1/2
	move.w	d1,p_amod_work4(a5)	*init speed work
	cmp.l	p_wvam_loop(a5),a1
	bne	svwvam
	clr.b	p_alta_flg(a5)
	addq.w	#2,a1
	bra	svwvam
amod_noko:
	move.b	p_amod_work2(a5),d6
	sub.b	p_amod_step2(a5),d6
	move.b	p_amod_rvs(a5),d3
	add.b	d3,p_amod_work3(a5)
	bcc	@f
	sub.b	p_amod_flg(a5),d6
@@:
	move.b	d6,p_amod_work2(a5)
	subq.w	#1,p_amod_work4(a5)
	bne	exit_amod_ope
	move.w	p_amod_spd(a5),d1
	add.w	d1,d1
	move.w	d1,p_amod_work4(a5)
	move.b	p_amod_work7(a5),p_amod_work2(a5)
	clr.b	p_amod_work3(a5)
	rts
amod_noko2:
	move.b	p_amod_work2(a5),d6
	tst.w	p_amod_work4(a5)
	beq	exit_amod_ope
	sub.b	p_amod_step2(a5),d6
	move.b	p_amod_rvs(a5),d3
	add.b	d3,p_amod_work3(a5)
	bcc	@f
	sub.b	p_amod_flg(a5),d6
@@:
	move.b	d6,p_amod_work2(a5)
	subq.w	#1,p_amod_work4(a5)
	rts
amod_kukei:				*矩形波
	move.b	p_amod_work2(a5),d6
	subq.w	#1,p_amod_work4(a5)
	bne	exit_amod_ope
	move.b	p_amod_step2(a5),d1
	sub.b	d1,p_amod_work2(a5)
	neg.b	p_amod_step2(a5)
	move.w	p_amod_spd(a5),p_amod_work4(a5)
	rts
amod_sankaku:				*三角波
	move.b	p_amod_work2(a5),d6
	sub.b	p_amod_step2(a5),d6
	move.b	p_amod_rvs(a5),d3
	add.b	d3,p_amod_work3(a5)
	bcc	@f
	sub.b	p_amod_flg(a5),d6
@@:
	move.b	d6,p_amod_work2(a5)
	subq.w	#1,p_amod_work4(a5)
	bne	exit_amod_ope
	move.w	p_amod_spd(a5),p_amod_work4(a5)
	neg.b	p_amod_step2(a5)
	neg.b	p_amod_flg(a5)
	clr.b	p_amod_work3(a5)
exit_amod_ope:
	rts

reduce_@av:
	* < d6.w=-32768～+32767
	* > d6.w=-128～+127
	cmpi.w	#-128,d6
	bge	@f
	move.w	#-128,d6
	bra	exit_r@a
@@:
	cmpi.w	#127,d6
	ble	exit_r@a
	move.w	#127,d6
exit_r@a:
	rts

dec_step	macro
	subq.w	#1,(a5)		*p_on_count
	beq	run_cmd
	rts
	endm
dec_step:
	dec_step

int_play_ope:
dec_gate:			*func$19で書き変わる
	tst.w	p_gate_time(a5)	*これがないと何時でもタイになっちゃう
	beq	sp_cmd_ope
	subq.w	#1,p_gate_time(a5)
	bne	sp_cmd_ope
	bsr	key_off

sp_cmd_ope:			*特殊コマンド処理
	moveq.l	#0,d5		*d5.w=detune paraeter
				*d5.l bit31 = 1でset_fm_kc2へコール
				*各ルーチンはd4は壊さない(壊したら戻す)
	moveq.l	#0,d6		*d6.l=amod/arcc parameter
	move.b	p_ch(a5),d4
	cmpi.b	#8,d4
	bhi	int_ope_midi	*MIDIのケース
	beq	dec_step	*ADPCMのケース
				*特殊コマンド処理(FMのケース)
	move.b	p_vol(a5),d6
	swap	d6		*d6.hw=volume parameter
				*d6.l bit31 = 1でVOLUME操作
	tst.w	p_port_flg(a5)
	beq	int_ope0
	tst.w	p_port_dly(a5)
	beq	do_port0
	subq.w	#1,p_port_dly(a5)
	bra	int_ope0
do_port0:
	bsr	port_ope		*ポルタメントパラメータ処理
	bra	int_ope2		*ポルタメントの時は和音とベンドはあり得ないから…
int_ope0:				*和音処理
	tst.b	p_waon_flg(a5)
	beq	int_ope1
	move.b	p_waon_num(a5),d1	*何番目のノートをオンにする
	bmi	int_ope1		*全てノートオン済み
	subq.b	#1,p_waon_work(a5)
	bne	int_ope1
	move.b	p_waon_dly(a5),p_waon_work(a5)	*ワーク初期化
	ext.w	d1
	move.b	p_note(a5,d1.w),d3	*d3=note number
	bpl	wko_1
	st.b	p_waon_num(a5)
	bra	int_ope1		*終了
wko_1:
	add.b	d1,d4
	addq.b	#1,d1
	cmpi.b	#7,d1
	bls	sv_wn
	st.b	d1
sv_wn:
	move.b	d1,p_waon_num(a5)
	tst.b	p_se_mode(a5)		*効果音モード(or mask)ならキーオンしない
	bpl	int_ope1
	bset.b	d4,p_waon_mark(a5)
	bne	@f
	set_wn_p		*主チャンネルのパラメータに合わせる(X d0,d1)
@@:
	move.b	d3,d0			*d0=note number
	moveq.l	#0,d3			*not tie
	bsr	fm_kco			*FM KEY ON!
	move.b	p_ch(a5),d4		*壊したd4を戻す
int_ope1:				*オートベンド処理
	tst.w	p_bend_flg(a5)		*ポルタメント有効時は
	beq	int_ope2		*オートベンド停止中だからここに来てもOK
	tst.w	p_port_dly(a5)
	beq	do_port1
	subq.w	#1,p_port_dly(a5)
	bra	int_ope2
do_port1:
	bsr	bend_ope		*ベンドパラメータ処理
int_ope2:				*アフタータッチシーケンス処理
	tst.b	p_aftc_flg(a5)
	beq	int_ope3
	subq.w	#1,p_aftc_work(a5)
	bne	iaf0
	move.w	p_aftc_dly(a5),p_aftc_work(a5)
	move.b	p_aftc_n(a5),d1
	addq.b	#1,d1
	cmpi.b	#7,d1
	bhi	iaf0
	move.b	d1,p_aftc_n(a5)
	ext.w	d1
	move.b	p_aftc_tbl(a5,d1.w),d1
	bmi	iaf0
	move.b	d1,p_last_aft(a5)	*d0=volume value(127～0)
	bra	@f
iaf0:
	move.b	p_last_aft(a5),d1
@@:
	move.l	#$0000_c07f,d6
	sub.b	d1,d6			*d6=volume value(127～0)
	swap	d6
int_ope3:				*ピッチモジュレーション処理
	tst.w	p_pmod_flg(a5)
	beq	int_ope4
	tst.w	p_pmod_work(a5)
	beq	fmpmod0
	subq.w	#1,p_pmod_work(a5)
	bra	int_ope4
fmpmod0:
	bsr	pmod_ope
int_ope4:				*ＡＭ処理
	tst.b	p_amod_flg(a5)
	beq	int_ope5
	tst.w	p_amod_work(a5)
	beq	fmamod0
	subq.w	#1,p_amod_work(a5)	*dec delay work
	bra	int_ope5
fmamod0:
	bsr	amod_ope
int_ope5:
	* < d5.w=pitch
	* < d6.hw=volume value(127～0)
	* < d6.lw=amod rltv value
	* < d4.b$=fm ch
	tst.b	p_se_mode(a5)		*効果音モード(mask ok)
	beq	dec_step
	move.l	d6,d0
	bpl	chg_pitch?
	swap	d0			*d0.b=volume value(127～0)
	move.b	p_arcc(a5),d3
	beq	@f
	sub.b	d6,d0			*volume(127～0)
	bpl	@f
	tst.b	d6
	bpl	uf_@av
	moveq.l	#127,d0
	bra	@f
uf_@av:
	moveq.l	#0,d0
@@:
	bsr	chk_vol_big		*>d0=calculated volume
	btst.l	#30,d6
	beq	@f
	or.b	p_cf(a5),d3
	lea	case_aft_am(pc),a1
	bra	dao
@@:
	and.b	p_cf(a5),d3
	lea	do_amod_ope(pc),a1
dao:
	move.b	p_waon_flg(a5),d1
	or.b	p_dumper(a5),d1
	bne	waon_stvl
	jsr	(a1)
	tst.l	d5
	bpl	dec_step
	move.b	p_note(a5),d0
	bsr	set_fm_kc2
	dec_step
chg_pitch?:
	tst.l	d5
	bpl	dec_step
	move.b	p_waon_flg(a5),d1
	or.b	p_dumper(a5),d1
	bne	waon_stkc2
	move.b	p_note(a5),d0
	bsr	set_fm_kc2
	dec_step

waon_stvl:			*和音を考慮したアフタータッチやＡＭ処理
	* < d0.b=volume value(127～0)
	* < d6.b=amod rltv value
	lea	p_note(a5),a2
waon_stvl_lp:
	cmpi.b	#$ff,(a2)+
	beq	@f
	jsr	(a1)
	addq.b	#1,d4
	cmpi.b	#7,d4
	bls	waon_stvl_lp
@@:
	tst.l	d5
	bpl	dec_step
	move.b	p_ch(a5),d4
waon_stkc2:			*和音を考慮したｵｰﾄﾍﾞﾝﾄﾞやﾓｼﾞｭﾚｰｼｮﾝ関係の処理
	lea	p_note(a5),a2
waon_stkc2_lp:
	move.w	d5,d2		*save d5 into d2
	move.b	(a2)+,d0
	bsr	set_fm_kc2
	cmpi.b	#$ff,(a2)
	beq	dec_step
	addq.b	#1,d4
	cmpi.b	#7,d4
	bhi	dec_step
	move.w	d2,d5		*get back d5 from d2
	bra	waon_stkc2_lp

do_amod_ope:
	* < d0.b=volume value(127～0)
	* < d6.b=amod rltv value
	* < d4=fm ch
	* - d0
	* X d1,d2,d3
	moveq.l	#$60,d1
	or.b	d4,d1
	swap	d3
	move.b	p_arcc(a5),d3

	btst.l	#0+16,d3
	beq	wow1?
	bsr	dao_@v1
	bra	dv3_
wow1?:
	btst.l	#0,d3
	beq	dv3_
	bsr	dao_@m1
dv3_:
	addq.b	#8,d1
	btst.l	#2+16,d3
	beq	wow3?
	bsr	dao_@v3
	bra	dv2_
wow3?:
	btst.l	#2,d3
	beq	dv2_
	bsr	dao_@m3
dv2_:
	addq.b	#8,d1
	btst.l	#1+16,d3
	beq	wow2?
	bsr	dao_@v2
	bra	dv4_
wow2?:
	btst.l	#1,d3
	beq	dv4_
	bsr	dao_@m2
dv4_:
	addq.b	#8,d1
	btst.l	#3+16,d3
	bne	dao_@v4
	btst.l	#3,d3
	bne	dao_@m4
dv_end_:
	rts

case_aft_am:
	* < d0.b=volume value(127～0)
	* < d6.b=amod rltv value
	* < d4=fm ch
	* - d0
	* X d1,d2,d3
	moveq.l	#$60,d1
	or.b	d4,d1
	swap	d3
	move.b	p_cf(a5),d3

	btst.l	#0+16,d3
	beq	dv3__
	pea	dv3__(pc)
	btst.l	#0,d3
	bne	dao_@v1
	bra	dao_@m1
dv3__:
	addq.b	#8,d1
	btst.l	#2+16,d3
	beq	dv2__
	pea	dv2__(pc)
	btst.l	#2,d3
	bne	dao_@v3
	bra	dao_@m3
dv2__:
	addq.b	#8,d1
	btst.l	#1+16,d3
	beq	dv4__
	pea	dv4__(pc)
	btst.l	#1,d3
	bne	dao_@v2
	bra	dao_@m2
dv4__:
	addq.b	#8,d1
*	btst.l	#3+16,d3
*	beq	dv_end_
*	btst.l	#3,d3
*	bne	dao_@v4
*	bra	dao_@m4
	bra	dao_@v4

dao_@v1:
	move.b	p_ol1(a5),d2	*OP1
	add.b	d0,d2
	bpl	opmset
	moveq.l	#127,d2
	bra	opmset		*opmset	d1,d2
dao_@v2:
	move.b	p_ol2(a5),d2	*OP2
	add.b	d0,d2
	bpl	opmset
	moveq.l	#127,d2
	bra	opmset		*opmset	d1,d2
dao_@v3:
	move.b	p_ol3(a5),d2	*OP3
	add.b	d0,d2
	bpl	opmset
	moveq.l	#127,d2
	bra	opmset		*opmset	d1,d2
dao_@v4:
	move.b	p_ol4(a5),d2	*OP4
	add.b	d0,d2
	bpl	opmset
	moveq.l	#127,d2
	bra	opmset		*opmset	d1,d2
dao_@m1:
	move.b	p_ol1(a5),d2	*OP1
	sub.b	d6,d2
	bmi	d2_illegal
	bra	opmset
dao_@m2:
	move.b	p_ol2(a5),d2	*OP2
	sub.b	d6,d2
	bmi	d2_illegal
	bra	opmset
dao_@m3:
	move.b	p_ol3(a5),d2	*OP3
	sub.b	d6,d2
	bmi	d2_illegal
	bra	opmset
dao_@m4:
	move.b	p_ol4(a5),d2	*OP4
	sub.b	d6,d2
	bmi	d2_illegal
	bra	opmset
d2_illegal:			*規定外のケース
	bcs	@f
	moveq.l	#127,d2
	bra	opmset
@@:
	moveq.l	#0,d2
	bra	opmset

int_ope_midi:			*特殊コマンド処理(MIDIのケース)
	move.w	p_sp_tie(a5),d5
	sub.b	#9,d4		*d4=MIDI CH(0-f)
	tst.w	p_port_flg(a5)
	beq	int_ope0_midi	*次へ…
	tst.w	p_port_dly(a5)
	beq	do_port0_midi
	subq.w	#1,p_port_dly(a5)
	bra	int_ope0_midi
do_port0_midi:
	bsr	port_ope
	bra	int_ope2_midi
int_ope0_midi:
	tst.b	p_waon_flg(a5)		*和音処理(MIDI)
	beq	int_ope1_midi
	move.b	p_waon_num(a5),d1	*何番目のノートをオンにする
	bmi	int_ope1_midi		*全てノートオン済み
	subq.b	#1,p_waon_work(a5)
	bne	int_ope1_midi
	move.b	p_waon_dly(a5),p_waon_work(a5)	*ワーク初期化
	ext.w	d1
	move.b	p_note(a5,d1.w),d0	*get note number
	bpl	wko_1_midi
	st.b	p_waon_num(a5)
	bra	int_ope1_midi		*終了
wko_1_midi:
	addq.b	#1,d1
	cmpi.b	#7,d1
	bls	sv_wn_midi
	st.b	d1
sv_wn_midi:
	move.b	d1,p_waon_num(a5)
	tst.b	p_se_mode(a5)		*効果音モード(or mask)ならキーオンしない
	bpl	int_ope1_midi
	bsr	midi_note_on2
int_ope1_midi:
	tst.w	p_bend_flg(a5)
	beq	int_ope2_midi		*次へ…
	tst.w	p_port_dly(a5)
	beq	do_port1_midi
	subq.w	#1,p_port_dly(a5)
	bra	int_ope2_midi
do_port1_midi:
	bsr	bend_ope
int_ope2_midi:
	tst.b	p_aftc_flg(a5)
	beq	int_ope3_midi
	subq.w	#1,p_aftc_work(a5)
	bne	int_ope3_midi
	move.w	p_aftc_dly(a5),p_aftc_work(a5)
	move.b	p_aftc_n(a5),d1
	addq.b	#1,d1
	cmpi.b	#7,d1
	bne	@f
	clr.b	p_aftc_flg(a5)		*(次回はオフ)
@@:
	move.b	d1,p_aftc_n(a5)
	ext.w	d1
	move.b	p_aftc_tbl(a5,d1.w),d0
	bmi	int_ope3_midi
	tst.b	p_se_mode(a5)		*効果音モード(mask ok)
	beq	int_ope3_midi
	moveq.l	#$d0,d1
	or.b	d4,d1
	bsr	m_out_d1			*after touch cmd
	bsr	m_out_d0			*velocity
int_ope3_midi:
	tst.w	p_pmod_flg(a5)		*PITCH MODULATION OPERATION
	beq	int_ope4_midi
	tst.w	p_pmod_work(a5)		*check dly
	beq	pmod1
	subq.w	#1,p_pmod_work(a5)
	bra	int_ope4_midi
pmod1:
	tst.b	p_pmod_mode(a5)
	bmi	pmod2
	bsr	pmod_ope
	bra	int_ope4_midi
@@:
	move.w	d0,p_pmod_flg(a5)	*(次回はオフ)
	bra	pmod3
pmod2:
	moveq.l	#0,d0
	move.w	p_pmod_work5(a5),d1	*1/8 mode?
	beq	@b			*no
	subq.w	#1,p_pmod_work6(a5)
	bne	int_ope4_midi
	move.w	d1,p_pmod_work6(a5)	*new delay
	addq.b	#1,p_pmod_n(a5)
	move.b	p_pmod_n(a5),d0
	cmpi.b	#7,d0
	bls	@f
	clr.w	p_pmod_flg(a5)		*(次回はオフ)
	bra	int_ope4_midi
@@:
	btst.b	d0,p_pmod_omt(a5)
	beq	int_ope4_midi		*省略の場合は前回のものを継続
	add.w	d0,d0
pmod3:
	tst.b	p_se_mode(a5)		*効果音モード(mask ok)
	beq	int_ope4_midi
	moveq.l	#$b0,d1
	or.b	d4,d1
	bsr	m_out_d1
	bsr	m_out1			*pitch modulation cmd
	bset.b	#1,p_md_flg(a5)
	m_out_	<p_pmod_tbl+1(a5,d0.w)>
int_ope4_midi:
	tst.b	p_arcc_flg(a5)		*ASSIGNABLE REALTIME CONTROL CHANGE
	beq	int_ope5_midi
	tst.w	p_arcc_work(a5)		*check dly
	beq	arcc1
	subq.w	#1,p_arcc_work(a5)
	bra	int_ope5_midi
arcc1:
	tst.b	p_arcc_mode(a5)
	bmi	arcc2
	bsr	amod_ope
	bra	int_ope5_midi
@@:
	move.b	d0,p_arcc_flg(a5)	*(次回はオフ)
	bra	arcc3
arcc2:
	moveq.l	#0,d0
	move.w	p_arcc_work5(a5),d1	*1/8 mode?
	beq	@b			*no
	subq.w	#1,p_arcc_work6(a5)
	bne	int_ope5_midi
	move.w	d1,p_arcc_work6(a5)	*new delay
	addq.b	#1,p_arcc_n(a5)
	move.b	p_arcc_n(a5),d0
	cmpi.b	#7,d0
	bls	@f
	clr.b	p_arcc_flg(a5)		*(次回はオフ)
	bra	int_ope5_midi
@@:
	btst.b	d0,p_amod_omt(a5)
	beq	int_ope5_midi		*省略の場合は前回のものを継続
arcc3:
	tst.b	p_se_mode(a5)		*効果音モード(mask ok)
	beq	int_ope5_midi
	moveq.l	#$b0,d1
	or.b	d4,d1
	bsr	m_out_d1
*	bset.b	#2,p_md_flg(a5)
	move.b	p_arcc_tbl(a5,d0.w),d0	*send value
	move.b	p_arcc(a5),d1
	bsr	m_out_d1
	bsr	arcc_p_chk
	bsr	m_out_d0
int_ope5_midi:
	* < d4.b=midi ch(0-f)
	* X d0-d5,a1
	* - d4
	tst.b	p_se_mode(a5)		*効果音モード(mask ok)
	beq	dec_step
	tst.l	d5
	bpl	sp_arcc
	moveq.l	#$e0,d0
	or.b	d4,d0		*ctrl chg
	bsr	m_out_d0
	add.w	p_detune_m(a5),d5	*get det. param.
	add.w	#8192,d5
	bpl	@f
	moveq.l	#0,d5
	bra	wrt_pb
@@:
	cmpi.w	#16383,d5
	bls	wrt_pb
	move.w	#16383,d5
wrt_pb:
	move.w	d5,d0
	andi.b	#127,d5
	bsr	m_out_d5	*lower @b
	bset.b	#0,p_md_flg(a5)
	lsr.w	#7,d0
	bsr	m_out_d0	*higher @b
sp_arcc:
	tst.l	d6
	bpl	dec_step
	move.b	p_arcc_def(a5),d0
	add.b	d6,d0
	bpl	@f
	bcs	uf_arcc
	moveq.l	#127,d0
	bra	@f
uf_arcc:
	moveq.l	#0,d0
@@:
	cmp.b	p_d6_last(a5),d0
	beq	dec_step
	move.b	d0,p_d6_last(a5)
	moveq.l	#$b0,d1
	or.b	d4,d1
	bsr	m_out_d1
*	bset.b	#2,p_md_flg(a5)
	move.b	p_arcc(a5),d1
	bsr	m_out_d1		*send ARCC code
	bsr	arcc_p_chk
	bsr	m_out_d0		*send value
	dec_step

arcc_p_chk:
	* < d1.b arcc ctrl number
	cmpi.b	#7,d1
	beq	arcc_vol
	cmpi.b	#10,d1
	beq	arcc_pan
	rts
arcc_pan:
	move.b	d0,p_pan2(a5)
	cmpi.b	#31,d0		*便宜上４(3)段階パンにする
	bhi	@f
	move.b	#1,p_pan(a5)
	rts
@@:
	cmpi.b	#95,d0
	bls	@f
	move.b	#2,p_pan(a5)
	rts
@@:
	move.b	#3,p_pan(a5)
	rts
arcc_vol:
	* X d0,d1
	moveq.l	#127,d1
	sub.b	d0,d1
	move.b	d1,d0
	add.b	#9,d4
	bsr	set_other_ch_v
	sub.b	#9,d4
	bsr	chk_vol_big
	move.b	d0,d1
	moveq.l	#127,d0
	sub.b	d1,d0
	rts

set_other_ch_v:
	* < d0.b=parameter value
	* < d4.b=ch num
	*X d1 a1 a2
	tst.b	p_marker(a5)		*効果音トラックに関しては処理しない。
	bmi	socv_se
	lea	play_trk_tbl(pc),a1
@@:
	move.b	(a1)+,d1
	bmi	exit_socv
	lsl.w	#wk_size2,d1
	movea.l	seq_wk_tbl(pc),a2
	adda.w	d1,a2
	cmp.b	p_ch(a2),d4
	bne	@b
npc8_5:					*npc8!
	cmpi.b	#8,d4
	bne	stot1
	move.b	p_extra_ch(a2),d1
	cmp.b	p_extra_ch(a5),d1
	bne	@b
stot1:
	move.b	d0,p_vol(a2)
	bra	@b
socv_se:
	move.b	d0,p_vol(a5)
exit_socv:
	rts

reset_sp_cmd	macro		*キーオフ時の場合の初期化
	rts
	endm

reset_sp_cmd2	macro		*ステップタイムエンド時の場合の初期化
	local	rsc0
				*休符等の時に現在のピッチを保存する
	tst.l	p_port_flg(a5)
	beq	rsc0
	moveq.l	#0,d0
	move.w	d0,p_port_step(a5)
	move.w	d0,p_port_work(a5)
rsc0:
	endm

exit_kof	macro
	reset_sp_cmd		*and return
	endm

key_off:			*キーオフ処理
	* X d0-d2/a1
	tst.b	p_se_mode(a5)	*効果音モード(or mask)ならキーオフしない
	bpl	exit_kof
	move.b	p_ch(a5),d1
	cmpi.b	#8,d1
	beq	kof_adp		*ADPCM
	bhi	kof_midi
				*ＦＭ音源のキーオフ処理
	tst.b	p_dumper(a5)
	bne	exit_kof	*dumper on ならキーオフしない
	lea	p_note(a5),a1
	moveq.l	#max_note_on-1,d2
	sub.b	d1,d2		*d2=loop counter
kof_lp00:
	tas.b	(a1)+		*minusへ
	bmi	exit_kof	*もともと１だったなら終わり
	tst.b	p_non_off(a5)
	bne	@f
	opmset	#8,d1		*=fmkey_off	d1		*fm key off
	bset.b	#6,p_seq_flag(a5)	*set key off bit
@@:
	addq.b	#1,d1
	dbra	d2,kof_lp00
exit_kof:
	exit_kof

kof_midi:
	lea	p_note(a5),a1	*note memo
	move.b	(a1),d0
	bmi	exit_kof	*exit
	tst.b	p_non_off(a5)
	bne	@f
	add.b	#$80-9,d1
	bsr	m_out_d1		*cmd
	bsr	m_out_d0		*note number
	bset.b	#6,p_seq_flag(a5)	*set key off bit
	bsr	m_out0			*#0
@@:
	st.b	(a1)+		*reset work area
	moveq.l	#max_note_on-1-1,d2		*max note on
out_lp_kof0:
	move.b	(a1),d0
	bmi	exit_kof	*exit
	tst.b	p_non_off(a5)
	bne	@f
*	bsr	m_out_d1		*cmd
	bsr	m_out_d0		*note number
	bset.b	#6,p_seq_flag(a5)	*set key off bit
	bsr	m_out0			*#0
@@:
	st.b	(a1)+		*reset work area
	dbra	d2,out_lp_kof0
	exit_kof
kof_adp:			*ADPCM
	move.b	se_mode(pc),d0
	bne	exit_kof
	st.b	p_note(a5)
	move.b	p_non_off(a5),d2
	or.b	p_dumper(a5),d2
	bne	exit_kof	*se_mode,dumper on,non_off ならキーオフしない
	bsr	adpcm_end
	bset.b	#6,p_seq_flag(a5)	*set key off bit
	exit_kof

run_cmd:			*コマンド解釈
	reset_sp_cmd2
	movea.l	p_data_pointer(a5),a0	*compiled data address
next_cmd:			*ジャンプする時はd0のビット8～31はall zero
	moveq.l	#0,d0
	move.b	(a0)+,d0
	bpl	case_key	*音階データ
	add.b	d0,d0		*わざと.bにして最上位ビットを殺す
	move.w	int_cmd_tbl(pc,d0.w),d1
	jmp	int_cmd_tbl(pc,d1.w)
a:
n:	equ	play_end-a
int_cmd_tbl:						*コマンド群
	dc.w	do_rest-a				*$80 休符 R
	dc.w	n
	dc.w	n_off-a					*$82 NOISE MODE OFF @O
	dc.w	waiting-a				*$83 待つ W
	dc.w	revive_velo-a				*$84 臨時ベロシティ復元
	dc.w	n,n,n
	dc.w	n,n,n,n,n,n,n,n
	dc.w	tmp_chg-a				*$90 テンポ @T
	dc.w	tmp_chg2-a				*$91 テンポ T
	dc.w	rltv_@t_p-a,rltv_@t_m-a			*$92～$93 相対テンポ @T±n
	dc.w	rltv_t_p-a,rltv_t_m-a			*$94～$95 相対テンポ T±n
	dc.w	rltv_@b_up-a,rltv_@b_dwn-a		*$96～$97 相対ベンド
	dc.w	wave_form_sel-a				*$98 波形タイプセレクト S
	dc.w	set_m_mode-a				*$99 モジュレーションモード選択 M
	dc.w	asgn_arcc-a				*$9a ARCCセット @C
	dc.w	opmd_y2_ope-a				*$9b ADPCM PLAY    y2,n
	dc.w	modu_hold-a				*$9c モジュレーションホールド H
	dc.w	n,n,n
	dc.w	neiro_com-a				*$a0 音色切り換え @n
	dc.w	neiro_com-a				*$a1 音色切り換え MXDRV互換音色切り替え
	dc.w	opmd_y13_ope-a				*$a2 ADPCM FRQ CHG y13,n
	dc.w	asgn_chg-a				*$a3 アサイン変更 @N
	dc.w	n					*!!!
	dc.w	noise_set-a				*$a5 NOISE MODE @O
	dc.w	fo_mode-a				*$a6 FADE OUT MODE SWITCH \n
	dc.w	dumper-a				*$a7 DUMPER @D
	dc.w	bend_range-a				*$a8 BEND RANGE CHANGE
	dc.w	frq_chg-a				*$a9 ADPCM FRQ CHANGE @F
	dc.w	rltv_up-a,rltv_dwn-a			*$aa～$ab 相対ボリュームアップダウン
	dc.w	rythm_mode-a				*$ac non key off mode @R
	dc.w	len0_note-a				*$ad 音長０の発音
	dc.w	soft_amd-a				*$ae FM soft amd @A
	dc.w	send_sync-a				*$af シンクロ信号送信 Wn
	dc.w	panpot-a,panpot-a,panpot-a,panpot-a	*$b0～$b3 パンポット P
	dc.w	panpot2-a				*$b4 MIDI パンポット @P
	dc.w	y_com-a					*$b5 Yコマンド Y
	dc.w	volume-a				*$b6 VOLUME V
	dc.w	opmd_y3_ope-a				*$b7 ADPCM ﾊﾟﾝﾎﾟｯﾄ y3,n
	dc.w	opmd_y14_ope-a				*$b8 ADPCM SE MODE y14,n
	dc.w	velocity-a				*$b9 velocity set @U
	dc.w	n					*!!!
	dc.w	pmodsw-a,arccsw-a,bendsw-a,aftcsw-a	*$bb～$be スイッチ
	dc.w	kill_note-a				*$bf 強制キーオフ `
	dc.w	seq_cmd-a				*$c0 SEQUENCE CMD []
	dc.w	repeat_start-a				*$c1 |:
	dc.w	repeat_end-a				*$c2 :|
	dc.w	repeat_skip-a				*$c3 |n
	dc.w	repeat_skip2-a				*$c4 |only
	dc.w	midi_tie_mode-a				*$c5 "
	dc.w	n
	dc.w	switch-a				*$c7 macro switch =
	dc.w	rltv_pan_up-a,rltv_pan_dwn-a		*$c8～$c9 相対パンポット
	dc.w	rltv_velo_up-a,rltv_velo_dwn-a		*$ca～$cb 相対ベロシティアップダウン
	dc.w	fade_all-a				*$cc フェード・イン／アウト
	dc.w	len0_note-a				*$cd 音長ゼロ｢タイ｣
	dc.w	forceplay-a				*$ce 強制再演奏
	dc.w	inc_rep_cnt-a				*$cf ２回目以降の |:
	dc.w	w_com-a					*$d0 ウェイト @W
	dc.w	detune-a				*$d1 デチューン @K/@B
	dc.w	NRPN-a					*$d2 NRPN set @Y
	dc.w	bank_select-a				*$d3 bank select I
	dc.w	n					*!!!
	dc.w	poke-a					*$d5 ワーク直接書き換えコマンド ?
	dc.w	speed-a					*$d6 モジュレーションスピード@S
	dc.w	poke_up-a,poke_dwn-a			*$d7～$d8 ワーク直接書き換え相対 ?
	dc.w	exc_velo-a				*$d9 臨時ベロシティ
	dc.w	exc_velo_up-a,exc_velo_dwn-a		*$da～$db 臨時ベロシティアップ/ダウン
	dc.w	n,n,n,n
	dc.w	portament-a				*$e0 ポルタメント ()
	dc.w	bend-a					*$e1 オートベンド @B
	dc.w	chord-a					*$e2 和音 ''
	dc.w	after_touch-a				*$e3 アフタータッチシーケンス @Z
	dc.w	n					*!!!
	dc.w	n					*!!!
	dc.w	soft_pmd-a				*$e6 soft pitch modulation @M
	dc.w	n
	dc.w	delay-a					*$e8 モジュレーションディレイ@H
	dc.w	n					*$e9 version 1.04以前の@S
	dc.w	send_ex-a				*$ea ﾛｰﾗﾝﾄﾞｴｸｽｸﾙｰｼﾌﾞｾﾝﾄﾞ
	dc.w	ID_set-a				*$eb ID SET @I
	dc.w	send_data-a				*$ec MIDI生データ転送
	dc.w	effect_ctrl-a				*$ed ｴﾌｪｸﾄｺﾝﾄﾛｰﾙ @E
	dc.w	modulation8-a				*$ee PMOD 1/8 @M
	dc.w	arcc_amod8-a				*$ef ARCC/AMOD 1/8 @A
	dc.w	next_cmd-a,skip_zmd-a,skip_zmd2-a	*$f0,$f1,$f2ダミー
	dc.w	n,n,n,n,n
	dc.w	n,n,n,n
	dc.w	noteoff-a				*$fc スペシャルnote off
	dc.w	noteon-a				*$fd スペシャルnote on
	dc.w	w_step_key-a				*$fe 2バイトのstep time使用の音階データ
	dc.w	play_end-a				*$ff 演奏終了

play_end:			*演奏終了
	* X d0 d1 a1 a2
	move.b	jump_flg2(pc),d0	*d0に意味無し
	bpl	@f
	bsr	reset_jump2		*[@]の後始末
@@:
	tst.b	p_se_mode(a5)
	bne	@f			*normal($ff)/mask mode($00)の時は後始末をする
	move.b	#1,p_not_empty(a5)	*se mode($01)ならマークするだけ
	clr.b	p_fo_mode(a5)
	rts
@@:
	cmpi.b	#8,p_ch(a5)
	beq	@f			*ADPCMは無視
	bsr	ms_key_off
@@:
	move.b	#1,p_not_empty(a5)
	clr.b	p_fo_mode(a5)
	move.w	#-1,p_note(a5)
	btst.b	#5,p_seq_flag(a5)
	beq	@f
	bsr	back_patch		*jump mode on ならパッチを復元
@@:
	move.b	loop_chk(pc),d0		*d0に意味無し
	bne	all_end?		*func $3b?
	rts

all_end?:				*func $3b用の処理
	lea	play_trk_tbl(pc),a1
	addq.b	#1,loop_chk_trk-play_trk_tbl(a1)	*一応
	move.l	seq_wk_tbl(pc),a2
@@:
	move.b	(a1)+,d0
	bmi	@f
	lsl.w	#wk_size2,d0
	tst.b	p_not_empty(a2,d0.w)
	bne	@b
	rts
@@:					*全トラックが演奏終了した
	lea	loop_bsr(pc),a1
	clr.b	loop_chk-loop_bsr(a1)	*off
	movea.l	(a1),a1
	moveq.l	#-1,d0
	jmp	(a1)

tmp_chg:			*＠Ｔコマンド
	lea	timer_value(pc),a1
	move.b	(a0)+,(a1)+
	move.b	(a0)+,(a1)+
tcg0:
	bsr	gyakusan_t
wrt_tmp:			*func$19で書き変わる/-aスイッチでないとset_timer_bに書き変わる
	bsr.w	set_timer_a
_@t_midi_clk:			*-eスイッチがないと'bra next_cmd'になる
	if	(type<>3.and.type<>4)
	bsr	midi_clk
	endif
	bra	next_cmd
gyakusan_t:			*@T値→T値変換
	* < a1.l=timer_value+2
	bra.s	gyakusan_tm_a	*-aスイッチでないとNOPに書き変わる
gyakusan_tm_b:
	move.w	#256,d0
	sub.w	-(a1),d0
	lsl.w	#4,d0
	move.l	tmp_base(pc),d1	*d1=16*4000*60000/(1024*48)
	divu	d0,d1
	swap	d1
	tst.w	d1		*余りがあるか
	beq	@f
	add.l	#$0001_0000,d1	*余りがあるなら切り上げ
@@:
	swap	d1		*d1.w=answer
	move.w	d1,-(a1)
	rts
gyakusan_tm_a:
	move.w	#1024,d0
	sub.w	-(a1),d0
	move.l	tmp_base(pc),d1	*d1=16*4000*60000/(1024*48)
	divu	d0,d1
	move.w	d1,-(a1)
	rts

rltv_@t_p:
	lea	timer_value(pc),a1
	move.w	(a1),d1
	MOVEW	(a0)+,d0
	add.w	d0,d1
	move.w	#$3ff,d0
	tst.b	timer_a_mode-timer_value(a1)
	bne	@f
	move.w	#$ff,d0
@@:
	cmp.w	d0,d1
	bls	@f
	move.w	d0,d1
@@:
	move.w	d1,(a1)+
	bra	tcg0

rltv_@t_m:
	lea	timer_value(pc),a1
	move.w	(a1),d1
	MOVEW	(a0)+,d0
	sub.w	d0,d1
	bcc	@f
	moveq.l	#0,d1
@@:
	move.w	d1,(a1)+
	bra	tcg0

tmp_chg2:			*Ｔコマンド
	lea	m_tmp_buf(pc),a1
	move.b	(a0)+,(a1)+
	move.b	(a0)+,(a1)+
go_calctm:
	bsr	calc_tm_b_
	bmi	next_cmd
	move.w	d1,timer_value-m_tmp_buf-2(a1)
	bra	wrt_tmp

rltv_t_p:
	lea	m_tmp_buf(pc),a1
	move.w	(a1),d1
	MOVEW	(a0)+,d0
	add.w	d0,d1
	move.w	#tempo_max,d0
	cmp.w	d0,d1
	bls	@f
	move.w	d0,d1
@@:
	move.w	d1,(a1)+
	bra	go_calctm

rltv_t_m:
	lea	m_tmp_buf(pc),a1
	move.w	(a1),d1
	MOVEW	(a0)+,d0
	sub.w	d0,d1
	moveq.l	#20,d0
	tst.b	timer_a_mode-m_tmp_buf(a1)
	beq	@f
	moveq.l	#77,d0
@@:
	cmp.w	d0,d1
	bcc	@f
	move.w	d0,d1
@@:
	move.w	d1,(a1)+
	bra	go_calctm

set_timer:			*タイマーBの初期化(init_timerとほぼ同じ)
	moveq.l	#$12,d1
	move.w	timer_value(pc),d2	*get timer value
	bra	opmset

set_timer_a:			*タイマーAの初期化(init_timer_bほぼ同じ)
	moveq.l	#$11,d1
	move.w	timer_value(pc),d2	*timer A value
	bsr	opmset			*set value L
	lsr.w	#2,d2
	moveq.l	#$10,d1
	bra	opmset			*set value H

y_com:				*Ｙコマンド
	move.b	(a0)+,d1	*reg number
	move.b	(a0)+,d2	*data
	tst.b	p_se_mode(a5)	*効果音モード(mask ok)
	beq	next_cmd
	move.b	p_ch(a5),d0
	cmpi.b	#8,d0		*ADPCM TRACK でのyはFMへ(だから#8)
	bhi	midi_y
	bsr	opmset
	cmpi.b	#$1b,d1
	bne	next_cmd
	move.b	d2,OPMCTRL.w	*ＯＳのワークへもセットする
	bra	next_cmd
midi_y:				*MIDIでのyはコントロールチェンジ
	add.b	#$b0-9,d0	*ctrl chg
	bsr	m_out_d0
	bsr	m_out_d1
	bsr	m_out_d2
	bra	next_cmd

exit_ssk	macro
	move.l	a0,p_data_pointer(a5)	*次回に備える
	rts
	endm

case_key:			*音階データ処理
	* < d0.b=key code
	moveq.l	#0,d1
	move.b	d1,p_waon_flg(a5)
	move.l	d1,p_port_flg(a5)
	move.b	(a0)+,d1
	move.w	d1,(a5)		*p_on_count
case_key_patch:
	nop
	nop
	moveq.l	#0,d2
	move.b	(a0)+,d2
	move.w	p_gate_time(a5),d3	*gate timeが０でないということはtie
	sne	p_tie_flg(a5)
	move.w	d2,p_gate_time(a5)
	bsr	calc_param		*特殊コマンド関係の計算
do_key_ope:
	* < d0.l=key code
	* < d3.w=tie or not
	tst.b	p_se_mode(a5)	*効果音モード(or mask)ならキーオンしない
	bpl	exit_ssk
	bsr	tie_and_dumper	*ダンパーやタイ時に置けるノート処理
	beq	do_key_on
	bpl	do_key_on_
exit_ssk:
	exit_ssk

noteon:				*special note on
noteon_patch:
	nop
	nop
	move.b	(a0)+,d0
	move.b	(a0)+,d3
	move.b	d3,p_velo(a5)
	tst.b	p_se_mode(a5)
	bpl	@f
	move.b	p_ch(a5),d1
	add.b	#$90-9,d1
	bsr	m_out_d1		*note on
	bsr	m_out_d0		*key code
	tas.b	p_seq_flag(a5)		*set key on bit
	bsr	m_out_d3		*velocity
@@:
	bsr	stntwk
	bra	next_cmd

noteoff:			*special note off
noteoff_patch:
	nop
	nop
	move.b	(a0)+,d0
	move.b	(a0)+,d3
	tst.b	p_se_mode(a5)
	bpl	@f
	move.b	p_ch(a5),d1
	add.b	#$80-9,d1
	bsr	m_out_d1		*note on
	bsr	m_out_d0		*key code
	bset.b	#6,p_seq_flag(a5)	*set key off bit
	bsr	m_out_d3		*velocity
@@:				*ノート番号をワークから削除
	lea	p_note(a5),a1
	moveq.l	#max_note_on-1,d2
@@:
	move.b	(a1)+,d1
	bmi	next_cmd
	cmp.b	d1,d0		*これ?
	beq	thats		*そうだ
	dbra	d2,@b
	bra	next_cmd
thats:
	subq.b	#1,d2
	bmi	thats0
@@:
	move.b	(a1)+,-2(a1)
	bmi	next_cmd
	dbra	d2,@b
thats0:
	st.b	-(a1)
	bra	next_cmd

len0_note:			*音長ゼロの特殊ケース
len0_patch:			*[@]/[!]/func $19パッチ
	nop
	nop
	move.b	d0,d1
	move.b	(a0)+,d0
	tst.b	p_se_mode(a5)		*効果音モード(or mask)ならキーオンしない
	bpl	next_cmd
	cmpi.b	#$5a,d1			*$ad*2 and $7f
	beq	l0n_1			*旧式対応
	moveq.l	#0,d3
	move.w	d3,p_gate_time(a5)	*タイをむりやりキャンセル(つじつま合せ)
	lea	p_note(a5),a1
	moveq.l	#max_note_on-2,d2	*normalノートオン用のワークを確保するため'-2'
	moveq.l	#0,d5
@@:
	move.b	(a1)+,d1
	bmi	@f
	cmp.b	d1,d0
	beq	next_cmd		*なにもしない
	addq.b	#1,d5
	dbra	d2,@b
@@:
	st.b	(a1)
	move.b	d0,-(a1)
	move.b	p_ch(a5),d4
	cmpi.b	#8,d4
	bcs	@f
	pea	next_cmd(pc)
	bne	do_key_on__	*MIDI
	bra	adpcm_keyon	*ADPCM
@@:				*FM
	add.b	d5,d4
	cmpi.b	#7,d4
	bhi	next_cmd
	bsr	dumper_inst_ope
	bsr	do_fm_key_on
	bra	next_cmd

l0n_1:
	pea	next_cmd(pc)

do_key_on:
	move.b	p_ch(a5),d4
do_key_on_:
	cmpi.b	#8,d4
	bcs	do_fm_key_on
	beq	adpcm_keyon
do_key_on__:
	sub.b	#9,d4		*make real midi ch
	bsr	midi_note_on
	exit_ssk

adpcm_keyon:			*case:adpcm
	tst.b	p_fo_mode(a5)
	beq	@f
	bpl	exit_ssk	*fade in mode/func $47の時はキーオンしない
@@:
	moveq.l	#0,d1
	move.b	p_pgm(a5),d1
	bmi	@f
	lsl.w	#7,d1
	add.w	d1,d0
@@:
	move.l	adpcm_tbl(pc),d2
	beq	exit_ssk
	move.l	d2,a2
	lsl.l	#3,d0		*8倍
	adda.l	d0,a2
	move.l	(a2)+,d0	*address
	beq	exit_ssk
	movea.l	d0,a1		*data address
	move.l	(a2)+,d2	*size
	beq	exit_ssk
	move.b	se_mode(pc),d0	*d0に意味なし
	bne	exit_ssk	*se modeで演奏中ならexit
	move.w	p_frq(a5),d1
	move.b	p_pan(a5),d1
	move.l	a0,p_data_pointer(a5)	*次回に備える
	tas.b	p_seq_flag(a5)		*set key on bit
	bra	adpcmout		*ADPCM START TO PLAY

tie_and_dumper:			*タイやダンパーに置ける処理
	* < d0.l=note number
	* < d3.w=tie or not
	* > eq=goto do_key_on (主チャンネルでキーオン)
	* > pl=goto do_key_on_(リターン値でキーオン)
	* > mi=goto exit_ssk  (キーオンしない)
	* > d4.b=ch(時と場合による)
	* - d0
	* X d1-d5/a1
	tst.b	p_dumper(a5)	*dumper on?
	bne	set_note_num2
tie_on??:
	tst.w	d3		*tie on ?
	bne	set_note_num3
stntwk:				*ノート番号ワークセット
	lea	p_note(a5),a1
	moveq.l	#max_note_on-1,d2
@@:
	move.b	(a1)+,d1
	bmi	@f
	cmp.b	d1,d0		*同じノートがすでにノートオン済み
	beq	nts1		*何もしないでexit
	dbra	d2,@b
	bra	to0
@@:
	st.b	(a1)		*じつはp_extra_chまでおよぶこともある・・・でも問題ない
to0:
	move.b	d0,-(a1)
	moveq.l	#0,d2		*goto do_key_on
	rts

set_note_num2:			*case:dumper on
	move.b	p_ch(a5),d4
	cmpi.b	#8,d4
	bcc	tie_on??	*ADPCM/MIDIは関係なし
	lea	p_note(a5),a1
	moveq.l	#max_note_on-1,d2
	sub.b	d4,d2		*d2=loop_counter
snn2_lp01:
	move.b	(a1)+,d1
	bmi	exit_snn2_lp01
	cmp.b	d1,d0
	beq	same_snn2
	addq.b	#1,d4
	dbra	d2,snn2_lp01
				*8CH目を超えた時(同じキーコードも無かった)
	move.b	p_ch(a5),d4	*ＤＶＡもどきの処理を行なう
	moveq.l	#0,d1
	move.b	p_dmp_n(a5),d1
	add.b	d1,d4
	cmpi.b	#7,d4
	bls	@f
				*主チャンネルをオーバーしたら
	sub.b	d1,d4		*d4=主チャンネル
	move.b	#1,p_dmp_n(a5)
	move.b	d0,p_note(a5)
	bra	non_tie_snn2
@@:
	move.b	d0,p_note(a5,d1.w)	*いちばん古い音を殺す
	addq.b	#1,d1
	move.b	d1,p_dmp_n(a5)
	bra	non_tie_snn2
exit_snn2_lp01:
	st.b	(a1)
	move.b	d0,-(a1)
non_tie_snn2:
	bsr	dumper_inst_ope
	moveq.l	#1,d2
	rts
same_snn2:
	tst.w	d3		*tie?
	beq	non_tie_snn2
snn3_same:			*同じノートがあった
nts1:				*タイで同じキーなら何もしないでexit
	moveq.l	#-1,d2
	rts

dumper_inst_ope:
	* X d5,d6
	opmset	#8,d4		*=fmkey_off	d4
	cmp.b	p_ch(a5),d4	*主チャンネルの場合は無視して次へ
	beq	@f
	bset.b	d4,p_waon_mark(a5)
	bne	@f
	move.w	d0,d5
	set_wn_p		*主チャンネルのパラメータに合わせる(X d0,d1)
	move.w	d5,d0
@@:
	rts

set_note_num3:			*case:tie on
	lea	p_note(a5),a1
	moveq.l	#0,d5		*タイ処理で使用
	moveq.l	#0,d6		*loopカウンタ兼ch_offset(FM)
snn3_lp01:
	move.b	(a1)+,d1	*d1=last note
	bmi	exit_snn3_lp01
	move.b	p_ch(a5),d4
	cmp.b	d1,d0
	bne	key_off2
	moveq.l	#-1,d5		*mark:find same note
	cmpi.b	#8,d4
	beq	snn3_0
	bcs	snn3_case_fm
	btst.b	#3,p_md_flg(a5)	*special tie mode?
	beq	snn3_0
*	tst.b	p_pmod_mode(a5)	*!2.03
*	bmi	@f		*!2.03
*	tst.w	p_pmod_flg(a5)	*!2.03
*	bne	snn3_0		*!2.03
@@:
	bsr	special_tie_ope
	bra	snn3_0
snn3_case_fm:
*	tst.w	p_pmod_flg(a5)	*!2.03
*	bne	snn3_0		*!2.03
	bsr	set_fm_tune
	bra	snn3_0
key_off2:				*TIE時の関係ない音のキーオフ処理
	* < d1.l=key off note (FMの場合は関係なし)
	* < d4.b=p_ch
	* < d6.l=fm ch offset(MIDIの場合は関係無し)
	tst.b	p_non_off(a5)
	bne	exit_kof2
	cmpi.b	#8,d4
	bcs	kof2_fm
	beq	exit_kof2		*kof2_adp
kof2_midi:
	btst.b	#3,p_md_flg(a5)
	bne	exit_kof2
	add.b	#$80-9,d4
	bsr	m_out_d4		*cmd
	bsr	m_out_d1		*note number
	bset.b	#6,p_seq_flag(a5)	*set key off bit
	bsr	m_out0			*#0
	bra	exit_kof2
kof2_fm:
	tst.b	d6			*主チャンネルの場合は無視
	beq	exit_kof2
	add.b	d6,d4			set offset
	opmset	#8,d4			*=fmkey_off	d6		*fm key off
	bset.b	#6,p_seq_flag(a5)	*set key off bit
exit_kof2:
snn3_0:
	addq.w	#1,d6
	cmpi.b	#max_note_on-1,d6
	bls	snn3_lp01
exit_snn3_lp01:
	tst.b	d5
	bne	snn3_same	*同じノートが一つでもあった
	move.b	p_ch(a5),d4
	cmpi.b	#8,d4
	beq	@f
	bcs	fm_sptie
	btst.b	#3,p_md_flg(a5)
	beq	@f
	move.b	p_note(a5),d1
	bsr	special_tie_ope
	moveq.l	#-1,d2
	rts
@@:
	move.b	d0,p_note(a5)	*store note
	st.b	p_note+1(a5)
	moveq.l	#1,d2
	rts
fm_sptie:
	move.b	d0,p_note(a5)	*store note
*	tst.w	p_pmod_flg(a5)	*!2.03
*	bne	@f		*!2.03
	bsr	set_fm_tune	*!2.03
*@@:				*!2.03
	moveq.l	#-1,d2
	rts

special_tie_ope:		*スペシャルタイ処理
	* < d1.b=source note
	* < d0.b=dest. note
	* < d4.b=p_ch
	* - d0.b
	* X d1,d4
	add.b	#$e0-9,d4		*bend
	bsr	m_out_d4

	move.b	d0,d4
	sub.b	d1,d4
	ext.w	d4
	muls	#683,d4
	move.w	d4,p_sp_tie(a5)

	add.w	p_detune_m(a5),d4	*get det. param.
	add.w	#8192,d4
	bpl	sto0
	moveq.l	#0,d4
	bra	sto1
sto0:
	cmpi.w	#16383,d4
	bls	sto1
	move.w	#16383,d4
sto1:
	move.w	d4,d1
	andi.b	#127,d1
	bsr	m_out_d1	*lower @b
	bset.b	#0,p_md_flg(a5)
	lsr.w	#7,d4
	bra	m_out_d4	*higher @b

fm_bend:				*ベンドパラメータの計算
	move.b	p_bend_sw(a5),d6
	beq	exit_fm_bend
	tst.w	p_port_flg(a5)
	bne	exit_fm_bend		*ポルタメント中はオートベンドしない
	move.w	p_bend_dly(a5),d2	*ベンドディレイ値を
	move.w	d2,p_port_dly(a5)	*portament delayの所にセット
	sub.w	d2,d1			*step=step-delay
	bls	exit_fm_bend
	ext.w	d6
	move.w	d6,p_bend_flg(a5)	*ベンド方向をセット
	move.w	p_bend_rng_f(a5),d2
	ext.l	d2
	divs	d1,d2		*d2=range/L d2.w=step counter
	move.w	d2,p_port_step(a5)
	clr.w	d2
	swap	d2
	tst.w	d2
	bpl	fb1
	neg.w	d2		*d2=絶対値
fb1:
	lsl.l	#8,d2		*d2=d2*256
	divu	d1,d2		*d2=d2/L
	swap	d2
	tst.w	d2
	beq	fb2
	add.l	#$0001_0000,d2
	clr.w	d2
fb2:
	move.l	d2,p_port_work(a5)	*set 	p_port_work(.b)=0
					*	p_port_rvs(.b)=d2.hw(b)
					*	p_port_work2(.w)=0
exit_fm_bend:
	rts

midi_bend:				*ﾋﾟｯﾁﾍﾞﾝﾄﾞﾊﾟﾗﾒｰﾀ
	move.b	p_bend_sw(a5),d6
	beq	exit_midi_bend
	tst.w	p_port_flg(a5)
	bne	exit_midi_bend
	move.w	p_bend_dly(a5),d2	*ベンドディレイ値を
	move.w	d2,p_port_dly(a5)	*portament delayの所にセット
	sub.w	d2,d1			*step=step-delay
	bls	exit_midi_bend
	ext.w	d6
	move.w	d6,p_bend_flg(a5)	*ベンド方向をセット
	move.w	p_bend_rng_m(a5),d2
	ext.l	d2
	divs	d1,d2		*d2=range/L d2.w=step counter
	move.w	d2,p_port_step(a5)
	clr.w	d2
	swap	d2
	tst.w	d2
	bpl	mb1
	neg.w	d2		*d2=絶対値
mb1:
	lsl.l	#8,d2		*d2=d2*256
	divu	d1,d2		*d2=d2/L
	swap	d2
	tst.w	d2
	beq	mb2
	add.l	#$0001_0000,d2
	clr.w	d2
mb2:
	move.l	d2,p_port_work(a5)	*set 	p_port_work(.b)=0
					*	p_port_rvs(.b)=d2.hw(b)
					*	p_port_work2(.w)=0
	bset.b	d2,p_md_flg(a5)		*=#0
exit_midi_bend:
	rts

midi_aftc:			*ｱﾌﾀｰﾀｯﾁｼｰｹﾝｽﾊﾟﾗﾒｰﾀ
fm_aftc:			*アフタータッチシーケンスパラメータ
	tst.b	p_aftc_sw(a5)
	beq	@f
	move.l	d5,d1
	lsr.w	#3,d1		*d1=step/8
				*ゼロになっちゃっても後でつじつまが合うから大丈夫
	move.w	d1,p_aftc_dly(a5)
	move.w	d1,p_aftc_work(a5)
	st.b	p_aftc_flg(a5)
	clr.b	p_aftc_n(a5)	*init pointer
@@:
	rts

fm_pmod:			*ピッチモジュレーションパラメータ
	move.b	p_pmod_sw(a5),d1
	beq	exit_fm_pmod
	move.w	p_pmod_dpt(a5),d6
	beq	case_fmd_dly0
	move.w	p_pmod_dly(a5),p_pmod_work(a5)	*init delay
	moveq.l	#0,d2
	move.l	d2,p_pmod_work2(a5)	*(p_pmod_work2～3を初期化)
	move.w	d2,p_pmod_work5(a5)	*1/8モードオフ
					*p_pmod_work2=kf値work
	ext.w	d1			*p_pmod_work3=補正値ワーク
	move.w	d1,p_pmod_flg(a5)	*p_pmod_work4=周期ワーク
	move.w	p_pmod_spd(a5),d1
	move.b	p_pmod_wf(a5),d2
	bmi	wv_pmd
	move.b	_fp1(pc,d2.w),d2
	jmp	_fp1(pc,d2.w)
_fp1:	dc.b	svpmsp-_fp1
	dc.b	fpkuk-_fp1
	dc.b	@f-_fp1
	dc.b	svpmsp-_fp1
@@:
	lsr.w	#1,d1			*d1=d1/2(case:tri)
svpmsp:
	move.w	d1,p_pmod_work4(a5)	*init speed work
	move.w	p_pmod_step(a5),p_pmod_step2(a5)	*init step
	rts
fpkuk:
	move.w	d1,p_pmod_work4(a5)	*init speed work
	move.w	d6,p_pmod_work2(a5)
	rts
wv_pmd_:
	clr.w	p_pmod_work(a5)
	move.w	p_pmod_spd(a5),d1
wv_pmd:					*波形メモリのケース
	lsr.w	#1,d1			*d1=d1/2
	move.w	d1,p_pmod_work4(a5)	*init speed work
	neg.b	d2
	subq.w	#wv_def_max,d2
	add.w	d2,d2		*2
	move.w	d2,d1
	add.w	d2,d2		*4
	move.w	d2,d6
	add.w	d2,d2		*8
	add.w	d1,d2		*12
	add.w	d6,d2		*14
	move.l	wave_tbl(pc),a1
	adda.w	d2,a1
	move.l	(a1)+,p_wvpm_point(a5)
	move.l	(a1)+,p_wvpm_end(a5)
	move.l	(a1)+,p_wvpm_loop(a5)
	move.w	(a1)+,p_wvpm_lpmd(a5)
	st.b	p_pmod_flg(a5)
	moveq.l	#0,d2
	move.b	d2,p_altp_flg(a5)	*反復モードスイッチオフ
	move.w	d2,p_pmod_work5(a5)	*1/8モードオフ
	bra	pmod_wvmm
case_fmd_dly0:				*ディレイがゼロのケース
	move.l	d5,d1
	lsr.w	#3,d1
	move.w	d1,p_pmod_work5(a5)	*1/8 step time
	move.w	d1,p_pmod_work6(a5)
	moveq.l	#0,d2
	move.b	p_pmod_wf(a5),d2
	bmi	wv_pmd_
	btst.b	#0,p_pmod_omt(a5)
	beq	cont_last_pm		*初めが省略ならば前回のものを継続
	move.w	p_pmod_spd(a5),d1
	move.b	_fp2(pc,d2.w),d2
	jmp	_fp2(pc,d2.w)
_fp2:	dc.b	svpm8sp-_fp2
	dc.b	fp8kuk-_fp2
	dc.b	@f-_fp2
	dc.b	svpm8sp-_fp2
@@:
	lsr.w	d1
svpm8sp:
	move.w	d1,p_pmod_work4(a5)	*init speed work
	moveq.l	#1,d1
	move.w	p_mstep_tbl(a5),d2
	move.w	d2,p_pmod_step2(a5)
	bpl	@f
	moveq.l	#-1,d1
@@:
	move.w	d1,p_pmod_flg(a5)
	move.b	p_mrvs_tbl(a5),d1
	move.b	d1,p_pmod_rvs(a5)
	or.b	d1,d2
	moveq.l	#0,d1
	move.l	d1,p_pmod_work(a5)	*init p_pmod_work,p_pmod_work2
	move.w	d1,p_pmod_work3(a5)	*init p_pmod_work3,p_pmod_n
	tst.w	d2			*振幅0のケースは
	bne	@f
	moveq.l	#1,d1
@@:
	move.b	d1,p_pmod_chain(a5)
	rts
fp8kuk:					*矩形波の場合
	move.w	d1,p_pmod_work4(a5)	*init speed work
	st.b	p_pmod_flg(a5)		*取り敢えずフラグをオン
	moveq.l	#0,d1
	move.w	d1,p_pmod_work(a5)	*init p_pmod_work
	move.b	d1,p_pmod_n(a5)		*init pmod pointer
	move.w	p_pmod_tbl(a5),p_pmod_work2(a5)
	bne	@f
	moveq.l	#1,d1
@@:
	move.b	d1,p_pmod_chain(a5)
	rts
cont_last_pm:				*前回のものを継続する場合
	moveq.l	#0,d1
	move.b	d1,p_pmod_n(a5)		*init pmod pointer
	tst.w	p_pmod_flg(a5)
	bne	@f
	moveq.l	#1,d1			*波形接続なし
	move.w	d1,p_pmod_flg(a5)	*取り敢えずフラグをオン
@@:
	move.b	d1,p_pmod_chain(a5)
exit_fm_pmod:
	rts

midi_pmod:				*ﾋﾟｯﾁﾓｼﾞｭﾚｰｼｮﾝﾊﾟﾗﾒｰﾀ
	tst.b	p_pmod_sw(a5)
	beq	exit_midi_pmod
	tst.b	p_pmod_mode(a5)
	bpl	special_pmod
	tst.w	p_pmod_dpt(a5)
	beq	case_pmod_dly0
	move.w	p_pmod_dly(a5),p_pmod_work(a5)	*init p_pmod_work
	clr.w	p_pmod_work5(a5)		*init p_pmod_work5(1/8 off)
	st.b	p_pmod_flg(a5)
	rts
case_pmod_dly0:				*ディレイがゼロのケース
	move.l	d5,d1
	lsr.w	#3,d1
	move.w	d1,p_pmod_work5(a5)
	move.w	d1,p_pmod_work6(a5)
	moveq.l	#1,d1
	moveq.l	#0,d6
	move.b	d6,p_pmod_n(a5)
	move.w	d6,p_pmod_work(a5)
	move.b	d1,p_pmod_flg(a5)
	btst.b	d6,p_pmod_omt(a5)
	beq	exit_midi_pmod		*初めが省略ならば前回のものを継続
	tst.b	p_se_mode(a5)		*効果音モード(mask ok)
	beq	exit_midi_pmod
	moveq.l	#$b0,d2
	or.b	d4,d2
	bsr	m_out_d2
	bsr	m_out_d1		*pitch modulation cmd
	bset.b	d1,p_md_flg(a5)		*=#1
	m_out_	p_pmod_tbl+1(a5)
exit_midi_pmod:
	rts
special_pmod:				*スペシャルモード時
	bsr	fm_pmod
	tst.b	p_se_mode(a5)		*効果音モード(mask ok)
	beq	exit_midi_pmod
	bclr.b	#0,p_md_flg(a5)
	move.w	p_pmod_work2(a5),d2
	add.w	p_detune_m(a5),d2	*get det. param.
	cmp.w	#-8192,d2
	bge	@f
	move.w	#-8192,d2
	bra	set_@b_
@@:
	cmpi.w	#8191,d2
	ble	set_@b_
	move.w	#8191,d2
	bra	set_@b_

fm_amod:				*AMパラメータ
	move.b	p_amod_sw(a5),d1
	beq	exit_fm_amod
fm_amod_:
	move.b	p_amod_dpt(a5),d6
	beq	case_amd_dly0
	move.w	p_amod_dly(a5),p_amod_work(a5)	*init delay
	moveq.l	#0,d2
	move.l	d2,p_amod_work2(a5)	*init p_amod_work2,3,7(&p_amod_n)
	move.w	d2,p_amod_work5(a5)	*1/8モードオフ
	tst.b	d6
	bpl	@f
	move.b	d6,p_amod_work2(a5)
	move.b	d6,p_amod_work7(a5)
@@:
	move.b	d1,p_amod_flg(a5)	*p_amod_work2=vol値work
					*p_amod_work3=補正値ワーク
					*p_amod_work4=周期ワーク
	move.w	p_amod_spd(a5),d1
	move.b	p_amod_wf(a5),d2
	bmi	wv_amd
	move.b	fa1(pc,d2.w),d2
	jmp	fa1(pc,d2.w)
fa1:	dc.b	fasaw-fa1		*2.03
	dc.b	fakuk-fa1
	dc.b	fa0_-fa1
	dc.b	fasaw-fa1		*2.03
wv_amd_:
	move.w	d1,p_amod_work(a5)	*p_amod_work=0
	move.w	p_amod_spd(a5),d1
wv_amd:
	lsr.w	#1,d1			*d1=d1/2
	move.w	d1,p_amod_work4(a5)	*init speed work
	neg.b	d2
	subq.w	#wv_def_max,d2
	add.w	d2,d2		*2
	move.w	d2,d1
	add.w	d2,d2		*4
	move.w	d2,d6
	add.w	d2,d2		*8
	add.w	d1,d2		*12
	add.w	d6,d2		*14
	move.l	wave_tbl(pc),a1
	adda.w	d2,a1
	move.l	(a1)+,p_wvam_point(a5)
	move.l	(a1)+,p_wvam_end(a5)
	move.l	(a1)+,p_wvam_loop(a5)
	move.w	(a1)+,p_wvam_lpmd(a5)
	st.b	p_amod_flg(a5)
	moveq.l	#0,d2
	move.b	d2,p_alta_flg(a5)	*反復モードスイッチオフ
	move.w	d2,p_amod_work5(a5)	*1/8モードオフ
	bra	amod_wvmm		*!2.03
fasaw:
	add.w	d1,d1					*ノコギリ波
fa0_:
	move.w	d1,p_amod_work4(a5)			*init speed work
	move.b	p_amod_step(a5),p_amod_step2(a5)	*init step
	rts
fakuk:
	move.w	d1,p_amod_work4(a5)		*init speed work
	move.b	d6,p_amod_step2(a5)		*init step
	rts
case_amd_dly0:				*ディレイがゼロのケース(1/8 mode)
	lsr.w	#3,d5
	move.w	d5,p_amod_work5(a5)	*1/8 step time
	move.w	d5,p_amod_work6(a5)
	moveq.l	#0,d1
	moveq.l	#0,d2
	move.b	p_amod_wf(a5),d2
	bmi	wv_amd_
	btst.b	d1,p_amod_omt(a5)	*初めが省略ならば前回のものを継続
	beq	cont_last_am
	move.l	d1,p_amod_work(a5)	*init p_amod_work,p_amod_work2,p_amod_work3
	move.w	d1,p_amod_work7(a5)	*init p_amod_work7,p_amod_n
	move.b	p_amod_tbl(a5),d6
	beq	cad0
	bpl	@f
	move.b	d6,p_amod_work2(a5)
	move.b	d6,p_amod_work7(a5)
	st.b	p_amod_flg(a5)
	move.b	d1,p_amod_chain(a5)
	bra	cad1
cad0:
	moveq.l	#1,d1			*case:depth=0
@@:
	move.b	#1,p_amod_flg(a5)
	move.b	d1,p_amod_chain(a5)
cad1:
	move.w	p_amod_spd(a5),d1
	move.b	fa2(pc,d2.w),d2
	jmp	fa2(pc,d2.w)
fa2:	dc.b	@f-fa2
	dc.b	fa8kuk-fa2
	dc.b	fa1_-fa2
	dc.b	@f-fa2
@@:
	add.w	d1,d1			*ノコギリ波の場合
fa1_:
	move.w	d1,p_amod_work4(a5)	*init p_amod_work4
	move.b	p_astep_tbl(a5),p_amod_step2(a5)
	move.b	p_arvs_tbl(a5),p_amod_rvs(a5)
	rts
fa8kuk:
	move.w	d1,p_amod_work4(a5)	*init p_amod_work4
	move.b	d6,p_amod_step2(a5)
	rts
cont_last_am:
	moveq.l	#0,d1
	move.b	d1,p_amod_n(a5)		*init p_amod_n=1
	tst.b	p_amod_flg(a5)
	bne	@f			*いままでﾓｼﾞｭﾚｰｼｮﾝをしていないのに省略の時は
	moveq.l	#1,d1			*波形接続関係なし
	move.b	d1,p_amod_flg(a5)	*取り敢えずフラグをオン
@@:
	move.b	d1,p_amod_chain(a5)
exit_fm_amod:
	rts

midi_arcc:				*arccパラメータ計算
	move.b	p_arcc_sw(a5),d1
	beq	exit_midi_arcc
	tst.b	p_arcc_mode(a5)
	bpl	special_arcc
	tst.b	p_amod_dpt(a5)
	beq	case_arcc_dly0
	clr.w	p_arcc_work5(a5)		*init p_arcc_work5(1/8 off)
	st.b	p_arcc_flg(a5)
	move.w	p_arcc_dly(a5),p_arcc_work(a5)	*init p_arcc_work
	beq	exit_midi_arcc			*!2.06
	move.b	p_arcc_rst(a5),d2
	bmi	exit_midi_arcc
	moveq.l	#$b0,d1
	or.b	d4,d1
	bsr	m_out_d1		*ctrl chg
	move.b	p_arcc(a5),d1
	bsr	m_out_d1
	exg	d0,d2
	bsr	arcc_p_chk
	exg	d0,d2
	bra	m_out_d2
case_arcc_dly0:				*ディレイがゼロのケース
	lsr.w	#3,d5
	move.w	d5,p_arcc_work5(a5)
	move.w	d5,p_arcc_work6(a5)
	st	p_arcc_flg(a5)
	moveq.l	#0,d1
	move.b	d1,p_arcc_n(a5)
	move.w	d1,p_arcc_work(a5)	*init p_arcc_work
	btst.b	d1,p_arcc_omt(a5)
	beq	exit_midi_arcc		*初めが省略ならば前回のものを継続
	tst.b	p_se_mode(a5)		*効果音モード(mask ok)
	beq	exit_midi_arcc
	moveq.l	#$b0,d1
	or.b	d4,d1
	bsr	m_out_d1
	move.l	d0,d2
	move.b	p_arcc(a5),d1
	bsr	m_out_d1		*arcc control number
*	bset.b	#2,p_md_flg(a5)
	move.b	p_arcc_tbl(a5),d0
	bsr	arcc_p_chk
	exg.l	d0,d2
	bra	m_out_d2
exit_midi_arcc:
	rts
special_arcc:				*スペシャルモード時
	bsr	fm_amod_
	st.b	p_d6_last(a5)
	tst.b	p_amod_dpt(a5)
	bne	@f
	tst.b	p_amod_tbl(a5)
	bmi	exit_midi_arcc
@@:
	tst.b	p_se_mode(a5)		*効果音モード(mask ok)
	beq	exit_midi_arcc
	moveq.l	#$b0,d1
	or.b	d4,d1
	bsr	m_out_d1		*ctrl chg
	move.b	p_arcc(a5),d1
	bsr	m_out_d1
	move.l	d0,d2
	move.b	p_arcc_def(a5),d0
	tst.w	p_arcc_dly(a5)
	bne	@f
	add.b	p_amod_work2(a5),d0
	bpl	@f
	moveq.l	#0,d0
@@:
	bsr	arcc_p_chk
	exg	d0,d2
	move.b	d2,p_d6_last(a5)	*!2.03
	bra	m_out_d2

calc_param:			*特殊コマンドのパラメータ計算
	* - d0.b=note number
	* - d3.w=tie or not
	* < d1.w=step time
	* X d1 d2 d4 d5 d6
	move.l	d1,d5		*d5=step time too
	move.b	p_ch(a5),d4
	cmpi.b	#8,d4
	bhi	midi_clcp
				*FM
	tst.w	d3
	bne	case_fm_tie
	bsr	fm_bend			*ベンドパラメータ
	bsr	fm_aftc			*アフタータッチパラメータ
	btst.b	#6,p_md_flg(a5)
	beq	@f
	bset.b	#4,p_md_flg(a5)
	bne	cm_1
@@:
	bsr	fm_pmod			*モジュレーションパラメータ
cm_1:
	tst.b	p_md_flg(a5)
	bpl	fm_amod			*アンプリチュードモジュレーション
	bset.b	#5,p_md_flg(a5)
	beq	fm_amod			*アンプリチュードモジュレーション
	rts

case_fm_tie:
	tst.b	p_tie_bend(a5)
	beq	@f
	bsr	fm_bend
	clr.b	p_tie_bend(a5)
@@:
	clr.b	p_aftc_flg(a5)		*タイの場合は以後、以前のｱﾌﾀｰﾀｯﾁを継続
	tst.b	p_tie_aftc(a5)
	beq	@f
	bsr	fm_aftc
	clr.b	p_tie_aftc(a5)
@@:
	tst.b	p_tie_pmod(a5)
	beq	@f
	tst.b	p_pmod_wf(a5)		*波形メモリならば飛ばす!2.05
	bmi	@f
	move.w	p_pmod_work(a5),-(sp)	*save delay
	bsr	fm_pmod
	move.w	(sp)+,p_pmod_work(a5)	*get back delay
	clr.b	p_tie_pmod(a5)
	bra	bndpm
@@:
	clr.w	p_pmod_work6(a5)	*つじつま合わせ
bndpm:
	tst.b	p_tie_amod(a5)
	beq	@f
	tst.b	p_amod_wf(a5)		*波形メモリならば飛ばす!2.05
	bmi	@f
	move.w	p_amod_work(a5),-(sp)	*save delay
	bsr	fm_amod
	move.w	(sp)+,p_amod_work(a5)	*get back delay
	clr.b	p_tie_amod(a5)
@@:
	rts

midi_clcp:				*ＭＩＤＩの特殊コマンドパラメータ計算
	sub.b	#9,d4			*d4=midi ch(0-f)
	tst.w	d3
	bne	case_midi_tie
	bsr	midi_bend		*MIDIベンドパラメータ
	bsr	midi_aftc		*MIDIアフタータッチパラメータ
	btst.b	#6,p_md_flg(a5)
	beq	@f
	bset.b	#4,p_md_flg(a5)
	bne	mc1
@@:
	bsr	midi_pmod		*MIDIピッチモジュレーションパラメータ
mc1:
	tst.b	p_md_flg(a5)
	bpl	midi_arcc		*MIDI ARCCパラメータ
	bset.b	#5,p_md_flg(a5)
	beq	midi_arcc		*MIDI ARCCパラメータ
	rts

case_midi_tie:
	tst.b	p_tie_bend(a5)
	beq	@f
	bsr	midi_bend
	clr.b	p_tie_bend(a5)
@@:
	clr.b	p_aftc_flg(a5)		*タイの場合は以後、以前のｱﾌﾀｰﾀｯﾁを継続
	tst.b	p_tie_aftc(a5)
	beq	@f
	bsr	midi_aftc
	clr.b	p_tie_aftc(a5)
@@:
	tst.b	p_tie_pmod(a5)
	beq	@f
	move.w	p_pmod_work(a5),-(sp)	*save delay
	bsr	midi_pmod
	move.w	(sp)+,p_pmod_work(a5)	*save delay
	clr.b	p_tie_pmod(a5)
	bra	bndpm_m
@@:
	clr.w	p_pmod_work6(a5)	*つじつま合わせ
bndpm_m:
	tst.b	p_tie_arcc(a5)
	beq	@f
	move.w	p_arcc_work(a5),-(sp)	*save delay
	bsr	midi_arcc
	move.w	(sp)+,p_arcc_work(a5)	*get back delay
	clr.b	p_tie_arcc(a5)
@@:
	rts

w_com:				*＠Ｗコマンド
	moveq.l	#0,d1
	move.w	d1,p_gate_time(a5)	*gate time
	move.b	(a0)+,d1
	move.w	d1,(a5)		*p_on_count
w_com_patch:
	nop
	nop
	addq.w	#1,a0			*skip gate (dummy)
	move.l	a0,p_data_pointer(a5)	*次回の割り込み時に備える
	bra	pitch_rvs

do_rest:			*休符データ処理
	moveq.l	#0,d1
	move.b	(a0)+,d1
	move.w	d1,(a5)		*p_on_count
rest_patch:
	nop
	nop
	move.b	(a0)+,d1	*get gate time
	move.w	d1,p_gate_time(a5)
r_or_@w:
	move.l	a0,p_data_pointer(a5)	*次回に備える
*	bra	pitch_rvs

pitch_rvs:				*ピッチ補正(r,w)
	clr.w	p_pmod_work6(a5)	*つじつま合わせ
*	btst.b	#1,p_pb_add(a5)		*p_pitch_add
*	beq	@f
*	move.w	p_port_work2(a5),d1	*ﾍﾞﾝﾄﾞ後のﾋﾟｯﾁを保ちつつ
*	clr.w	p_port_work2(a5)
*	add.w	d1,p_pmod_work2(a5)	*ﾓｼﾞｭﾚｰｼｮﾝを継続
*@@:
	rts

w_step_key:			*ワードステップタイムのキー
	moveq.l	#0,d1
	move.b	d1,p_waon_flg(a5)
	move.l	d1,p_port_flg(a5)
	move.b	(a0)+,d0	*d0.l=note number
	move.b	(a0)+,(a5)+
	move.b	(a0)+,(a5)+
	move.w	-(a5),d1	*p_on_count
w_step_patch:
	nop
	nop
	move.w	p_gate_time(a5),d3	*gate timeが０でないということはタイである
	sne	p_tie_flg(a5)
	move.b	(a0)+,p_gate_time+0(a5)
	move.b	(a0)+,p_gate_time+1(a5)
	tst.b	d0			*休符＆ウェイト
	bmi	r_or_@w
	bsr	calc_param		*特殊コマンド関係の計算
	bra	do_key_ope
	move.l	a0,p_data_pointer(a5)	*次回に備える
	rts

panpot:			*Ｐコマンド
	move.b	d0,d1
	lsr.b	d1
	andi.w	#3,d1			*d1=0-3
	move.b	_pan_tbl(pc,d1.w),d0	*d0=pan2
	move.b	d1,p_pan(a5)
	move.b	d0,p_pan2(a5)
	clr.b	p_waon_mark(a5)
	tst.b	p_se_mode(a5)	*効果音モード(mask ok)
	beq	next_cmd
	move.b	p_ch(a5),d4
	cmpi.b	#8,d4
	bhi	pan_midi
	beq	pan_adpcm
					*FM
	move.b	d1,d2
	ror.b	#2,d2
	or.b	p_af(a5),d2
	moveq.l	#$20,d1
	or.b	d4,d1
	bsr	opmset
	bra	next_cmd
_pan_tbl:	dc.b	64,0,127,64
pan_midi:				*3段階パンポット
	tst.b	d1
	beq	case_midi_pan0		*pan0は特別
pan_midi_:				*MIDI
	add.b	#$b0-9,d4
	bsr	m_out_d4		*ctrl chg
	bsr	m_out10			*pan cmd
	bsr	m_out_d0
	bra	next_cmd
case_midi_pan0:				*PAN0は一応ボリュームゼロ,ベロシティゼロ
	add.b	#$b0-9,d4
	bsr	m_out_d4		*ctrl chg
	bsr	m_out7			*volume cmd
	bsr	m_out0			*volume=0
	clr.b	p_velo(a5)
	bra	next_cmd
pan_adpcm:
	tst.w	p_gate_time(a5)
	beq	next_cmd
pan_a0:				*pc8!
	lea	$e9a005,a1
	move.b	(a1),d0
	and.b	#%1111_1100,d0
	tst.b	d1
	beq	@f
	cmpi.b	#3,d1		*出力チェック
	bne	pa1
@@:
	eori.b	#%0000_0011,d1	*ビット反転
pa1:
	or.b	d1,d0
	move.b	d0,(a1)
	bra	next_cmd
pan_adpcm_8:
	moveq.l	#$70,d0
	or.b	p_extra_ch(a5),d0
	ori.l	#$ffff_ff00,d1		*vol/frq保存
	trap	#2
	bra	next_cmd

rltv_pan_up:			*相対パンポット・アップ
	move.b	p_pan2(a5),d0
	add.b	(a0)+,d0
	bpl	@f
	moveq.l	#127,d0
	bra	@f

rltv_pan_dwn:			*相対パンポット・ダウン
	move.b	p_pan2(a5),d0
	sub.b	(a0)+,d0
	bpl	@f
	moveq.l	#0,d0
	bra	@f

panpot2:			*MIDI専用多段階パンポット(FM/ADPCMも一応サポート)
	move.b	(a0)+,d0
@@:
	moveq.l	#1,d1
	cmpi.b	#31,d0		*便宜上４(3)段階パンにする
	bls	set_atp
	moveq.l	#3,d1
	cmpi.b	#95,d0
	bls	set_atp
	moveq.l	#2,d1
set_atp:
	move.b	d1,p_pan(a5)
	move.b	d0,p_pan2(a5)
	tst.b	p_se_mode(a5)	*SE modeなら帰還
	beq	next_cmd
	move.b	p_ch(a5),d4
	cmpi.b	#8,d4
	bhi	pan_midi_
	beq	pan_adpcm
				*FM
	move.b	d1,d2
	ror.b	#2,d2
	or.b	p_af(a5),d2
	moveq.l	#$20,d1
	or.b	d4,d1
	bsr	opmset
	bra	next_cmd

seq_cmd:			*シーケンス特殊コマンド群([～]コマンド群処理)
*	moveq.l	#0,d0
	move.b	(a0)+,d0
	add.w	d0,d0
	move.w	seq_cmd_tbl(pc,d0.w),d0
	jmp	seq_cmd_tbl(pc,d0.w)

seq_cmd_tbl:
	dc.w	next_cmd-seq_cmd_tbl	*0	RESERVED
	dc.w	next_cmd-seq_cmd_tbl	*1	RESERVED
	dc.w	next_cmd-seq_cmd_tbl	*2	RESERVED
	dc.w	dc_ope-seq_cmd_tbl	*3
	dc.w	segno_ope-seq_cmd_tbl	*4
	dc.w	ds_ope-seq_cmd_tbl	*5
	dc.w	coda_ope-seq_cmd_tbl	*6
	dc.w	tocoda_ope-seq_cmd_tbl	*7
	dc.w	fine_ope-seq_cmd_tbl	*8
	dc.w	do_ope-seq_cmd_tbl	*9
	dc.w	loop_ope-seq_cmd_tbl	*10
	dc.w	jump_ope-seq_cmd_tbl	*11
	dc.w	jump_ope2-seq_cmd_tbl	*12

dc_ope:				*3 [d.c.]処理(初めに帰る)
	btst.b	#0,p_seq_flag(a5)	*p_dc_flag
	bne	next_cmd		*一度処理済み
	btst.b	#2,p_seq_flag(a5)	*[coda]が今まで登場してないなら
	bne	dc_jump
	move.l	a0,p_coda_ptr(a5)
	bset.b	#2,p_seq_flag(a5)
dc_jump:
	move.b	-1(a6),d0
	bsr	top_ptr_set	*先頭アドレスをセット
	movea.l	p_data_pointer(a5),a0
	or.b	#%11,p_seq_flag(a5)	*p_dc_flag,p_fine_flag
	bra	next_cmd

segno_ope:			*4 [segno]処理(MEMORIZE WHERE IT IS.)
	bset.b	#3,p_seq_flag(a5)	*p_flag
	move.l	a0,p_pointer(a5)
	bra	next_cmd

ds_ope:				*5 [d.s.]処理([SEGNO]へジャンプ)
	btst.b	#3,p_seq_flag(a5)	*p_flag
	beq	next_cmd		*[segno]がなかったので無視
	btst.b	#4,p_seq_flag(a5)	*p_ds_flag
	bne	next_cmd		*[d.s.]一度処理したことがあるので無視
	ori.b	#%0001_0010,p_seq_flag(a5)	*p_ds_flag,p_fine_flag
	btst.b	#2,p_seq_flag(a5)	*[coda]が今まで登場してないなら
	bne	ds_jump
	move.l	a0,p_coda_ptr(a5)
	bset.b	#2,p_seq_flag(a5)
ds_jump:
	movea.l	p_pointer(a5),a0	*飛先アドレス
	bra	next_cmd

coda_ope:
	bset.b	#2,p_seq_flag(a5)	*p_coda_flag
	move.l	a0,p_coda_ptr(a5)
	bra	next_cmd

tocoda_ope:				*7 [tocoda]処理([coda]へジャンプ)
	btst.b	#2,p_seq_flag(a5)	*p_coda_flag
	beq	next_cmd
	movea.l	p_coda_ptr(a5),a0
	bra	next_cmd

fine_ope:			*[fine]処理
	btst.b	#1,p_seq_flag(a5)	*p_fine_flag
	beq	next_cmd
	bra	play_end

do_ope:				*[do]処理
	move.b	#1,p_do_loop_flag(a5)
	move.l	a0,p_do_loop_ptr(a5)
	move.l	p_total(a5),p_total_olp(a5)
	clr.l	p_total(a5)
	bra	next_cmd

case_calc:			*total count 計算モード時
	st.b	p_do_loop_flag(a5)	*loop命令のあったことをマーク
	bra	play_end

loop_bsr_ope	macro
	* < d1.b=現在までに繰り返した回数
	* X d0,d2,a1
	* - d1は破壊禁止
	cmp.b	loop_time(pc),d1
	bcs	exit_lbo
	moveq.l	#0,d0
	move.b	-1(a6),d0		*現在実行中のトラックナンバー(0-79)
	move.b	d0,d2
	lsr.b	#3,d0			*d0=d0/8
	lea	done_bit(pc,d0.w),a1
	bset.b	d2,(a1)
	bne	exit_lbo
	lea	play_trk_tbl(pc),a1
	moveq.l	#0,d2
lbo_lp:
	move.b	(a1)+,d0
	bmi	lbo0
	lsl.w	#wk_size2,d0
	move.l	seq_wk_tbl(pc),a2
	lea	(a2,d0.w),a2
	tst.b	p_marker(a2)		*!2.04
	bmi	lbo_lp			*!2.04 se trackは数えない
	addq.b	#1,d2
	bra	lbo_lp
done_bit:	ds.b	tr_max/8	*ワーク
lbo0:
	lea	loop_chk_trk(pc),a2
	addq.b	#1,(a2)
	cmp.b	(a2),d2
	bhi	exit_lbo
	movea.l	loop_bsr(pc),a1
	moveq.l	#0,d0			*d0.l=演奏中の全トラックがループした
	move.b	d0,loop_chk-loop_chk_trk(a2)	*off
	jsr	(a1)
exit_lbo:
	endm

loop_ope:			*[loop]処理([do]へジャンプ)
	nop			*パッチが当たることがある
	nop
	move.b	jump_flg2(pc),d0	*d0に意味無し
	bpl	@f
	bsr	reset_jump2	*[@]後始末
@@:
	btst.b	#5,p_seq_flag(a5)
	beq	@f
	bsr	back_patch	*[!]後始末
@@:
	move.b	p_do_loop_flag(a5),d1
	beq	play_end	*[do]が省略の場合は演奏終了
	move.b	loop_chk(pc),d0	*d0に意味無し
	beq	@f
	loop_bsr_ope		*ループ回数分ループしたら特定のアドレスへジャンプする
@@:
	addq.b	#1,d1		*inc loop time
	beq	@b		*0になったら１にする
	move.b	d1,p_do_loop_flag(a5)
	moveq.l	#0,d0			*ワーク初期化
	andi.b	#%1100_0000,p_seq_flag(a5)	*p_ds_flag	*!2.04
	move.l	d0,p_rpt_cnt(a5)
	move.l	d0,p_rpt_cnt+4(a5)
	move.b	d0,p_rpt_last?(a5)
	move.l	p_do_loop_ptr(a5),a0
	cmpi.b	#$c0,(a0)
	bne	next_cmd
	cmpi.b	#$0a,1(a0)	*loop ope
	bne	next_cmd
	bra	play_end	*[do]と[loop]の間に何もない場合は停止

jump_ope:			*次の[!]へ飛ぶ
	lea	case_key_patch(pc),a1
	tst.b	trace_mode-case_key_patch(a1)		*case:func$19
	beq	@f
	subq.l	#1,p_total(a5)
	bra	next_cmd
@@:
	tst.b	jump_flg2-case_key_patch(a1)
	bne	next_cmd
	bset.b	#5,p_seq_flag(a5)
	bne	end_jump_mode	*ジャンプモード解除へ
	move.l	#BRA*65536+((jump_plus1-case_key_patch-2).and.$ffff),(a1)
	move.l	#BRA*65536+((jump_plus1-rest_patch-2).and.$ffff),rest_patch-case_key_patch(a1)
	move.l	#BRA*65536+((jump_plus1-w_com_patch-2).and.$ffff),w_com_patch-case_key_patch(a1)
	move.l	#BRA*65536+((jump_plus8-port_patch-2).and.$ffff),port_patch-case_key_patch(a1)
	move.l	#BRA*65536+((jump_plus11-waon_patch-2).and.$ffff),waon_patch-case_key_patch(a1)
	move.l	#BRA*65536+((jump_plus2-w_step_patch-2).and.$ffff),w_step_patch-case_key_patch(a1)
	move.l	#BRA*65536+((jump_plus2-noteon_patch-2).and.$ffff),noteon_patch-case_key_patch(a1)
	move.l	#BRA*65536+((jump_plus2-noteoff_patch-2).and.$ffff),noteoff_patch-case_key_patch(a1)
	move.l	#BRA*65536+((jump_plus1-len0_patch-2).and.$ffff),len0_patch-case_key_patch(a1)
	move.l	#BRA*65536+((jump_plus1-opmd_y2_ope-2).and.$ffff),opmd_y2_ope-case_key_patch(a1)
	bra	next_cmd

jump2_plus1_:
jump_plus1:
	addq.w	#1,a0
	bra	next_cmd
jump2_plus2_:
jump_plus2:
	addq.w	#2,a0
	bra	next_cmd
jump_plus8:
	addq.w	#8,a0
	bra	next_cmd
jump_plus11:
	lea	11(a0),a0
	bra	next_cmd

end_jump_mode:			*ジャンプモード解除
	bclr.b	#5,p_seq_flag(a5)
	pea	next_cmd(pc)
	bra	back_patch

jump_ope2:			*次の[@]へ飛ぶ
	btst.b	#5,p_seq_flag(a5)
	bne	next_cmd
	lea	jump_flg2(pc),a1
	tst.b	trace_mode-jump_flg2(a1)		*case:func$19
	bne	next_cmd
	tst.b	(a1)
	bmi	end_jump_mode2				*ジャンプモード解除へ
*	bne	next_cmd				*１回のみ有効
	move.b	-1(a6),d0
	addq.b	#1,d0
	neg.b	d0
	move.b	d0,(a1)					*mark jump2 mode
	move.l	#BRA*65536+((jump2_plus1-case_key_patch-2).and.$ffff),case_key_patch-jump_flg2(a1)
	move.l	#BRA*65536+((jump2_plus1-rest_patch-2).and.$ffff),rest_patch-jump_flg2(a1)
	move.l	#BRA*65536+((jump2_plus1-w_com_patch-2).and.$ffff),w_com_patch-jump_flg2(a1)
	move.l	#BRA*65536+((jump2_plus8-port_patch-2).and.$ffff),port_patch-jump_flg2(a1)
	move.l	#BRA*65536+((jump2_plus11-waon_patch-2).and.$ffff),waon_patch-jump_flg2(a1)
	move.l	#BRA*65536+((jump2_plus2-w_step_patch-2).and.$ffff),w_step_patch-jump_flg2(a1)
	move.l	#BRA*65536+((jump2_plus2_-noteon_patch-2).and.$ffff),noteon_patch-jump_flg2(a1)
	move.l	#BRA*65536+((jump2_plus2_-noteoff_patch-2).and.$ffff),noteoff_patch-jump_flg2(a1)
	move.l	#BRA*65536+((jump2_plus1_-len0_patch-2).and.$ffff),len0_patch-jump_flg2(a1)
	move.l	#BRA*65536+((jump2_plus1_-opmd_y2_ope-2).and.$ffff),opmd_y2_ope-jump_flg2(a1)

	move.l	#BRA*65536+((reset_int_e-int_rte-2).and.$ffff),int_rte-jump_flg2(a1)
	bra	next_cmd

jump2_plus1:
	addq.w	#1,a0
	bra	exit_ssk
jump2_plus2:
	addq.w	#2,a0
	bra	exit_ssk
jump2_plus8:
	addq.w	#8,a0
	bra	exit_ssk
jump2_plus11:
	lea	11(a0),a0
	bra	exit_ssk

end_jump_mode2:			*ジャンプモード#2解除
	pea	next_cmd(pc)
reset_jump2:			*play_endからも参照
	movem.l	d0/a1,-(sp)
	lea	jump_flg2(pc),a1
	move.b	(a1),d0
	neg.b	d0
	subq.b	#1,d0
	cmp.b	-1(a6),d0		*不一致
	bne	@f
	clr.b	(a1)			*make jump_flg2 clear
	move.l	rte_src(pc),int_rte-jump_flg2(a1)	*ori.w
	bsr	back_patch
@@:
	movem.l	(sp)+,d0/a1
	rts

repeat_start:			*|:n処理
	bsr	push_rep_cnt	*push repeat counter
	addq.w	#2,a0
	bra	next_cmd

inc_rep_cnt:			*リピートカウンタ処理
	bsr	get_rc_adr	*>a1=rept work,d1.b=number of work seq.
	addq.b	#1,(a1)		*inc work cnt
	cmpm.b	(a0)+,(a1)+	*もしラストリピートならゼロに...
	bne	next_cmd
	bset.b	d1,p_rpt_last?(a5)	*最終リピートならマーク
	bra	next_cmd

repeat_end:			*:|処理
*	moveq.l	#0,d0
	MOVEW	(a0)+,d0	*d0.w=offset value
	move.l	a0,d2		*save a0 to d2
	suba.l	d0,a0		*offsetを引く
	bsr	pop_rep_cnt	*>d0=rep_cnt,a1=rep_work,d1.b=number of work seq.
	cmp.b	1(a0),d0	*cmp with src_cnt and d0
	bne	next_cmd	*go repeat!
	clr.b	(a1)		*ワーク初期化
	bclr.b	d1,p_rpt_last?(a5)
	movea.l	d2,a0		*リピートしないで次へ
	bra	next_cmd

repeat_skip:			*|n処理
	move.b	(a0)+,d2	*d2=repeat count
	bsr	pop_rep_cnt	*d0=inter rep cnt
	cmp.b	d0,d2		*same rept cnt??
	bne	skip_to_next	*飛ぶ
	addq.w	#2,a0		*通常に次のデータを処理しに行く
	bra	next_cmd
skip_to_next:
	moveq.l	#0,d0		*!2.03
	MOVEW	(a0)+,d0	*d0=offset
	adda.l	d0,a0		*オフセットを足して飛ぶ
	bra	next_cmd

repeat_skip2:			*|処理(nが省略時のケース)
	bsr	get_rc_adr	*a1=last or not work address,d1.b=number of work seq.
	bmi	rs_next
	btst.b	d1,p_rpt_last?(a5)	*nzならラスト
	bne	skip_to_next2		*ラストならスキップ
rs_next:
	addq.w	#2,a0		*通常に次のデータを処理しに行く
	bra	next_cmd
skip_to_next2:			*最後のリピートケース
	moveq.l	#0,d0
	move.b	d0,(a1)
	bclr.b	d1,p_rpt_last?(a5)
	MOVEW	(a0)+,d0	*d0=offset
	adda.l	d0,a0		*オフセットを足して飛ぶ
	bra	next_cmd

push_rep_cnt:			*リピートカウンタのセット
	* > a1=work address
	* > d1.b=number of work seq.
	* X d1,a1
	lea	p_rpt_cnt(a5),a1
	moveq.l	#0,d1
@@:
	tst.b	(a1)+
	beq	vacant_prc
	addq.b	#1,d1
	cmpi.b	#7,d1
	bls	@b
vacant_prc:
	addq.b	#1,-(a1)	*一杯ならば最後のをつぶす
	rts

pop_rep_cnt:			*リピートカウンタのゲット
	* > d0.b=repeat counter
	* > a1=work address
	* > d1.b=number of work seq.
	* X d1,a1
	lea	p_rpt_cnt+8(a5),a1
	moveq.l	#8-1,d1
@@:
	tst.b	-(a1)
	bne	exact_pprc
	dbra	d1,@b
	moveq.l	#-1,d0		*case error(255ということは必ずループを抜ける)
	rts
exact_pprc:
	move.b	(a1),d0		*d0=work cnt
	rts

get_rc_adr:			*リピートカウンタのアドレス
	* > a1=work address
	* > pl=non error
	* > mi=error
	* > d1.b=number of work seq.
	* X d1,a1
	lea	p_rpt_cnt+8(a5),a1
	moveq.l	#8-1,d1
@@:
	tst.b	-(a1)
	bne	exact_gra
	dbra	d1,@b
exact_gra:
	tst.w	d1
	rts

asgn_chg:			*＠Ｎコマンド
	move.b	(a0)+,d0
	move.b	d0,d2
	moveq.l	#0,d1
	move.w	d1,p_sp_tie(a5)
	move.b	d1,p_extra_ch(a5)
	moveq.l	#ch_max-1,d1
	cmp.b	d1,d0
	bls	@f
	sub.b	d1,d0
	move.b	d0,p_extra_ch(a5)
	moveq.l	#8,d0
@@:
	move.b	d0,p_ch(a5)
*---------------------------------------
	tst.b	p_marker(a5)		*効果音トラックに関しては処理しない。
	bmi	exit_soac
	lea	play_trk_tbl(pc),a1	*切り替え後のチャンネルと同じチャンネルの
@@:					*パラメータをコピーしてくる
	move.b	(a1)+,d1
	bmi	exit_soac
	lsl.w	#wk_size2,d1
	movea.l	seq_wk_tbl(pc),a2
	adda.w	d1,a2
	cmp.b	p_ch(a2),d0
	bne	@b
npc8_6:					*npc8!
	cmpi.b	#8,d4
	bne	ac0
	move.b	p_extra_ch(a2),d1
	cmp.b	p_extra_ch(a5),d1
	bne	@b
ac0:
*	move.b	p_pgm(a2),p_pgm(a5)
*	move.w	p_pan(a2),p_pan(a5)
	move.b	p_vol(a2),p_vol(a5)
*	move.l	p_detune_f(a2),p_detune_f(a5)
exit_soac:
*---------------------------------------
	lea	ch_tr_msk(pc),a1
	tst.b	(a1)+
	beq	otpl??			*トラックマスクなら以下の処理不用

	move.l	mask_wk(pc),d1
	pea	otpl??(pc)
	btst.l	d2,d1
	bne	do_msk			*チャンネル無効化
	bra	en_msk			*チャンネル有効化
otpl??:
	tst.b	(a1)+
	beq	next_cmd		*トラックアウトプットレベルなら以下の処理不用
	move.l	out_wk(pc),d1
	btst.l	d2,d1
	beq	next_cmd
	move.b	outlvl(pc),d3
	bsr	st_otl			*X d4破壊
	bra	next_cmd

revive_velo:
	move.b	p_velo_dmy(a5),p_velo(a5)
	st.b	p_velo_dmy(a5)
	cmpi.b	#8,p_ch(a5)
	bhi	next_cmd
				*ＦＭ/ADPCMの場合はvolume と同じ処理だけれど...
	move.b	(a0),d0
	cmpi.b	#$aa,d0		*~
	beq	next_cmd
	cmpi.b	#$ab,d0		*_
	beq	next_cmd
	cmpi.b	#$b6,d0		*@v/v
	beq	next_cmd
	cmpi.b	#$b9,d0		*@u/u
	beq	next_cmd
	cmpi.b	#$ca,d0		*u+
	beq	next_cmd
	cmpi.b	#$cb,d0		*u-
	beq	next_cmd

	moveq.l	#127,d0
	sub.b	p_velo(a5),d0
	bra	_volume

exc_velo_up:
	tst.b	p_velo_dmy(a5)
	bpl	@f
	move.b	p_velo(a5),p_velo_dmy(a5)
@@:
	move.b	p_velo_dmy(a5),d1
	add.b	(a0)+,d1
	bpl	@f
	moveq.l	#127,d1
@@:
evd0:
	move.b	d1,p_velo(a5)
	cmpi.b	#8,p_ch(a5)
	bhi	next_cmd
				*ＦＭ/ADPCMの場合はvolume と同じ処理
	moveq.l	#127,d0
	sub.b	p_velo(a5),d0
	bra	_volume

exc_velo_dwn:
	tst.b	p_velo_dmy(a5)
	bpl	@f
	move.b	p_velo(a5),p_velo_dmy(a5)
@@:
	move.b	p_velo_dmy(a5),d1
	sub.b	(a0)+,d1
	bcc	evd0
	moveq.l	#0,d1
	bra	evd0

exc_velo:
	tst.b	p_velo_dmy(a5)
	bpl	velocity
	move.b	p_velo(a5),p_velo_dmy(a5)

velocity:			*＠Ｕコマンド
	move.b	(a0)+,p_velo(a5)
	cmpi.b	#8,p_ch(a5)
	bhi	next_cmd
				*ＦＭ/ADPCMの場合はvolume と同じ処理
	moveq.l	#127,d0
	sub.b	p_velo(a5),d0
	bra	_volume

rltv_velo_up:			*相対ベロシティアップ
	move.b	p_velo(a5),d1
	add.b	(a0)+,d1
	bpl	fm_velo?
	moveq.l	#127,d1
fm_velo?:
	move.b	d1,p_velo(a5)
	cmpi.b	#8,p_ch(a5)
	bhi	next_cmd
				*ＦＭ/ADPCMの場合はvolume と同じ処理
	moveq.l	#127,d0
	sub.b	d1,d0
	bra	_volume

rltv_velo_dwn:			*相対ベロシティダウン
	move.b	p_velo(a5),d1
	sub.b	(a0)+,d1
	bcc	fm_velo?
	moveq.l	#0,d1
	bra	fm_velo?

rltv_up:			*相対ボリュームアップ
	move.b	p_vol(a5),d0
	sub.b	(a0)+,d0
	bcc	_volume
	moveq.l	#0,d0
	bra	_volume

rltv_dwn:			*相対ボリュームダウン
	move.b	(a0)+,d0
	add.b	p_vol(a5),d0
	bpl	_volume
	moveq.l	#127,d0
	bra	_volume

volume:				*Ｖコマンド
	move.b	(a0)+,d0	*v=127-volume value
_volume:
	move.b	p_ch(a5),d4
	bsr	set_other_ch_v	*new v valueをセーブ
	bsr	chk_vol_big
	clr.b	p_waon_mark(a5)
	tst.b	p_se_mode(a5)	*効果音モード(mask ok)
	beq	next_cmd
	cmpi.b	#8,d4
	bhi	midi_volume
	beq	adpcm_volume
	tst.b	p_amod_flg(a5)	*!2.03
	bne	next_cmd	*!2.03
	pea	next_cmd(pc)	*return addr.

do_volume:
	* < d0=127-volume
	* < d4=fm ch
	* - d0
	* X d1,d2
	move.b	p_cf(a5),d1
	swap	d1
	move.w	#$60,d1
	or.b	d4,d1

	btst.l	#0+16,d1
	beq	dv3
	bsr	dao_@v1
dv3:
	addq.b	#8,d1
	btst.l	#2+16,d1
	beq	dv2
	bsr	dao_@v3
dv2:
	addq.b	#8,d1
	btst.l	#1+16,d1
	beq	dv4
	bsr	dao_@v2
dv4:
	addq.b	#8,d1
	bra	dao_@v4

adpcm_volume:
				*npc8!
	tst.w	p_gate_time(a5)	*gate timeが０でないということはtie
	beq	next_cmd
				*tieの時のみ処理する
	moveq.l	#127,d1
	sub.b	d0,d1
	moveq.l	#$70,d0
	or.b	p_extra_ch(a5),d0
	lsr.b	#3,d1		*0～f
	swap	d1
	move.w	#-1,d1		*pan/frq保存
	trap	#2
	bra	next_cmd

midi_volume:
	add.b	#$b0-9,d4		*ctrl chg
	bsr	m_out_d4
	bsr	m_out7		*volume cmd
	moveq.l	#127,d1
	sub.b	d0,d1
	bsr	m_out_d1
	bra	next_cmd

*pcm_volume:			*pcm8モードの時
*	move.b	pcm8_flg(pc),d1
*	beq	next_cmd
*	moveq.l	#127,d1
*	sub.b	d0,d1
*	lsr.b	#3,d1		*0～f
*	swap	d1
*	move.w	#-1,d1
*	moveq.l	#$70,d0
*	or.b	p_extra_ch(a5),d0
*	trap	#2
*	bra	next_cmd

rltv_@b_up:
	MOVEW	(a0)+,d0
	add.w	d0,p_detune_f(a5)
	move.w	#7680,d1
	cmp.w	p_detune_f(a5),d1
	bge	@f
	move.w	d1,p_detune_f(a5)
@@:
*	MOVEW	(a0)+,d0
	add.w	d0,p_detune_m(a5)
	move.w	#8191,d1
	cmp.w	p_detune_m(a5),d1
	bge	det0
	move.w	d1,p_detune_m(a5)
	bra	det0

rltv_@b_dwn:
	MOVEW	(a0)+,d0
	sub.w	d0,p_detune_f(a5)
	move.w	#-7680,d1
	cmp.w	p_detune_f(a5),d1
	ble	@f
	move.w	d1,p_detune_f(a5)
@@:
*	MOVEW	(a0)+,d0
	sub.w	d0,p_detune_m(a5)
	move.w	#-8192,d1
	cmp.w	p_detune_m(a5),d1
	ble	det0
	move.w	d1,p_detune_m(a5)
	bra	det0

detune:
	lea	p_detune_f(a5),a1
	move.b	(a0)+,(a1)+		*get p_detune_f,p_detune_m
	move.b	(a0)+,(a1)+
	move.b	(a0)+,(a1)+
	move.b	(a0)+,(a1)+
det0:
	moveq.l	#0,d2			*offset=0
	move.b	d2,p_bend_sw(a5)	*detuneを設定するとオートベンドは停止
	move.w	d2,p_bend_flg(a5)	*detuneを設定するとオートベンドは停止	!!!
	tst.b	p_se_mode(a5)		*効果音モード(mask ok)
	beq	next_cmd
	bclr.b	d2,p_md_flg(a5)		*ここで設定するからノートオンの時はノータッチ
	pea	next_cmd(pc)
	move.b	p_ch(a5),d4
	cmpi.b	#8,d4
	bhi	case_md_dtn		*MIDIの場合は…
	beq	exit_stdt
set_detune:
	* < d2.b=kf
	* < d4.b=ch (0<=ch<=7)
	moveq.l	#7,d3
	sub.b	d4,d3
	lea	p_note(a5),a1
@@:				*和音をやっている場合も考慮
	move.b	(a1)+,d0
	bmi	exit_stdt
	move.l	d2,d5
	bsr	set_fm_kc2	*FM音源キーコードをセット
	addq.b	#1,d4
	dbra	d3,@b
exit_stdt:
	rts

case_md_dtn:				*ＭＩＤＩ DETUNE
	sub.b	#9,d4

set_@b:				*ベンド値書き込み
	* < d4.b=midi ch(0-f)
	* X d1,d2
	* d0/a1-a2  破壊禁止
	move.w	p_detune_m(a5),d2
set_@b_:
	moveq.l	#$e0,d1
	or.b	d4,d1		*pitch bend
	bsr	m_out_d1
	add.w	#8192,d2
	move.w	d2,d1
	andi.b	#127,d2
	bsr	m_out_d2		*lower @b
	lsr.w	#7,d1
	bra	m_out_d1		*higher @b

reset_@m:
	* d0/a1-a2　破壊禁止
	tst.b	p_pmod_sw(a5)
	beq	@f
	tst.w	p_pmod_dly(a5)	*ディレイがない場合は関係無し
	beq	exit_rst@m
@@:
	moveq.l	#$b0,d1
	or.b	d4,d1
	bsr	m_out_d1		*ctrl chg
	bsr	m_out1
	bra	m_out0
exit_rst@m:
	rts

portament:			*ポルタメント
	move.b	(a0)+,d0	*d0=start note number
	move.b	(a0)+,(a5)+	*get step time
	move.b	(a0)+,(a5)+	*d1=step time
	move.w	-(a5),d1	*p_on_count
port_patch:
	nop
	nop
	move.w	p_gate_time(a5),d3	*if d3≠0 then tie
	sne	p_tie_flg(a5)
	move.b	(a0)+,p_gate_time+0(a5)
	move.b	(a0)+,p_gate_time+1(a5)	*set gate time
	move.b	(a0)+,p_port_dly+0(a5)
	move.b	(a0)+,p_port_dly+1(a5)	*set delay time
	move.b	(a0)+,p_port_step+0(a5)
	move.b	(a0)+,p_port_step+1(a5)	*set step cnt
	move.b	(a0)+,p_port_rvs(a5)	*set revise parameter
	move.b	(a0)+,d2
	ext.w	d2
	move.w	d2,p_port_flg(a5)	*ベンド方向
	moveq.l	#0,d2
	move.b	d2,p_waon_flg(a5)
	move.b	d2,p_port_work(a5)	*reset revise work
	move.w	d2,p_port_work2(a5)	*reset counter work
	bsr	calc_param		*特殊コマンド関係の計算( < d1.w=step time)
	tst.b	p_se_mode(a5)
	bpl	exit_ssk
	bsr	tie_and_dumper		*タイやダンパーに置ける処理(d0をワークへもセット)
	beq	port_normal
	bmi	exit_ssk		*タイで同じノート番号ならば
	cmpi.b	#8,d4
	beq	exit_ssk
	bhi	port_normal

do_fm_key_on:			*FM音源のキーオン
	* < d3.b=tie flag
	* < d4.l=fm ch		*(ここを変更したら和音のところも同じにすること)
	* < d0.l=note number
	move.l	a0,p_data_pointer(a5)	*次回に備える
fm_kco:				*ＦＭ音源のキーコード書き込みとキーオン
	bsr	set_fm_tune	*ＦＭ音源の音程設定
fm_key_on:			*ＦＭ音源のキーオン書き込み
	* < d3.b=tie or not
	* < d4.b=fm ch
	* X d0-d2
	* - d4,d6,a1
	tst.b	p_aftc_flg(a5)	*アフタータッチシーケンス時の音量初期化
	beq	@f
	bsr	do_fm_aftc
@@:
	tst.b	p_amod_flg(a5)	*AM実行時の音量初期化
	beq	@f
	bsr	reset_v2
@@:
	tst.b	p_vset_flg(a5)	*単純な音量初期化
	beq	@f
	bsr	reset_v
@@:
	move.b	d4,d2		*ch number
	or.b	p_om(a5),d2	*set operator mask
	moveq.l	#$08,d1
	bsr	opmset
	tas.b	p_seq_flag(a5)	*set key on bit
	tst.b	p_sync(a5)
	beq	exit_fko	*case:sync off
	tst.w	d3
	bne	exit_fko	*tie ならLFOリセットしない
	moveq.l	#$01,d1
	moveq.l	#$02,d2
	bsr	opmset		*HARD LFO RESET/第１ビットをオン
	moveq.l	#0,d2
	bra	opmset		*第１ビットをオフ(LFO RESET)
exit_fko:
fko1:
	rts

reset_v:			*ボリュームリセット
	moveq.l	#127,d1
	move.b	p_vol(a5),d0
	sub.b	d0,d1
	move.b	d1,p_last_aft(a5)
	bsr	chk_vol_big
	bsr	reset_ol
	clr.b	p_vset_flg(a5)
	rts

reset_ol:			*トータルレベルの初期化
	* < d0.b=volume
	* < d4.b=ch
	* X d1,d2
	move.b	p_cf(a5),d1
	swap	d1
	move.w	#$60,d1
	or.b	d4,d1

	btst.l	#0+16,d1
	bne	@f
	move.b	p_ol1(a5),d2	*OP1
	bsr	opmset
	bra	ro3
@@:
	bsr	dao_@v1
ro3:
	addq.b	#8,d1
	btst.l	#2+16,d1
	bne	@f
	move.b	p_ol3(a5),d2	*OP3
	bsr	opmset
	bra	ro2
@@:
	bsr	dao_@v3
ro2:
	addq.b	#8,d1
	btst.l	#1+16,d1
	bne	@f
	move.b	p_ol2(a5),d2	*OP2
	bsr	opmset
	addq.b	#8,d1
	bra	dao_@v4
@@:
	bsr	dao_@v2
	addq.b	#8,d1
	bra	dao_@v4

reset_v2:			*AM時のボリュームリセット
	tst.w	d3
	bne	fko1
	moveq.l	#127,d1
	move.b	p_vol(a5),d0
	sub.b	d0,d1
	move.b	d1,p_last_aft(a5)
	tst.w	p_amod_dly(a5)
	bne	rv_dly
	move.w	d6,-(sp)
	move.b	p_amod_work2(a5),d6
	sub.b	d6,d0
	bpl	@f
	moveq.l	#127,d0
@@:
	bsr	chk_vol_big
	move.b	p_arcc(a5),d3
	and.b	p_cf(a5),d3
	bsr	do_amod_ope
	move.w	(sp)+,d6
	moveq.l	#0,d3
	move.b	d3,p_vset_flg(a5)
	rts
rv_dly:
	bsr	chk_vol_big
	bsr	reset_ol
	move.b	d3,p_vset_flg(a5)
	rts

do_fm_aftc:			*FMのｱﾌﾀｰﾀｯﾁｼｰｹﾝｽ
	* < d4.b=fm ch
	* X d0-d2
	tst.w	d3
	bne	fko1
	moveq.l	#0,d1
	move.b	p_aftc_n(a5),d1
	move.b	p_aftc_tbl(a5,d1.w),d2
	bpl	dfa1

	tst.w	d1
	beq	dfa0		*初めての時は…
@@:
	subq.b	#1,d1
	bmi	dfa0		*一番初めも省略
	move.b	p_aftc_tbl(a5,d1.w),d2
	bpl	dfa1
	bra	@b
dfa0:
	moveq.l	#127,d2
	sub.b	p_vol(a5),d2
dfa1:				*実際に音量を書き込む
	move.b	d2,p_last_aft(a5)
	moveq.l	#127,d0
	sub.b	d2,d0
	bpl	@f
	moveq.l	#127,d0
@@:
	tst.b	p_amod_flg(a5)
	beq	rv_dly			*アフタータッチオンリー
	tst.w	p_amod_dly(a5)
	bne	rv_dly
	move.w	d6,-(sp)
	move.b	p_amod_work2(a5),d6
	sub.b	d6,d0
	bpl	@f
	moveq.l	#127,d0
@@:
	bsr	chk_vol_big
	move.b	p_arcc(a5),d3
	or.b	p_cf(a5),d3
	bsr	case_aft_am
	move.w	(sp)+,d6
	moveq.l	#0,d3
	move.b	d3,p_vset_flg(a5)
	rts

set_fm_tune:
	* < d0.b=midi note data	*デチューン、のパラメータも考慮する
	* < d4.b=fm ch
	* X d0,d1,d2
	* - d3-d4 a1
	ext.w	d0		*as same as 'andi.w #$ff,d0'
	move.w	p_detune_f(a5),d1	*get det. param.
	addq.w	#5,d1		*4MHz offset
	move.w	d1,d2
	asr.w	#6,d2		*d1.w=d1.w/64=kc offset value
	beq	@f
	add.w	d2,d0
	andi.w	#$003f,d1	*d1=ｱﾏﾘ
@@:
	add.b	d1,d1
	add.b	d1,d1		*２つシフト

	moveq.l	#$30,d2		*kf
	or.b	d4,d2
	opmset	d2,d1

	subq.b	#8,d2		*$28+n
	opmset	d2,<key_code_tbl(pc,d0.w)>	*kc
	rts

set_fm_kc2:
	* < d0.b=midi note number
	* < d5.w=total kf value
	* < d4.b fm ch
	* X d0-d1,d3,d5
	* - d2 d4 a1
	andi.w	#$7f,d0			*最上位ビットがたっている場合もあるからext.wでは駄目
	addq.w	#5,d5		*4MHz offset
	add.w	p_detune_f(a5),d5	*get det. param.
	move.w	d5,d1
	asr.w	#6,d5		*d5.w=d5.w/64=kc offset value
	beq	@f
	add.w	d5,d0
	andi.w	#$003f,d1	*d1=ｱﾏﾘ
@@:
	add.b	d1,d1
	add.b	d1,d1		*２つシフト

	cmpi.w	#110,d0
	bhi	exit_kc2
	sub.w	#15,d0
	bmi	exit_kc2
	moveq.l	#$30,d3		*key code
	or.b	d4,d3
	opmset	d3,d1		*kf

	subq.b	#8,d3		*$28+n
	opmset	d3,<key_code_tbl+15(pc,d0.w)>	*kc
exit_kc2:
	rts

key_code_tbl:				*ＦＭ音源のキーコードのテーブル
	*	c   c#  d   d#  e   f   f#  g   g#  a   a#  b
	dc.b	$0C,$0D,$0E,$00,$01,$02,$04,$05,$06,$08,$09,$0A	*o-1(便宜上)
	dc.b	$0C,$0D,$0E,$00,$01,$02,$04,$05,$06,$08,$09,$0A	*o0
	dc.b	$0C,$0D,$0E,$10,$11,$12,$14,$15,$16,$18,$19,$1A	*o1
	dc.b	$1C,$1D,$1E,$20,$21,$22,$24,$25,$26,$28,$29,$2A	*o2
	dc.b	$2C,$2D,$2E,$30,$31,$32,$34,$35,$36,$38,$39,$3A	*o3
	dc.b	$3C,$3D,$3E,$40,$41,$42,$44,$45,$46,$48,$49,$4A	*o4
	dc.b	$4C,$4D,$4E,$50,$51,$52,$54,$55,$56,$58,$59,$5A	*o5
	dc.b	$5C,$5D,$5E,$60,$61,$62,$64,$65,$66,$68,$69,$6A	*o6
	dc.b	$6C,$6D,$6E,$70,$71,$72,$74,$75,$76,$78,$79,$7A	*o7
	dc.b	$7C,$7D,$7E,$70,$71,$72,$74,$75,$76,$78,$79,$7A	*o8
	dc.b	$7C,$7D,$7E,$70,$71,$72,$74,$75			*o9(便宜上)
	.even

port_normal:
	move.b	p_ch(a5),d4
	cmpi.b	#8,d4
	bls	do_fm_key_on
	sub.b	#9,d4			*make real midi ch
	move.l	a0,p_data_pointer(a5)	*次回に備える
	tst.w	d3
	beq	@f
	btst.b	#3,p_md_flg(a5)
	bne	special_tie_ope
@@:
	bset.b	#0,p_md_flg(a5)		*=#0

midi_note_on:
	* < d4.b=midi ch(0-f)
	* < d0.b=note number
	* - d4
	* X d1-d2,d6
	* d0/a1-a2 破壊禁止
				*特殊コントローラのリセット
	clr.w	p_sp_tie(a5)
	move.b	p_md_flg(a5),d6
				*check @b flg
	bclr.l	#0,d6
	beq	@f
	bsr	set_@b
@@:				*check @m flg
	bclr.l	#1,d6
	beq	@f
	bsr	reset_@m
@@:				*check @a flg
*	bclr.l	#2,d6
*	beq	@f
*	reset_@a
@@:
	move.b	d6,p_md_flg(a5)
midi_note_on2:
	moveq.l	#$90,d1
	or.b	d4,d1
	bsr	m_out_d1
midi_note_on3:
	tas.b	p_seq_flag(a5)	*set key on bit
	bsr	m_out_d0		*key code
	tst.b	p_aftc_flg(a5)
	bne	@f
do_mno0:
	m_out_	p_velo(a5)
	rts
@@:				*after touch sequence考慮
	moveq.l	#0,d1
	move.b	p_aftc_n(a5),d1
	move.b	p_aftc_tbl(a5,d1.w),d0
	bmi	@f
	bra	m_out_d0
@@:				*省略
	tst.w	d1
	beq	do_mno0		*初めの時は…
@@:
	subq.b	#1,d1
	bmi	do_mno0		*一番初めも省略
	move.b	p_aftc_tbl(a5,d1.w),d0
	bmi	@b		*省略ならばもっと前の
	bra	m_out_d0

kill_note:			*強制キーオフ
	move.b	p_dumper(a5),d6	*save
	clr.b	p_dumper(a5)
	bsr	key_off
	move.b	d6,p_dumper(a5)
	clr.w	p_gate_time(a5)
	bra	next_cmd

chord:				*和音コマンド
	moveq.l	#0,d1
	move.l	d1,p_port_flg(a5)
	move.b	(a0)+,(a5)+	*get step time
	move.b	(a0)+,(a5)+
	move.w	-(a5),d1	*p_on_count
waon_patch:
	nop
	nop
	move.w	p_gate_time(a5),d3	*if d3≠0 then tie
	sne	p_tie_flg(a5)
	move.b	(a0)+,p_gate_time+0(a5)
	move.b	(a0)+,p_gate_time+1(a5)	*get gate time
	move.b	(a0)+,d0
	move.b	d0,p_waon_dly(a5)	*set delay time
	move.b	d0,p_waon_work(a5)	*reset delay work
	bsr	calc_param		*特殊コマンド関係の計算( < d1.w=step time)
	tst.b	p_se_mode(a5)
	bpl	exit_waon01		*se modeの時は発音行為は無し
	st.b	p_waon_flg(a5)		*p_waon_flg on

	tst.w	d3
	beq	all_on?or_not		*tieじゃないとき
					*タイの時
	moveq.l	#0,d1			*d1=init. on/off flg2
	moveq.l	#0,d5			*d5=init. on/off flg
	movea.l	a0,a1
	moveq.l	#0,d2
chk_wn_lp00:
	move.b	(a1)+,d0
	bmi	exit_chk_wn		*終わり
	moveq.l	#0,d6
	lea	p_note(a5),a2
chk_wn_lp01:
	move.b	(a2)+,d4
	bmi	chd0
	cmp.b	d4,d0
	bne	@f
	bset.l	d2,d5			*同じｷｰｺｰﾄﾞがあったらその位置をﾏｰｸ
	bset.l	d6,d1			*同上
@@:
	addq.b	#1,d6
	cmpi.b	#max_note_on,d6
	bcs	chk_wn_lp01
chd0:
	addq.b	#1,d2
	cmpi.b	#max_note_on,d2
	bcs	chk_wn_lp00
exit_chk_wn:
	move.b	p_ch(a5),d4
	bra	waon_all_on		*タイの時はディレイ無視
all_on?or_not:
	move.b	p_ch(a5),d4
	tst.b	p_waon_dly(a5)		*check delay
	beq	waon_all_on2		*ﾃﾞｨﾚｲが０なので一斉にｷｰｵﾝする

	lea	p_note(a5),a2
	move.b	#1,p_waon_num(a5)	*次にキーオンするのは+1番目のノート
	move.b	(a0)+,d0
	move.b	d0,(a2)+
	cmpi.b	#8,d4
	bhi	wkon_midi0
	bsr	fm_kco			*第一声目(FM)
					*規定外のノートを殺す(FMのみ)
	moveq.l	#max_note_on-2,d1
kill_ntnlp:
	move.b	(a0)+,d0
	addq.b	#1,d4
	cmpi.b	#7,d4
	bhi	kill_ntn
	move.b	d0,(a2)+
	bra	next_chkwn
kill_ntn:
	st.b	(a2)+
next_chkwn:
	dbra	d1,kill_ntnlp
	exit_ssk
wkon_midi0:				*第一声目(MIDI)
	sub.b	#9,d4
	bsr	midi_note_on
	rept	max_note_on-1		*１声目以降をワークへ
	move.b	(a0)+,(a2)+
	endm
	exit_ssk

waon_all_on:				*一斉に発音するケース
	* < d1,d5=bit pattern
	* < d4.b=ch(0-24)
	st.b	p_waon_num(a5)		*以後発音の必要無し
	cmpi.b	#8,d4
	bhi	waon_all_on_midi	*MIDIの場合

	move.l	d1,d3		*d3=work側のマーク
	moveq.l	#0,d1
	moveq.l	#0,d2
	move.b	d4,d6		*save
waon_lp01:
	btst.l	d1,d3		*ワーク側のマークを検査
	beq	sww1
	btst.l	d2,d5		*新規側のマークを検査
	beq	sww3
	*両方マークのケース
	addq.w	#1,d1
	addq.w	#1,d2
	addq.w	#1,d4
	cmpi.b	#7,d4
	bls	waon_lp01
	bra	exit_wnlp
sww1:
	btst.l	d2,d5
	bne	sww2
	*両方マーク無しのケース
	cmpi.b	#7,d4
	bhi	exit_wnlp
	move.b	(a0,d2.w),d0
	bmi	exit_wnlp2
	move.b	d0,p_note(a5,d1.w)
	movem.w	d1-d3,-(sp)
	cmp.b	d6,d4
	beq	swn01		*主ﾁｬﾝﾈﾙならﾊﾟﾗﾒｰﾀ設定は不要
	bset.b	d4,p_waon_mark(a5)
	bne	swn01
	move.l	d0,d3		*save d0 into d3
	moveq.l	#0,d0
	move.b	p_pgm(a5),d0	*主チャンネルのパラメータに合わせる
	bmi	@f
	move.b	d4,p_ch(a5)
	bsr	pan_save_fmvset		*set 音色
@@:
	move.l	d3,d0		*get back note number
swn01:
	moveq.l	#0,d3
	bsr	fm_kco
	movem.w	(sp)+,d1-d3
	addq.w	#1,d1
	addq.w	#1,d2
	addq.w	#1,d4
	bra	waon_lp01
sww2:	*ワークにマーク無し/新規にマーク有り
	addq.w	#1,d2
	bra	waon_lp01
sww3:	*ワークにマーク有り/新規にマーク無し
	addq.w	#1,d1
	addq.w	#1,d4
	cmpi.b	#7,d4
	bls	waon_lp01
	bra	exit_wnlp
exit_wnlp2:			*余計なノートを殺す
	tas.b	p_note(a5,d1.w)
	bmi	exit_wnlp
	btst.l	d1,d3
	bne	@f
	opmset	#8,d4		*=fmkey_off	d4
@@:
	addq.w	#1,d4
	cmpi.b	#7,d4
	bhi	exit_wnlp
	addq.w	#1,d1
	bra	exit_wnlp2
exit_wnlp:
	addq.w	#8,a0
	move.b	d6,p_ch(a5)
	exit_ssk

waon_all_on_midi:			*KEY ON (MIDI)
	* < d4.b=ch(0-24)
	sub.b	#9,d4
	move.l	d1,d3		*d3=work側のマーク
	bset.l	#31,d3		*flag
	moveq.l	#0,d1
	moveq.l	#0,d2
waon_lp01_midi:
	btst.l	d1,d3		*ワーク側のマークを検査
	beq	sww1_midi
	btst.l	d2,d5		*新規側のマークを検査
	beq	sww3_midi
	*両方マークのケース
	addq.w	#1,d1
	addq.w	#1,d2
	cmpi.b	#7,d1
	bls	waon_lp01_midi
	bra	exit_wnlp_midi
sww1_midi:
	btst.l	d2,d5
	bne	sww2_midi
	*両方マーク無しのケース
	cmpi.b	#7,d1
	bhi	exit_wnlp_midi
	move.b	(a0,d2.w),d0	*note number
	bmi	exit_wnlp2_midi
	move.b	p_note(a5,d1.w),d6
	bmi	@f
	ori.b	#$90,d4		*いらないノートは殺す
	bsr	m_out_d4
	bsr	m_out_d6
	bset.b	#6,p_seq_flag(a5)	*set key off bit
	bsr	m_out0
@@:
	* < d0.b=note number
	move.l	d1,a1		*save d1-d2 into a1-a2
	move.l	d2,a2
	move.b	d0,p_note(a5,d1.w)
	bclr.l	#31,d3
	bne	@f
	bsr	midi_note_on3	*２回目以降
	move.l	a1,d1		*get back d1-d2 from a1-a2
	move.l	a2,d2
	addq.w	#1,d1
	addq.w	#1,d2
	bra	waon_lp01_midi
@@:				*１回目
	bsr	midi_note_on
	move.l	a1,d1		*get back d1-d2 from a1-a2
	move.l	a2,d2
	addq.w	#1,d1
	addq.w	#1,d2
	bra	waon_lp01_midi
sww2_midi:	*ワークにマーク無し/新規にマーク有り
	addq.w	#1,d2
	bra	waon_lp01_midi
sww3_midi:	*ワークにマーク有り/新規にマーク無し
	addq.w	#1,d1
	cmpi.b	#7,d1
	bls	waon_lp01_midi
	bra	exit_wnlp_midi
exit_wnlp2_midi:		*余計なノートを殺す
	move.b	p_note(a5,d1.w),d0
	bmi	exit_wnlp_midi
	st.b	p_note(a5,d1.w)
	btst.l	d1,d3
	bne	@f
	bsr	m_out_d0
	bset.b	#6,p_seq_flag(a5)	*set key off bit
	bsr	m_out0
@@:
	addq.w	#1,d1
	cmpi.b	#7,d1
	bls	exit_wnlp2_midi
exit_wnlp_midi:
	addq.w	#8,a0
	exit_ssk

exit_waon01:			*se modeのとき
	addq.w	#8,a0
	st.b	p_note(a5)
	exit_ssk

exit_waon02:			*全部同じ音程でしかもタイの時
	addq.w	#8,a0
	exit_ssk

waon_all_on2:			*通常の和音
	* < d4.b=ch(0-24) 
	st.b	p_waon_num(a5)	*以後発音の必要なし
	cmpi.b	#8,d4
	bhi	waon_all_on2_midi

	move.l	d4,d6		*save ch
	move.l	a0,a1
	lea	p_note(a5),a2
wn2ko_lp:
	move.b	(a1)+,d3
	move.b	d3,(a2)+
	bmi	exit_wn2ko	*all end
	cmp.b	d6,d4		*主チャンネルならパラメータ設定は不用
	beq	do_wn2ko
	bset.b	d4,p_waon_mark(a5)
	bne	do_wn2ko
	moveq.l	#0,d0
	move.b	p_pgm(a5),d0	*主チャンネルのパラメータに合わせる
	bmi	@f
	move.b	d4,p_ch(a5)
	bsr	pan_save_fmvset		*set 音色
@@:
do_wn2ko:
	move.l	d3,d0
	moveq.l	#0,d3		*tie off(dummy)
	bsr	fm_kco
	addq.b	#1,d4
	cmpi.b	#7,d4
	bls	wn2ko_lp
	tst.b	d6		*主チャンネルが０の時は$ffを書き込むスペースはないから
	beq	exit_wn2ko
	st.b	(a2)
exit_wn2ko:
	addq.w	#8,a0
	move.b	d6,p_ch(a5)
	exit_ssk

waon_all_on2_midi:
	sub.b	#9,d4		*d4=midi ch
	move.l	a0,a1
	lea	p_note(a5),a2
	moveq.l	#1,d3
	moveq.l	#max_note_on-1,d5
wm2ko_lp:
	move.b	(a1)+,d0
	move.b	d0,(a2)+
	bmi	exit_wm2ko	*all end
	lsr.b	#1,d3
	bcc	@f
	bsr	midi_note_on	*一回目
	subq.b	#1,d5
	bra	wm2ko_lp
@@:
	bsr	midi_note_on3	*二回目以降
wm2ko_next:
	dbra	d5,wm2ko_lp
exit_wm2ko:
	addq.w	#8,a0
	exit_ssk

bend:				*オートベンド
	move.b	(a0)+,p_detune_f+0(a5)
	move.b	(a0)+,p_detune_f+1(a5)		*get fm start
	move.b	(a0)+,p_bend_rng_f+0(a5)
	move.b	(a0)+,p_bend_rng_f+1(a5)	*get fm end
	move.b	(a0)+,p_detune_m+0(a5)
	move.b	(a0)+,p_detune_m+1(a5)		*get midi start
	move.b	(a0)+,p_bend_rng_m+0(a5)
	move.b	(a0)+,p_bend_rng_m+1(a5)	*get midi end
	move.b	(a0)+,p_bend_dly+0(a5)
	move.b	(a0)+,p_bend_dly+1(a5)		*get delay
	move.b	(a0)+,p_bend_sw(a5)		*bend switch on
	tst.w	p_gate_time(a5)
	sne	p_tie_bend(a5)
	bra	next_cmd

skip_zmd:				*スキップコマンド1
*	moveq.l	#0,d0
	MOVEW	(a0)+,d0
	adda.l	d0,a0
	bra	next_cmd

skip_zmd2:				*スキップコマンド2
*	moveq.l	#0,d0
	MOVEW	(a0)+,d0
	suba.l	d0,a0
	bra	next_cmd

modu_hold:
	*d4=pm first time? or not
	*d5=am first time? or not
	*d6=pm hold or not
	*d7=am hol or not
	tst.b	(a0)+
	bmi	mh1
	bne	@f
	bclr.b	#6,p_md_flg(a5)
	bra	mh1
@@:
	bset.b	#6,p_md_flg(a5)
mh1:
	tst.b	(a0)+
	bmi	next_cmd
	bne	@f
	bclr.b	#7,p_md_flg(a5)
	bra	next_cmd
@@:
	tas.b	p_md_flg(a5)
	bra	next_cmd

midi_tie_mode:				*MIDIのタイモードの設定
	tst.b	(a0)+
	beq	@f
	bset.b	#3,p_md_flg(a5)
	bra	next_cmd
@@:
	bclr.b	#3,p_md_flg(a5)
	bra	next_cmd

switch:					*スイッチ '='コマンド
	moveq.l	#0,d0
	tst.w	p_gate_time(a5)
	beq	@f
	moveq.l	#-1,d0
@@:
	move.l	d0,p_tie_pmod(a5)	*4つまとめて設定
	move.b	(a0)+,d6
	ror.b	#1,d6
	bcs	@f
	bclr.b	#1,p_md_flg(a5)
	clr.b	p_pmod_sw(a5)
	clr.w	p_pmod_flg(a5)
	move.w	p_pmod_dly(a5),p_pmod_work(a5)
	bra	gsw1_
@@:
	bsr	calc_pmd
gsw1_:
	ror.b	#1,d6
	bcs	@f
*	bclr.b	#2,p_md_flg(a5)
	clr.b	p_arcc_sw(a5)
	st.b	p_vset_flg(a5)
	clr.b	p_arcc_flg(a5)
	move.w	p_arcc_dly(a5),p_arcc_work(a5)
	bra	gsw2_
@@:
	bsr	calc_amd
gsw2_:
	ror.b	#1,d6
	bcs	@f
*	bclr.b	#0,p_md_flg(a5)	*＝コマンドでは前の状態を保存する
	clr.b	p_bend_sw(a5)	*オフならゼロ(この辺を変えたら下のも変える)
	bra	gsw3_
@@:
	bsr	set_bend_sw
gsw3_:
	ror.b	#1,d6
	scs	p_aftc_sw(a5)
	scc	p_vset_flg(a5)
	bcs	@f
	clr.b	p_aftc_flg(a5)
	bra	next_cmd
@@:
	clr.b	p_waon_mark(a5)
	bra	next_cmd

pmodsw:				*ﾋﾟｯﾁﾓｼﾞｭﾚｰｼｮﾝｽｲｯﾁ
	pea	next_cmd(pc)		*set ret addr.
	tst.w	p_gate_time(a5)
	sne	p_tie_pmod(a5)
	clr.w	p_pmod_flg(a5)
	move.b	(a0)+,p_pmod_sw(a5)
	bne	calc_pmd
	move.w	p_pmod_dly(a5),p_pmod_work(a5)
	tst.b	p_se_mode(a5)		*効果音モード(mask ok)
	beq	exit_pmdsw
	move.b	p_ch(a5),d4
	cmpi.b	#8,d4
	bls	ps0
	sub.b	#9,d4			*MIDIの場合はSEND MOD OFF
	tst.b	p_pmod_mode(a5)
	bpl	@f
	ori.b	#$b0,d4
	bsr	m_out_d4
	bsr	m_out1
	bclr.b	#1,p_md_flg(a5)
	bra	m_out0			*pmod off
@@:
	tst.l	p_port_flg(a5)
	beq	@f
	move.w	p_detune_m(a5),d2
	add.w	p_port_work2(a5),d2	*ﾍﾞﾝﾄﾞ後のﾋﾟｯﾁを保つ
*	clr.w	p_port_work2(a5)
	bra	set_@b_
@@:
	bclr.b	#0,p_md_flg(a5)		*ここで設定するからノートオンの時はノータッチ
	bra	set_@b
ps0:					*FMの場合はピッチパラメータ修正
	moveq.l	#0,d2
	tst.l	p_port_flg(a5)
	beq	set_detune
	move.w	p_port_work2(a5),d2	*ﾍﾞﾝﾄﾞ後のﾋﾟｯﾁを保つ
*	clr.w	p_port_work2(a5)
	bra	set_detune

arccsw:				*ARCCｽｲｯﾁ
	pea	next_cmd(pc)		*set ret addr.
	tst.w	p_gate_time(a5)
	sne	p_tie_arcc(a5)
	clr.b	p_arcc_flg(a5)
	move.b	(a0)+,p_arcc_sw(a5)
	bne	calc_amd
*	bclr.b	#2,p_md_flg(a5)
	move.w	p_arcc_dly(a5),p_arcc_work(a5)
	tst.b	p_se_mode(a5)		*効果音モード(mask ok)
	beq	@f
	move.b	p_ch(a5),d4
	cmpi.b	#8,d4
	scs	p_vset_flg(a5)		*FMならばset
	bcs	@f
	move.b	p_arcc(a5),d1
	move.b	p_arcc_rst(a5),d0
	bmi	@f
	sub.b	#9,d4
	bmi	@f
	ori.b	#$b0,d4
	bsr	m_out_d4
	bsr	m_out_d1
	bsr	arcc_p_chk
	bra	m_out_d0
exit_pmdsw:
@@:
	rts

bendsw:				*ｵｰﾄﾍﾞﾝﾄﾞｽｲｯﾁ
	tst.w	p_gate_time(a5)
	sne	p_tie_bend(a5)
	move.b	(a0)+,p_bend_sw(a5)	*オフならゼロ
	beq	next_cmd
	pea	next_cmd(pc)
set_bend_sw:
	moveq.l	#1,d0
	tst.w	p_bend_rng_m(a5)	*_fでもいいけど
	bpl	@f
	neg.b	d0
@@:
	move.b	d0,p_bend_sw(a5)
	rts

aftcsw:
	tst.w	p_gate_time(a5)
	sne	p_tie_aftc(a5)
	move.b	(a0)+,p_aftc_sw(a5)
	seq	p_vset_flg(a5)
	bne	@f
	clr.b	p_aftc_flg(a5)
	bra	next_cmd
@@:
	clr.b	p_waon_mark(a5)
	bra	next_cmd

after_touch:				*アフタータッチ
	lea	p_aftc_tbl(a5),a1
	moveq.l	#aftc_max-1,d0
@@:
	move.b	(a0)+,(a1)+
	dbra	d0,@b
	st.b	p_aftc_sw(a5)
	tst.w	p_gate_time(a5)
	sne	p_tie_aftc(a5)
	bra	next_cmd

modulation8:				*MIDI/FM 1/8 ピッチモジュレーション
	move.b	(a0)+,p_pmod_omt(a5)
	lea	p_pmod_tbl(a5),a1
	moveq.l	#8-1,d2
@@:
	MOVEW	(a0)+,d1
	bsr	reduce_@mv
	move.w	d1,(a1)+
	dbra	d2,@b
	clr.w	p_pmod_dpt(a5)		*init
	tst.w	p_gate_time(a5)
	sne	p_tie_pmod(a5)
	bsr	calc_pmd2
	bra	next_cmd

arcc_amod8:				*MIDI ARCC/FM AMD 1/8
	move.b	(a0)+,p_amod_omt(a5)
	lea	p_arcc_tbl(a5),a1
	moveq.l	#8-1,d1
@@:
	move.b	(a0)+,(a1)+
	dbra	d1,@b
	clr.b	p_amod_dpt(a5)		*init
	tst.w	p_gate_time(a5)
	sne	p_tie_arcc(a5)
	bsr	calc_amd2
	bra	next_cmd

delay:				*モジュレーションディレイ設定
	MOVEW	(a0)+,d0
	cmpi.w	#-1,d0
	beq	@f
	move.w	d0,p_pmod_dly(a5)
@@:
	MOVEW	(a0)+,d0
	cmpi.w	#-1,d0
	beq	next_cmd
	move.w	d0,p_arcc_dly(a5)
	bra	next_cmd

speed:				*モジュレーションスピード設定
	MOVEW	(a0)+,d0
	add.w	d0,d0		*2倍にする
	beq	@f
	move.w	d0,p_pmod_spd(a5)
	tst.b	p_pmod_sw(a5)
	beq	@f
	bsr	calc_pmd
		tst.w	p_gate_time(a5)		*!2.05
		sne	p_tie_pmod(a5)		*!2.05
@@:
	MOVEW	(a0)+,d0
	add.w	d0,d0		*2倍にする
	beq	next_cmd
	move.w	d0,p_amod_spd(a5)
	tst.b	p_amod_sw(a5)
	beq	next_cmd
	bsr	calc_amd
		tst.w	p_gate_time(a5)		*!2.05
		sne	p_tie_amod(a5)		*!2.05
	bra	next_cmd

wave_form_sel:			*波形タイプセレクト
	move.b	(a0)+,d0
	bmi	get_am_wf
	cmpi.b	#wv_def_max,d0
	bcs	@f
	neg.b	d0
@@:
	move.b	d0,p_pmod_wf(a5)
	move.b	p_ch(a5),d1
	cmpi.b	#8,d1
	bls	@f
	tst.b	p_pmod_mode(a5)
	bpl	@f
	move.b	#1,p_pmod_mode(a5)
@@:
	tst.b	p_pmod_sw(a5)
	beq	get_am_wf
	bsr	calc_pmd
get_am_wf:
	move.b	(a0)+,d0
	bmi	next_cmd
	cmpi.b	#wv_def_max,d0
	bcs	@f
	neg.b	d0
@@:
	move.b	d0,p_amod_wf(a5)
	cmpi.b	#8,d1
	bls	@f
	tst.b	p_arcc_mode(a5)
	bpl	@f
	move.b	#1,p_arcc_mode(a5)
@@:
	tst.b	p_amod_sw(a5)
	beq	next_cmd
	bsr	calc_amd
	bra	next_cmd

reduce_@mv:			*ピッチモジュレーションのデプス変換
	* < d1.w=@m value
	cmpi.b	#8,p_ch(a5)
	bls	exit_rmv
	tst.b	p_pmod_mode(a5)
	beq	need_conv
	bpl	exit_rmv
	tst.w	d1
	bpl	@f
	neg.w	d1
@@:
	cmpi.w	#127,d1
	bls	exit_rmv
	moveq.l	#127,d1
exit_rmv:
	rts
need_conv
	ext.l	d1
	muls	#683,d1		*d1=683*d1
	asr.l	#6,d1		*d1=d1/64 d1=-8192～8191
ndc0:
	cmpi.w	#-8192,d1
	bge	@f
	move.w	#-8192,d1
	bra	ndc1
@@:
	cmpi.w	#8191,d1
	ble	@f
	move.w	#8191,d1
@@:
ndc1:
	rts

soft_pmd:			*ﾋﾟｯﾁﾓｼﾞｭﾚｰｼｮﾝのﾃﾞﾌﾟｽ設定
	tst.w	p_gate_time(a5)
	sne	p_tie_pmod(a5)
	MOVEW	(a0)+,d1
	bsr	reduce_@mv
@@:
	move.w	d1,p_pmod_tbl(a5)
	move.w	d1,p_pmod_dpt(a5)
*	bne	sft_lf0		*@m @m0はスイッチOFFコマンドへコンパイルされるから
*	clr.b	p_pmod_sw(a5)	*いらない
*	bra	next_cmd
sft_lf0:
	pea	next_cmd(pc)

calc_pmd:			*パラメータ計算(LFO)
	move.w	p_pmod_dpt(a5),d0
	beq	calc_pmd2
	smi	p_pmod_sw(a5)
	bmi	@f
	addq.b	#1,p_pmod_sw(a5)
@@:
	ext.l	d0
	clr.w	p_pmod_work(a5)
	move.w	p_pmod_spd(a5),d1
	cmpi.b	#2,p_pmod_wf(a5)
	bne	@f
	lsr.w	#1,d1
@@:
	divs	d1,d0
	move.w	d0,p_pmod_step(a5)
	tst.l	d0
	bpl	@f
	neg.l	d0
@@:
	clr.w	d0
	swap	d0
	lsl.l	#8,d0
	divu	d1,d0
	swap	d0
	tst.w	d0
	beq	clclf1
	add.l	#$0001_0000,d0
clclf1:
	swap	d0
	move.b	d0,p_pmod_rvs(a5)
	bclr.b	#4,p_md_flg(a5)
	rts

calc_pmd2:			*パラメータ計算(LFO)その２
	* X d0,d1,d2,d4,d5  a1,a2
	move.l	a0,d4			*save a0 into d4
	lea	p_pmod_tbl(a5),a0
	lea	p_mstep_tbl(a5),a1
	lea	p_mrvs_tbl(a5),a2
	tst.w	p_pmod_flg(a5)
	bne	@f
	moveq.l	#0,d1			*初めが省略の時のために初期化する
	move.w	d1,p_pmod_step2(a5)	*init p_pmod_step2
	move.l	d1,p_pmod_work(a5)	*init p_pmod_work～2
	move.b	d1,p_pmod_rvs(a5)	*init p_pmod_rvs
@@:
	move.w	p_pmod_spd(a5),d5
	cmpi.b	#2,p_pmod_wf(a5)
	bne	@f
	lsr.w	#1,d5
@@:
	moveq.l	#7,d2		*loop counter
clclf2_lp:
	move.w	(a0)+,d0	*get @m value
	ext.l	d0
	divs	d5,d0
	move.w	d0,(a1)+	*save step
	tst.l	d0
	bpl	@f
	neg.l	d0
@@:
	clr.w	d0
	swap	d0
	lsl.l	#8,d0
	divu	d5,d0
	swap	d0
	tst.w	d0
	beq	@f
	add.l	#$0001_0000,d0
@@:
	swap	d0
	move.b	d0,(a2)+	*save rvs
clclf2_2:
	dbra	d2,clclf2_lp
	st.b	p_pmod_sw(a5)	*switch on
	move.l	d4,a0
	bclr.b	#4,p_md_flg(a5)
	rts

asgn_arcc:			*ARCC セット
	move.b	(a0)+,d0
	bmi	@f
	move.b	d0,p_arcc(a5)
@@:
	move.b	(a0)+,d0
	cmpi.b	#-1,d0
	beq	@f
	move.b	d0,p_arcc_rst(a5)
@@:
	move.b	(a0)+,d0
	bmi	next_cmd
	move.b	d0,p_arcc_def(a5)
	bra	next_cmd

soft_amd:			*AMのﾃﾞﾌﾟｽ設定(92/01/25)
	tst.w	p_gate_time(a5)
	sne	p_tie_amod(a5)
	move.b	(a0)+,d0
	move.b	d0,p_amod_tbl(a5)
	move.b	d0,p_amod_dpt(a5)
*	bne	sft_am0		*@a @a0はスイッチOFFコマンドへコンパイルされるから
*	clr.b	p_amod_sw(a5)	*いらない
*	bra	next_cmd
sft_am0:
	pea	next_cmd(pc)

calc_amd:			*パラメータ計算(AMD)
	move.b	p_amod_dpt(a5),d0
	beq	calc_amd2
	smi	p_amod_sw(a5)
	bmi	@f
	addq.b	#1,p_amod_sw(a5)
@@:
	ext.w	d0
	ext.l	d0
	clr.w	p_amod_work(a5)
	move.w	p_amod_spd(a5),d1
	move.b	p_amod_wf(a5),d2
	beq	@f
	cmpi.b	#3,d2
	bne	ca1
@@:
	add.w	d1,d1		*ノコギリ波オンリー
ca1:
	divs	d1,d0
	move.b	d0,p_amod_step(a5)
	tst.l	d0
	bpl	@f
	neg.l	d0
@@:
	clr.w	d0
	swap	d0
	lsl.l	#8,d0
	divu	d1,d0
	swap	d0
	tst.w	d0
	beq	clcam1
	add.l	#$0001_0000,d0
clcam1:
	swap	d0
	move.b	d0,p_amod_rvs(a5)
	bclr.b	#5,p_md_flg(a5)
	rts

calc_amd2:			*パラメータ計算(AMD)その２
	* X d0,d1,d2,d4,d5  a1,a2
	move.l	a0,d4
	lea	p_amod_tbl(a5),a0
	lea	p_astep_tbl(a5),a1
	lea	p_arvs_tbl(a5),a2
	tst.b	p_amod_flg(a5)
	bne	@f
	moveq.l	#0,d1			*初めが省略の時のために初期化する
	move.b	d1,p_amod_step2(a5)	*init p_amod_step2
	move.l	d1,p_amod_work(a5)	*init p_amod_work～3
	move.b	d1,p_amod_rvs(a5)	*init p_amod_rvs
@@:
	move.w	p_amod_spd(a5),d5
	move.b	p_amod_wf(a5),d2
	beq	@f
	cmpi.b	#3,d2
	bne	ca2
@@:
	add.w	d5,d5		*ノコギリ波オンリー
ca2:
	moveq.l	#7,d2		*loop counter
clcam2_lp:
	move.b	(a0)+,d0
	ext.w	d0
	ext.l	d0
	divs	d5,d0
	move.b	d0,(a1)+
	tst.l	d0
	bpl	@f
	neg.l	d0
@@:
	clr.w	d0
	swap	d0
	lsl.l	#8,d0
	divu	d5,d0
	swap	d0
	tst.w	d0
	beq	@f
	add.l	#$0001_0000,d0
@@:
	swap	d0
	move.b	d0,(a2)+
clcam2_2:
	dbra	d2,clcam2_lp
	st.b	p_amod_sw(a5)	*switch on
	move.l	d4,a0
	bclr.b	#5,p_md_flg(a5)
	rts

fo_mode:
	move.b	trace_mode(pc),d0	*d0に意味無し
	bne	fo_mode__
	clr.b	1+p_marker(a5)		*clear flg
	move.b	(a0)+,d0
	bmi	fi_mode
	bne	@f
	move.b	d0,p_fo_mode(a5)	*fade out mode off
	bra	next_cmd
@@:					*fade out
	move.b	d0,p_fo_spd(a5)		*speed set
	st.b	p_fo_mode(a5)
	move.b	#127,p_fo_lvl(a5)
	bra	next_cmd
fi_mode:				*fade in
	neg.b	d0
	move.b	d0,p_fo_spd(a5)		*speed set
	move.b	#1,p_fo_mode(a5)
	clr.b	p_fo_lvl(a5)
	cmpi.b	#8,p_ch(a5)
	bcc	next_cmd
	move.b	#62,p_fo_lvl(a5)
	bra	next_cmd
fo_mode__:				*func$19のケース
	addq.w	#1,a0
	bra	next_cmd

n_off:
	tst.b	p_se_mode(a5)		*効果音モード(mask ok)
	beq	next_cmd
	moveq.l	#15,d1
	moveq.l	#0,d2
	bsr	opmset
	tst.b	p_marker(a5)		*効果音モード時の処理ならメモは不用
	bmi	next_cmd
	lea	noise_mode(pc),a1
	move.b	d2,(a1)
	bra	next_cmd

noise_set:
	move.b	(a0)+,d2
	tst.b	p_se_mode(a5)		*効果音モード(mask ok)
	beq	next_cmd
	moveq.l	#15,d1
	bsr	opmset
	tst.b	p_marker(a5)		*効果音モード時の処理ならメモは不用
	bmi	next_cmd
	lea	noise_mode(pc),a1
	move.b	d2,(a1)
	bra	next_cmd

dumper:				*ダンパーペダル
	move.b	(a0)+,d1
	sne.b	p_dumper(a5)	*work set
	tst.b	p_se_mode(a5)	*効果音モード(mask ok)
	beq	next_cmd
	move.b	p_ch(a5),d4
	cmpi.b	#8,d4
	bls	dmp_kof?
	add.b	#$b0-9,d4
	bsr	m_out_d4
	bsr	m_out64
	bsr	m_out_d1		*write dumper value
	bra	next_cmd
dmp_kof?:
	tst.b	d1
	bne	next_cmd
	bsr	key_off
	clr.b	p_dmp_n(a5)
	bra	next_cmd

rythm_mode:
	move.b	(a0)+,p_non_off(a5)
	bra	next_cmd

bend_range:			*ベンドレンジ切り換え
	move.b	trace_mode(pc),d0	*d0に意味無し
	bne	bendrg__
	move.b	p_ch(a5),d4
	subi.b	#9,d4
	bmi	bendrg__
	ori.b	#$b0,d4
	bsr	m_out_d4
	m_out_	#$64		*RPN L
	bsr	m_out0
*	bsr	m_out_d4
	m_out_	#$65		*RPN H
	bsr	m_out0
*	bsr	m_out_d4	*PITCH BEND RANGEを１オクターブにする
	m_out_	#$06		*data entry
	move.b	(a0)+,d0
	move.b	d0,p_@b_range(a5)
	bsr	m_out_d0
	bra	next_cmd
bendrg__:			*func$19のケース
	addq.w	#1,a0
	bra	next_cmd

send_ex:			*ﾛｰﾗﾝﾄﾞﾌｫｰﾏｯﾄ対応ｴｸｽｸﾙｰｼﾌﾞﾃﾞｰﾀ転送
	move.b	trace_mode(pc),d0	*d0に意味無し
	bne	send_ex__
	m_out_	#$f0
	m_out_	p_maker(a5)	*Maker ID
	m_out_	p_device(a5)	*Device ID
	m_out_	p_module(a5)	*Model ID
	m_out_	#$12		*Command ID
send_exlp:
	move.b	(a0)+,d0
	bsr	m_out_d0
	cmpi.b	#$ff,(a0)
	bne	send_exlp
	addq.w	#1,a0		*skip	$FF
	m_out_	#$f7		*end of exclusive
	bra	next_cmd
send_ex__:			*func$19のケース
	addq.w	#1,a0
	cmpi.b	#$ff,(a0)
	bne	send_ex__
	addq.w	#1,a0		*skip	$FF
	bra	next_cmd

ID_set:				*ID SET
	move.b	(a0)+,p_maker(a5)
	move.b	(a0)+,p_device(a5)
	move.b	(a0)+,p_module(a5)
	bra	next_cmd

send_data:			*生MIDIデータ転送
	move.b	trace_mode(pc),d0	*d0に意味無し
	bne	send_data__
	MOVEW	(a0)+,d4	*d4=number of data
	bsr	m_outa0		*send first data
	subq.w	#1,d4
	beq	next_cmd
	subq.w	#1,d4		*for dbra count
senddt_lp:
	bsr	m_outa0
	dbra	d4,senddt_lp
	bra	next_cmd
send_data__:			*func$19のケース
	MOVEW	(a0)+,d0	*d4=number of data
	add.w	d0,a0
	bra	next_cmd

frq_chg:			*@F 周波数切り換え
	move.b	(a0)+,p_frq(a5)
	bra	next_cmd

NRPN:				*@Yコマンド(SET DATA TO NRPN)
	move.b	trace_mode(pc),d0	*d0に意味無し
	bne	NRPN__
	move.b	p_ch(a5),d4
	subi.b	#9,d4
	bmi	NRPN__
	ori.b	#$b0,d4		*ｺﾝﾄﾛｰﾙﾁｪﾝｼﾞ
	bsr	m_out_d4
	m_out_	#$63		*NRPN H
	bsr	m_outa0

*	bsr	m_out_d4
	m_out_	#$62		*NRPN L
	bsr	m_outa0

*	bsr	m_out_d4
	m_out_	#$06
	bsr	m_outa0		*H
	move.b	(a0)+,d0
	bmi	next_cmd	*L省略時
*	bsr	m_out_d4
	m_out_	#$26
	bsr	m_out_d0	*L
	bra	next_cmd
NRPN__:				*func$19のケース
	addq.w	#4,a0
	bra	next_cmd

bank_select:			*Iコマンド(bank select) ctrl chg 0,32
	move.b	trace_mode(pc),d0	*d0に意味無し
	bne	bnksl__
	move.b	p_ch(a5),d4
	subi.b	#9,d4
	bmi	bnksl__
	ori.b	#$b0,d4		*ctrl chg
	bsr	m_out_d4
	bsr	m_out0
	move.b	(a0)+,d0
	move.b	d0,p_bank_msb(a5)
	bsr	m_out_d0	*higher byte
*	bsr	m_out_d4
	m_out_	#32
	move.b	(a0)+,d0
	move.b	d0,p_bank_lsb(a5)
	bsr	m_out_d0	*lower byte
	bra	next_cmd
bnksl__:			*func$19のケース
	addq.w	#2,a0
	bra	next_cmd

effect_ctrl:			*@E effect control
	move.b	trace_mode(pc),d0	*d0に意味無し
	bne	effctrl__
	move.l	p_maker(a5),d0
	andi.l	#$ff00_ff00,d0
	cmpi.l	#$4100_1600,d0
	beq	e_mt32		*MT32
				*コントロールチェンジでコントロールする場合
	move.b	p_ch(a5),d1
	sub.b	#9,d1
	bmi	effctrl__
	ori.b	#$b0,d1		*d1=ctrl chg message
	move.b	(a0)+,d0
	bmi	@f		*minusだった
	move.b	d0,p_effect1(a5)
	bsr	m_out_d1
	m_out_	#$5b		*reverb
	bsr	m_out_d0
@@:
	move.b	(a0)+,d0
	bmi	@f		*minusだった
	move.b	d0,p_effect3(a5)
	bsr	m_out_d1
	m_out_	#$5d		*chorus
	bsr	m_out_d0
@@:
	addq.w	#1,a0		*３つ目のﾊﾟﾗﾒｰﾀは未定義
	bra	next_cmd

e_mt32:
	move.l	#$0003_00_00,d0
	move.b	(a0)+,d0	*get part number
	bmi	jump_plus2
	subq.b	#1,d0		*0-7
	bmi	jump_plus2
	cmpi.b	#7,d0
	bls	@f
	move.b	#$10,d0		*8以上はrythm partとみなす
@@:
	lsl.w	#4,d0		*16倍
	addq.b	#6,d0		*reverb switch offset

	m_out_	#$f0		*exc
	m_out_	#$41		*maker
	m_out_	p_device(a5)	*device
	m_out_	#$16		*module
	m_out_	#$12		*command
	moveq.l	#0,d2		*clr chk_sum
	swap	d0
	bsr	m_out_d0
	add.b	d0,d2
	rept	2
	rol.l	#8,d0
	bsr	m_out_d0
	add.b	d0,d2
	endm
	move.b	(a0)+,d1
	andi.b	#1,d1		*switch value
	bsr	m_out_d1
	add.b	d1,d2
	andi.b	#$7f,d2
	moveq.l	#$80,d0
	sub.b	d2,d0
	andi.b	#$7f,d0
	bsr	m_out_d0	*check sum
	addq.w	#1,a0		*3つ目のﾊﾟﾗﾒｰﾀはMTには関係無し
	m_out_	#$f7		*end of EXC
	bra	next_cmd

effctrl__:			*楽器に出力せず
	addq.w	#3,a0
	bra	next_cmd

poke:				*? コマンド
	move.b	trace_mode(pc),d0
	beq	@f
	addq.w	#2,a0
	bra	next_cmd
@@:
	move.b	(a0)+,d0
	move.b	(a0)+,(a5,d0.w)
	bra	next_cmd

poke_up:				*? コマンド 相対＋
	move.b	trace_mode(pc),d0
	beq	@f
	addq.w	#2,a0
	bra	next_cmd
@@:
	move.b	(a0)+,d0
	move.b	(a0)+,d1
	add.b	d1,(a5,d0.w)
	bra	next_cmd

poke_dwn:				*? コマンド 相対＋
	move.b	trace_mode(pc),d0
	beq	@f
	addq.w	#2,a0
	bra	next_cmd
@@:
	move.b	(a0)+,d0
	move.b	(a0)+,d1
	sub.b	d1,(a5,d0.w)
	bra	next_cmd

set_m_mode:			*モジュレーションモード設定
	move.b	(a0)+,d0	*d0=0～2
	bmi	@f
	subq.b	#1,d0		*d0=-1～+1
	move.b	d0,p_pmod_mode(a5)
	tst.b	p_pmod_sw(a5)
	beq	@f
	bsr	calc_pmd
@@:
	move.b	(a0)+,d0	*d0=0～127
	bmi	next_cmd
	subq.b	#1,d0		*d0=-1～
	move.b	d0,p_arcc_mode(a5)
	tst.b	p_amod_sw(a5)
	beq	next_cmd
	bsr	calc_amd
	bra	next_cmd

waiting:			*Wコマンド(同期待ち)
	move.b	trace_mode(pc),d0
	bne	next_cmd
	moveq.l	#1,d0
	lea	p_sync_wk(a5),a1
	tst.b	(a1)		*同期待ちにするかどうかの決断
	bne	@f
	move.b	d0,(a1)		*同期待ちにせず初期状態に戻してリターン
	bra	next_cmd
@@:
	move.b	#$7f,p_not_empty(a5)
	move.w	d0,(a5)
	st.b	(a1)		*wainting
	bra	exit_ssk

send_sync:			*Wコマンド(同期送信)
	move.b	(a0)+,d0
	lea	seq_wk_tbl(pc),a1
	tst.b	trace_mode-seq_wk_tbl(a1)
	bne	next_cmd
	lsl.w	#wk_size2,d0
	move.l	(a1),a1
	adda.w	d0,a1
	moveq.l	#0,d0
	move.b	p_not_empty(a1),d1
	bne	@f
	move.b	d0,p_sync_wk(a1)
	move.b	d0,p_not_empty(a1)
	bra	next_cmd
@@:
	cmpi.b	#$7f,d1
	bne	next_cmd
	move.b	d0,p_not_empty(a1)
	bra	next_cmd

forceplay:			*Jコマンド
*	moveq.l	#0,d0
	move.b	(a0)+,d0
	lea	seq_wk_tbl(pc),a1
	tst.b	trace_mode-seq_wk_tbl(a1)
	bne	next_cmd
	movea.l	trk_po_tbl(pc),a2
	lsl.w	#2,d0
	adda.w	d0,a2
	lsl.w	#wk_size2-2,d0
	move.l	(a1),a1
	adda.w	d0,a1
	tst.b	p_not_empty(a1)	*死んでるトラックは演奏不可
	bmi	next_cmd
	move.l	(a2),p_data_pointer(a1)
	move.w	#1,(a1)
	clr.b	p_not_empty(a1)
	bra	next_cmd

fade_all:
	move.b	(a0)+,d2
	lea	exit_mfo_(pc),a1
	tst.b	jump_flg2-exit_mfo_(a1)
	bmi	next_cmd
	btst.b	#5,p_seq_flag(a5)
	bne	next_cmd
	ext.w	d2
	ext.l	d2
	move.w	(a1),d0
	move.w	#RTS,(a1)
	movem.l	d0/a0-a1/a5,-(sp)
	bsr	fo__
	movem.l	(sp)+,d0/a0-a1/a5
	move.w	d0,(a1)
	bra	next_cmd

neiro_com:			*音色切り換え
	move.b	d0,d5		*save command code
	move.b	(a0)+,d0
	subq.b	#1,d0
	move.b	d0,p_pgm(a5)
	move.b	p_ch(a5),d4
	cmpi.b	#8,d4
	beq	next_cmd	*adpcmは無視
	bhi	midi_neiro
	clr.b	p_waon_mark(a5)
	cmpi.b	#($a1*2.and.$ff),d5
	beq	mx_conpach
	tst.b	p_se_mode(a5)	*効果音モードなら音色切り替えしない
	beq	@f
	bsr	fmvset		*OPMDRV.Xコンパチ
	bra	next_cmd
@@:
	movea.l	neiro(pc),a1
	mulu	#55,d0
	adda.l	d0,a1
	bsr	swk_copy	*アウトプットレベル等のワークへのコピー
	bra	next_cmd
mx_conpach:			*MXDRV.Xコンパチ
	tst.b	p_se_mode(a5)	*効果音モードなら音色切り替えしない
	beq	@f
	bsr	mx_fmvset	*音色設定ルーチンへ
	bra	next_cmd
@@:
	movea.l	neiro(pc),a1
	mulu	#55,d0
	adda.l	d0,a1
	bsr	swk_copy_	*アウトプットレベル等のワークへのコピー(PAN保存)
	bra	next_cmd

midi_neiro:
	tst.b	p_se_mode(a5)	*効果音モードなら音色切り替えしない
	beq	next_cmd
	add.b	#$c0-9,d4
	bsr	m_out_d4	*send pgm chg
	bsr	m_out_d0
	bra	next_cmd

init_rr:
	*X d1,d2
	moveq.l	#$e0,d1
	moveq.l	#$ff,d2
	or.b	p_ch(a5),d1
	bsr	opmset
	addq.b	#8,d1
	bsr	opmset
	addq.b	#8,d1
	bsr	opmset
	addq.b	#8,d1
	bra	opmset

pan_save_fmvset:		*ＦＭ音色のセット
	* < d0.l=sound number(0～199)
	* < a5=seq_wk_tbl n
	* - all
	movem.l	d0-d2/a1,-(sp)

	bsr	init_rr		*音色切り換え時にプチといわせない処理

	movea.l	neiro(pc),a1
	mulu	#55,d0
	adda.w	d0,a1
	bsr	swk_copy_	*アウトプットレベル等のワークへのコピー(PAN保存)

	move.b	p_vol(a5),d0
	move.b	d0,-(sp)
	bsr	chk_vol_big
	move.b	d0,p_vol(a5)	*だます
	move.b	p_ch(a5),d0

	moveq.l	#$20,d1		*LR/AF
	or.b	d0,d1
	move.b	p_pan(a5),d2	*pan
	ror.b	#2,d2
	or.b	(a1),d2		*AF
	bsr	opmset
	bra	set_H_lfo

mx_fmvset:			*ＦＭ音色のセット
	* < d0.l=sound number(0～199)
	* < a5=seq_wk_tbl n
	* - all
	movem.l	d0-d2/a1,-(sp)

	bsr	init_rr		*音色切り換え時にプチといわせない処理

	movea.l	neiro(pc),a1
	mulu	#55,d0
	adda.w	d0,a1
	bsr	swk_copy_	*アウトプットレベル等のワークへのコピー(PAN保存)

	move.b	p_vol(a5),d0
	move.b	d0,-(sp)
	bsr	chk_vol_big
	move.b	d0,p_vol(a5)	*だます
	move.b	p_ch(a5),d0

	moveq.l	#$20,d1		*LR/AF
	or.b	d0,d1
	move.b	p_pan(a5),d2	*pan
	ror.b	#2,d2
	or.b	(a1),d2		*AF
	bsr	opmset
	bra	OP_parameters

fmvset:				*ＦＭ音色のセット
	* < d0.l=sound number(0～199)
	* < a5=seq_wk_tbl n
	* - all
	movem.l	d0-d2/a1,-(sp)

	bsr	init_rr		*音色切り換え時にプチといわせない処理

	movea.l	neiro(pc),a1
	mulu	#55,d0
	adda.w	d0,a1
	bsr	swk_copy	*アウトプットレベル等のワークへのコピー

	move.b	p_vol(a5),d0
	move.b	d0,-(sp)
	bsr	chk_vol_big
	move.b	d0,p_vol(a5)	*だます
	move.b	p_ch(a5),d0

	moveq.l	#$20,d1		*LR/AF
	or.b	d0,d1
	move.b	$09(a1),d2	*pan
	ror.b	#2,d2
	or.b	(a1),d2		*AF
	bsr	opmset
set_H_lfo:
	add.b	#$18,d1		*$38+n
	move.b	$07(a1),d2	*PMS
	lsl.b	#4,d2
	or.b	$08(a1),d2	*AMS
	bsr	opmset

	moveq.l	#$18,d1
	move.b	$04(a1),d2	*LFRQ
	bsr	opmset

	moveq.l	#$19,d1
	move.b	$05(a1),d2	*PMD
	tas.b	d2
	bsr	opmset
	move.b	$06(a1),d2	*AMD
	andi.b	#$7f,d2
	bsr	opmset

	move.b	$02(a1),d2	*WF
	move.b	OPMCTRL.w,d1
	andi.b	#$fc,d1
	andi.b	#$03,d2
	or.b	d1,d2
	moveq.l	#$1b,d1
	move.b	d2,OPMCTRL.w
	bsr	opmset
OP_parameters:
*-----------------------------------
	move.b	p_cf(a5),d1
	swap	d1
	move.w	#$40,d1		*OP1
	or.b	d0,d1
	move.b	$13(a1),d2	*DT1
	lsl.b	#4,d2
	or.b	$12(a1),d2	*MUL
	bsr	opmset
				*OP3
	addq.b	#8,d1		*$48+n
	move.b	$29(a1),d2	*DT1
	lsl.b	#4,d2
	or.b	$28(a1),d2	*MUL
	bsr	opmset
				*OP2
	addq.b	#8,d1		*$50+n
	move.b	$1e(a1),d2	*DT1
	lsl.b	#4,d2
	or.b	$1d(a1),d2	*MUL
	bsr	opmset
				*OP4
	addq.b	#8,d1		*$58+n
	move.b	$34(a1),d2	*DT1
	lsl.b	#4,d2
	or.b	$33(a1),d2	*MUL
	bsr	opmset
*-----------------------------------
				*OP1
	addq.b	#8,d1		*$60+n
	move.b	$10(a1),d2	*TL
	btst.l	#0+16,d1
	beq	@f
	add.b	p_vol(a5),d2
	bpl	@f
	moveq.l	#127,d2
@@:
	bsr	opmset
				*OP3
	addq.b	#8,d1		*$68+n
	move.b	$26(a1),d2	*TL
	btst.l	#2+16,d1
	beq	@f
	add.b	p_vol(a5),d2
	bpl	@f
	moveq.l	#127,d2
@@:
	bsr	opmset
				*OP2
	addq.b	#8,d1		*$70+n
	move.b	$1b(a1),d2	*TL
	btst.l	#1+16,d1
	beq	@f
	add.b	p_vol(a5),d2
	bpl	@f
	moveq.l	#127,d2
@@:
	bsr	opmset
				*OP4
	addq.b	#8,d1		*$78+n
	move.b	$31(a1),d2	*TL
*	btst.l	#3+16,d1
*	beq	@f
	add.b	p_vol(a5),d2
	bpl	@f
	moveq.l	#127,d2
@@:
	bsr	opmset
*-----------------------------------
				*OP1
	addq.b	#8,d1		*$80+n
	move.b	$11(a1),d2	*KS
	ror.b	#2,d2
	or.b	$0b(a1),d2	*AR
	bsr	opmset
				*OP3
	addq.b	#8,d1		*$88+n
	move.b	$27(a1),d2	*KS
	ror.b	#2,d2
	or.b	$21(a1),d2	*AR
	bsr	opmset
				*OP2
	addq.b	#8,d1		*$90+n
	move.b	$1c(a1),d2	*KS
	ror.b	#2,d2
	or.b	$16(a1),d2	*AR
	bsr	opmset
				*OP4
	addq.b	#8,d1		*$98+n
	move.b	$32(a1),d2	*KS
	ror.b	#2,d2
	or.b	$2c(a1),d2	*AR
	bsr	opmset
*-----------------------------------
				*OP1
	addq.b	#8,d1		*$a0+n
	move.b	$15(a1),d2	*AMS
	ror.b	#1,d2
	or.b	$0c(a1),d2	*D1R
	bsr	opmset
				*OP3
	addq.b	#8,d1		*$a8+n
	move.b	$2b(a1),d2	*AMS
	ror.b	#1,d2
	or.b	$22(a1),d2	*D1R
	bsr	opmset
				*OP2
	addq.b	#8,d1		*$b0+n
	move.b	$20(a1),d2	*AMS
	ror.b	#1,d2
	or.b	$17(a1),d2	*D1R
	bsr	opmset
				*OP4
	addq.b	#8,d1		*$b8+n
	move.b	$36(a1),d2	*AMS
	ror.b	#1,d2
	or.b	$2d(a1),d2	*D1R
	bsr	opmset
*-----------------------------------
				*OP1
	addq.b	#8,d1		*$c0+n
	move.b	$14(a1),d2	*DT2
	ror.b	#2,d2
	or.b	$0d(a1),d2	*D2R
	bsr	opmset
				*OP3
	addq.b	#8,d1		*$c8+n
	move.b	$2a(a1),d2	*DT2
	ror.b	#2,d2
	or.b	$23(a1),d2	*D2R
	bsr	opmset
				*OP2
	addq.b	#8,d1		*$d0+n
	move.b	$1f(a1),d2	*DT2
	ror.b	#2,d2
	or.b	$18(a1),d2	*D2R
	bsr	opmset
				*OP4
	addq.b	#8,d1		*$d8+n
	move.b	$35(a1),d2	*DT2
	ror.b	#2,d2
	or.b	$2e(a1),d2	*D2R
	bsr	opmset
*-----------------------------------
				*OP1
	addq.b	#8,d1		*$e0+n
	move.b	$0f(a1),d2	*D1L
	lsl.b	#4,d2
	or.b	$0e(a1),d2	*RR
	bsr	opmset
				*OP3
	addq.b	#8,d1		*$e8+n
	move.b	$25(a1),d2	*D1L
	lsl.b	#4,d2
	or.b	$24(a1),d2	*RR
	bsr	opmset
				*OP2
	addq.b	#8,d1		*$f0+n
	move.b	$1a(a1),d2	*D1L
	lsl.b	#4,d2
	or.b	$19(a1),d2	*RR
	bsr	opmset
				*OP4
	addq.b	#8,d1		*$f8+n
	move.b	$30(a1),d2	*D1L
	lsl.b	#4,d2
	or.b	$2f(a1),d2	*RR
	bsr	opmset
*-----------------------------------
	move.b	(sp)+,p_vol(a5)
	movem.l	(sp)+,d0-d2/a1
	rts

swk_copy:			*あとでボリューム変更等で使うので音色数列のなかから
	* X d0			*必要なパラメータをワークへ待避する
	moveq.l	#0,d0
	move.b	$09(a1),d0
	move.b	d0,p_pan(a5)
	move.b	_pan_tbl_(pc,d0.w),p_pan2(a5)
swk_copy_:
	move.b	$10(a1),p_ol1(a5)
	move.b	$1b(a1),p_ol2(a5)
	move.b	$26(a1),p_ol3(a5)
	move.b	$31(a1),p_ol4(a5)
	move.b	(a1),d0
	andi.b	#%00111111,d0
	move.b	d0,p_af(a5)		*get AF
	andi.w	#7,d0			*get algorithm
	move.b	carrier_tbl(pc,d0.w),d0
	move.b	d0,p_cf(a5)
	move.b	d0,p_arcc(a5)
	move.b	1(a1),d0
	lsl.b	#3,d0
	move.b	d0,p_om(a5)
	move.b	$03(a1),p_sync(a5)	*sync switch
	rts

carrier_tbl:			*アルゴリズムに対応するキャリアの位置
	dc.b	%1000		*AL0
	dc.b	%1000		*AL1
	dc.b	%1000		*AL2
	dc.b	%1000		*AL3
	dc.b	%1010		*AL4
	dc.b	%1110		*AL5
	dc.b	%1110		*AL6
	dc.b	%1111		*AL7

_pan_tbl_:	dc.b	64,0,127,64

opmd_y2_ope:			*OPMDのＹコマンドエミュレーション
	* < a5=seq_wk_tbl n
	* < a0=command line
	nop			*[@]でpatchがあたる
	nop
*	moveq.l	#0,d0
	MOVEW	(a0)+,d0	*note number
	tst.b	p_fo_mode(a5)
	bne	next_cmd
	tst.b	p_se_mode(a5)
	bpl	next_cmd
	lsl.l	#3,d0		*8倍
	move.l	adpcm_tbl(pc),d2
	beq	next_cmd
	move.l	d2,a2
	adda.l	d0,a2
	move.l	(a2)+,d0
	beq	next_cmd
	movea.l	d0,a1		*adpcm data address
	move.l	(a2)+,d2	*size
	beq	next_cmd
	move.b	se_mode(pc),d0	*d0に意味なし
	bne	next_cmd	*se modeで演奏中ならexit
	move.w	frq(pc),d1	*y3をd1.bへ
	move.l	a0,d6
	bsr	adpcmout
	move.l	d6,a0
	bra	next_cmd

opmd_y3_ope:
	lea	y3(pc),a1
	move.b	(a0)+,(a1)
	bra	next_cmd
opmd_y13_ope:
	lea	frq(pc),a1
	move.b	(a0)+,(a1)
	bra	next_cmd
opmd_y14_ope:			*pc8!
	lea	se_mode(pc),a1
	move.b	(a0)+,(a1)
	bra	next_cmd

PCM8KON:			*PCM8モード用 NORMAL ADPCMOUT
	move.l	$580.w,a2	*IOCS $60
	jmp	(a2)

PCM8KOFF:			*PCM8モード用のキーオフ処理
	moveq.l	#0,d0
	or.b	p_extra_ch(a5),d0
	moveq.l	#0,d2
	trap	#2
	rts

PCM8_keyon:			*PCM8の場合のADPCMキーオン処理(volume考慮)
	moveq.l	#0,d1
	move.b	p_pgm(a5),d1
	bmi	@f
	lsl.w	#7,d1
	add.w	d1,d0
@@:
	move.l	adpcm_tbl(pc),d2
	beq	exit_ssk
	move.l	d2,a2
	lsl.l	#3,d0		*8倍
	adda.l	d0,a2
*	moveq.l	#0,d0
*	or.b	p_extra_ch(a5),d0
*	moveq.l	#0,d2
*	trap	#2		*mute
	move.l	(a2)+,d2	*address
	beq	exit_ssk
	movea.l	d2,a1		*data address
	move.b	p_vol(a5),d0
	bsr	chk_vol_big
	moveq.l	#127,d1		*volume operation
	sub.b	d0,d1
	lsr.b	#3,d1		*0～f
	swap	d1
	move.w	p_frq(a5),d1
	move.b	p_pan(a5),d1
	move.l	(a2),d2		*size
	beq	exit_ssk
	move.l	a0,p_data_pointer(a5)	*次回に備える
	moveq.l	#0,d0
	tas.b	p_seq_flag(a5)	*set key on bit
poly_:
	move.b	p_extra_ch(a5),d0
	trap	#2
	rts

poly_play:
	lea	rotate_ch(pc),a2
	move.b	(a2),d0
	cmp.w	poly_ch(pc),d0
	bls	@f
	moveq.l	#0,d0
	move.b	d0,(a2)
@@:
	addq.b	#1,(a2)
	trap	#2
	rts

*----------------------------------------
*    X68k ADPCM PUTI NOISE ELIMINATOR
*
*		ＸＡＰＮＥＬ
*
*	 Programmed by Z.Nishikawa
*----------------------------------------
DMADSTAT:	equ	$c32
OPMCTRL:	equ	$9da
*	以下'XAPNEL.X'のルーチンの流用です

CPUSH:	.macro	op
	cmpi.b	#4,($cbc)
	bcs	@skip
	.cpu	68040
	op
	.cpu	68000
@skip:
	.endm

adpcmout:			*IOCS	$60
**	CPUSH	<cpusha dc>
	move.w	sr,-(sp)
	ori.w	#$0700,sr
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
	CPUSH	<cpushl dc,(a2)>
	bsr	adpcm_dtst
	move.b	#$02,$e92001
	moveq.l	#0,d0
	movem.l	(sp)+,d1-d2/a0-a2
	move.w	(sp)+,sr
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
	move.b	se_mode(pc),d0
	bne	@f
	moveq.l	#0,d0		*終了コード
	move.w	sr,-(sp)
	ori.w	#$0700,sr	*最上位割り込みマスク
	move.b	#$10,CCR3
	move.b	#$88,$e92003
	move.w	d0,DMADSTAT.w
	move.w	(sp)+,sr
@@:
	rts
adpcm_stop:
	moveq.l	#0,d0
	move.w	sr,-(sp)
	ori.w	#$0700,sr
	move.b	d0,se_mode
	move.b	#$20,CCR3	*動作中断
				*move.b	#$88,$e92003をしないのは再開した時に音が変に鳴るから
	move.w	(sp)+,sr
	rts
adpcm_cnt:
	move.b	#$08,CCR3	*動作継続
	moveq.l	#0,d0
	rts

int_adpcm_stop:			*ＡＤＰＣＭデータの再生が終了するとここへくる
	ori.w	#$0700,sr
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
	moveq.l	#0,d0
	move.b	d0,DMADSTAT.w
	move.b	d0,se_mode-last_feff(a0)
	movem.l	(sp)+,d0-d2/a0-a1
	rte

adpcm_stop_v:	ds.l	1
adpcmout_v:	ds.l	1
adpcmmod_v:	ds.l	1
	.align	16
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
	.even

	if	debug=1
debug2:				*デバグ用ルーチン(レジスタ値を表示／割り込み対応)
	move.w	sr,db_work2	*save sr	(サブルーチンget_hex32が必要)
	ori.w	#$700,sr	*int on
	movem.l	d0-d7/a0-a7,db_work
	move.l	(a7),pc_work

	moveq.l	#%0011,d1
	IOCS	_B_COLOR

	lea	str__(pc),a1

	move.w	#$0d0a,(a1)+

	moveq.l	#8-1,d7
	lea	db_work(pc),a6
dbg2_lp01:
	move.l	(a6)+,d0
	bsr	get_hex32
	addq.w	#8,a1
	cmpi.b	#4,d7
	bne	@f
	move.b	#' ',(a1)+
@@:
	move.b	#' ',(a1)+
	dbra	d7,dbg2_lp01

	move.b	#$0d,(a1)+
	move.b	#$0a,(a1)+

	moveq.l	#8-1,d7
dbg2_lp02:
	move.l	(a6)+,d0
	bsr	get_hex32
	addq.w	#8,a1
	cmpi.b	#4,d7
	bne	@f
	move.b	#' ',(a1)+
@@:
	move.b	#' ',(a1)+
	dbra	d7,dbg2_lp02

	move.l	pc_work(pc),d0
	bsr	get_hex32
	addq.w	#8,a1
*	move.b	#$0d,(a1)+
*	move.b	#$0a,(a1)+
	clr.b	(a1)+
	lea	str__(pc),a1
	IOCS	_B_PRINT
@@:
	btst.b	#5,$806.w
	bne	@b

	movem.l	db_work(pc),d0-d7/a0-a7
	move.w	db_work2(pc),sr	*get back sr
	rts

*debug3:				*デバグ用ルーチン
*	movem.l	d0-d7/a0-a6,-(sp)
*	move.l	pc_work(pc),a6
*	move.b	d1,(a6)+
*	move.b	d3,(a6)+
*	move.l	a6,pc_work
*	movem.l	(sp)+,d0-d7/a0-a6
*	rts
		*デバッグ用ワーク
	.even
str__:	ds.b	96*2
	dc.b	'REGI'
db_work:		dcb.l	16,0		*for debug
db_work2:		dc.l	0
pc_work:		dc.l	$c00000
	endif
