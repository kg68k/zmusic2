***************** ワークエリア *****************
		.even
work:
real_ch_tbl:	dc.b	0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19
		dc.b	20,21,22,23,24
		dc.b	25,26,27,28,29,30,31
		.even
zmd18_p:
fo_flg:		dc.l	0		*フェードアウト処理をしたチャンネルのビットパターン
zmd18:
fo_or_not:	dc.b	0		*フェードアウト処理をするかしないか
p16_or_not:	dc.b	0		*16bit pcmかどうか(func $10)
		.even
play_trk_tbl:	dcb.b	tr_max,-1	*どのtrkを演奏するのか
ptt_end:	dc.b	$ff		*end mark

onkai_flg_:				*onkai_flgのバックアップ
psw_est?:				*-Pスイッチが使用されたか？(常駐処理時使用)
up_down:	dc.b	0		*ADPCM加工ワーク
hajimete:	dc.b	0		*ADPCM加工ワーク
adpb_clr:	dc.b	0		*ADPCMバッファ初期化有無フラグ(nz=要初期化)
read_or_not:	dc.b	0		*データを読んだか読まなかったか(0=読まなかった,-1=読んだ)
OPM:		dc.b	'oPm',0		*自己出力用ファイルネーム
MIDI:		dc.b	'MiDi',0	*自己出力用ファイルネーム
getname:	dc.b	'zmusic',0,0	*環境変数名
cmnt:		dc.b	0
data:		dc.b	0
adpcm_read_flg:	dc.b	0		*ADPCMデータを読んだか
oct_wk:					*和音オクターブワーク
su_flg:		dc.b	0		*START UP FILEを読むかどうか
loop_time:	dc.b	0		*func $3b 希望ループ回数
loop_chk_trk:	dc.b	0		*func $3b 規定条件をｸﾘｱしたﾄﾗｯｸが幾つあるか
support_mode:	dcb.b	sp_max,0	*func $40ワーク
jump_flg2:	dc.b	0		*[@] flag
se_level:	dc.b	0		*ADPCM効果音の優先レベル(default=0)
		.even
outlvl:		ds.b	1		*func $47のパラメータ保存
rv_p:		ds.b	1		*adpcn cnf command work
rotate_ch:	dc.b	0		*poly mode用ワーク
		.even

sc55_id:	dc.b	$10		*-$86)DEVICE IDデフォルト値
mt32_id:	dc.b	$10		*-$85)
u220_id:	dc.b	$10		*-$84)
m1_id:		dc.b	$30		*-$83)
loop_chk:	dc.b	0		*-$82)ループチェック有りか無しか(func $3b用ワーク)
ps_flg:		dc.b	0		*-$81)picture sync mode flag
frq:		dc.b	0		*-$80)					!!!
y3:		dc.b	0		*-$7f)					!!!
noise_mode:	dc.b	0		*-$7e)NOISE modeかどうか		!!!
first_cmt:	ds.b	96+1		*-$7d)最初のコメント格納バッファ	!!!
pcm8_ch:	dc.b	0		*-$1c)PCM8時のチャンネル番号の覚え書き		!!!
ch_tr_msk:	dc.b	0		*-$1b)トラック系?/チャンネル系?(0=tr,ne=ch)	!!!
ch_tr_opl:	dc.b	0		*-$1a)トラック系?/チャンネル系?(0=tr,ne=ch)	!!!
mclk:		dc.b	192		*-$19)全音符の絶対音長カウント			!!!
se_mode:	dc.b	0		*-$18)ADPCMの効果音モードか
cmd_or_dev:	dc.b	0		*-$17)コマンドライン？それともDEVICE=？
timer_a_mode:	dc.b	0		*-$16)どのタイマーを使うか(A/B)
synchro_mode:	dc.b	0		*-$15)外部同期モードか($ff=yes)
mfp_mode:	dc.b	0		*-$14)多重割り込み対応モードか($ff=yes)
no_init_mode:	dc.b	0		*-$13)初期化無しモードか
trace_mode:	dc.b	0		*-$12)func $19 実行中?
		dc.b	0		*-$11)undefined
m_tmp_buf:	dc.w	0		*-$10)テンポ値(default=120)	!!!順番変更禁止
timer_value:	dc.w	0		*-$0e)timer b value(8bits)	!!!
zmusic_int:	dc.l	0		*-$0c)ZMUSICが割り込むとインクリメントされる
err_code:	dcb.b	8,0		*-$1～-$8)発生したエラーコードのＦＩＦＯ
zmusic_stat:
midi_board:	dc.b	0		*0)MIDIチャンネル有効／無効(無効=0/有効<>0)	!!!0
rs_midi:	dc.b	rs232c		*1)MIDIタイプ(0=CZ6BM1,-1=RS232C,+1POLYPHON	!!!1
emulate_mode:	dc.b	0		*2)-Uスイッチ(設定<>0)				!!!2
pcm8_flg:	dc.b	0		*3)PCM8でADPCMを鳴らすかどうか(0=no,$ff=yes)	!!!3
poly_ch:	dc.w	-1		*4)ポリーモードにおける				!!!4 5
					*5)ボイスリザーブ数－１(default-1)		!!!
juke_mode:	dc.b	0		*6)外部プログラム常駐状況			!!!6
timer_flg:	dc.b	0		*7)TIMER ON/OFF					!!!7
last_fn:	ds.b	91		*8～98)前回読み込んだファイルの名前		!!!8
		.even			*99)
date_buf:	ds.l	1		*100)ファイルのタイムスタンプ保存

	ifndef	tiny
trk_num:	dcb.b	8,0		*default track number buffer	!!!
		dc.b	0		*end code			!!!
vs_mode:	dc.b	0		*音色設定中か
act_flg:	dc.b	$ff		*[!]/[end]の有効無効フラグ
rltv_uv_mode:	dc.b	0		*_~記号の機能アサイン
		.even
renp_flg:	dc.b	0		*連符フラグ				!!!
onkai_flg:	dc.b	0		*前回のコマンドは音階MMLだったか	!!!

rinji_buf:	dc.w	0		*臨時ベロシティ復元バッファ	!!!
		dc.b	0		*				!!!
rinji_flg:	dc.b	0		*臨時ベロシティ復元フラグ	!!!

clcttl_mode:	dc.b	0		*-Qか
compile_mode:	dc.b	0		*絶対ゼロ(compile modeかどうか)
no_optmz:	dc.b	0		*コンパイル時に未使用トラックを消去するか
kakko_type:	dc.b	0		*終端コード
v_number:	dc.b	0		*音色設定コマンドワーク
v_offset:	dc.b	0		*同上
shp_com_no:	dc.b	0		*実行中の共通コマンド番号
note_number:				*楽器個別コマンド用ワーク
rythm_number
prog_number:
part_number:	dc.b	0
partial_number:
map_number:	dc.b	0
outmem_flg:	dc.b	0		*out of memory error flg
mml_cnv_err:	dc.b	0
	endif

	if	type=4
polyphon_buff:	ds.b	1
	endif

		.even
*			        dev mdl
header:		dc.b	$f0,$41,$00,$00,$12	*ROLAND EXCLUSIVE HEADER
exc_addr:	dc.b	0,0,0		*ROLAND EXCLUSIVE ADDRESS
sc_p_data:	dc.b	0		*1 byte転送時に使用
tail:		dc.b	0,$f7		*ROLAND EXCLUSIVE TAIL

SPC2:	dc.b	'  ',0
CRLF:	dc.b	13,10,0
	dc.b	09		*tab(ERROR CODE表示時に使用)	!!!順番と位置を
suji:	dcb.b	11,0		*数値表示用			!!!変えては駄目

	.even
fade_p:		dc.l	0	*ADPCM加工処理関係のパラメータ
cut_p:		dc.l	0	*ADPCM加工処理関係のパラメータ
mix_p:		dc.l	0	*ADPCM加工処理関係のパラメータ
vol_p:		dc.l	0	*ADPCM加工処理関係のパラメータ
pitch_p:	dc.l	0	*ADPCM加工処理関係のパラメータ

compile_p:				*コンパイルデータのポインタ
rec_data_now:		dc.l	0	*MIDIデータ格納アドレス
zmd18_c:
rec_data_end:		dc.l	0	*MIDIデータ格納最終アドレス

			*ここから
trk_top:		dc.l	0		*00)TRACK BUFFER TOP ADDRESS
trk_buf_size:		dc.l	0		*04)TRACK BUFFER SIZE
trk_buf_end:					*08)TRACK BUFFER END ADDRESS
dev_end_adr:		dc.l	0		*08)(=ZMUSIC.X END ADDRESS)
adpcm_buffer_top:	dc.l	0		*12)ADPCM DATA BUFFER TOP ADDRESS
adpcm_buffer_size:	dc.l	dflt_pcmbf	*16)ADPCM DATA BUFFER SIZE
adpcm_buffer_end:	dc.l	0		*20)ADPCM DATA BUFFER END ADDRESS
adpcm_work_top:		dc.l	0		*24)ADPCM DATA WORK TOP ADDRESS (実際は汎用ワーク)
adpcm_work_size:	dc.l	0		*28)ADPCM DATA WORK SIZE
adpcm_work_end:		dc.l	0		*32)ADPCM DATA WORK END ADDRESS
adpcm_buffer_next:	dc.l	0		*36)NEXT ADPCM DATA ADDRESS
						*V1.22以降追加
adpcm_work_now:		dc.l	0		*40)NEXT WORK ADDRESS
adpcm_work_true_size:	dc.l	0		*44)ADPCM DATA WORK TRUE SIZE
seq_wk_tbl:		dc.l	0		*48)演奏中のワーク
seq_wk_tbl2:		dc.l	0		*52)演奏中のワーク(se mode用)
adpcm_tbl:		dc.l	0		*56)ADPCM DATAのADDRESSテーブル(default=0)
wave_tbl:		dc.l	0		*60)波形メモリデータのテーブルアドレス
neiro:			dc.l	0		*64)FM音色バッファ
						*V1.49H以降追加
trk_po_tbl:		dc.l	0		*68)各トラックの先頭アドレス格納テーブルアドレス
trk_len_tbl:		dc.l	0		*72)m_allocで確保した各トラックのサイズ格納アドレス
			*ここまで順序変更禁止

last_val:	dc.w	0		*0	ADPCM加工処理関係のWORK
last_val2:	dc.w	0		*2
frq_wk:		dc.w	0		*4
frq_flg:	dc.w	0		*6
vect_1f0:	dc.l	0		*default 0(IOCS OPMDRV)
sv_trap3:	dc.l	0		*trap #3
copy_org:	dc.l	0
rec_vect:	dc.l	0		*default=0(vector $8a)
	if	type=3			*rs232c
rec_vect_:	dc.l	0
	endif
out_name:	dc.l	0		*OPM/MIDIどちらへ出力するか
fh_su:		dc.l	0		*ファイルハンドル(START UP FILE組み込み専用)
loop_bsr:	dc.l	0		*func $3b用ワーク
tmp_base:	dc.l	78125		*タイマ値計算用定数
dmy_seq_wk:	ds.l	1		*ダミーシーケンスワーク

rte_src:	ds.l	1		*パッチ元
m_play00_bak:	ds.l	1		*パッチ元
dummy_vect:	dc.l	0		*default=0(PCM8組み込み拒否)
d0_work:	dc.l	0		*汎用ワーク
d2_work:	dc.l	0		*汎用ワーク
eox_w:		dc.w	3		*EOX wait(default)
sr_type:	dc.w	$2500		*sr をマスクする際のデフォルト値(普通は書き変わる)
mp_bak:		dcb.l	3,0		*m_playのバックアップ
mask_wk:	ds.l	1		*func $44のパラメータ保存
out_wk:		ds.l	1		*func $47のパラメータ保存
adpcm_n_max:	dc.l	adpcm_n_max_default	*定義できる最大数(default=512)
_sp_buf:	ds.l	1		*MMLコンパイル/ZMUSIC起動時のスタックワーク
ssp:		dc.l	0		*スーパーバイザスタックの一時退避ワーク
a0work:		dc.l	0		*default=0(汎用ワーク)
a1work:		dc.l	0		*default=0(汎用ワーク)
a2work:		dc.l	0		*default=0(汎用ワーク)
done_flg:				*init_inst用ワーク
nul_address:	dc.l	0		*NULの存在したアドレス
zmd_size:	dc.l	0		*ZMDを演奏しているならばそのサイズ(ZMSは0)
zpd_size:	dc.l	0
timer_i_v:	dc.l	$78_0174	*テンポ＆タイマー初期値(タイマーＢでは書き変わる)

	ifndef	tiny
rest_buf:	dc.w	0		*連符処理関係ワーク
x_midi:		dc.l	0		*MIDIデータ転送コマンドの途中か
renp_cnt:	dc.l	0		*連符の個数
psp_buf:	dc.l	0		*連符ワーク
cnv_wk_tbl:	dc.l	0		*コンパイル中のワーク
num_of_err:	dc.l	0		*コンパイル時に発生したエラーの個数
line_number:	dc.l	0		*現在コンパイル中の行数
shp_rd_ex:	dc.b	$10,$16		*device ID,MT-32 (DEFAULT VALUE)
shp_com_ptr:	dc.l	0		*#コマンド系のポインタ
wv_loop_str:				*波形メモリループ開始オフセットアドレス
waon_buf:				*和音用バッファ
modu_work:				*各種モジュレーション用ワーク
aftc_work:	dcb.b	16,0		*アフタータッチシーケンス用バッフア
st_buf:		dc.l	0		*和音用音長/ポルタメント音長用ワーク
gt_buf:		dc.l	0		*ゲートタイムバッファ
shp_com_pr:	dc.w	0		*実行中の共通コマンドのパラメータオフセットワーク
adpcm_bank:	dc.w	0		*セレクトされているADPCMバンク番号*128(default=0)!!!
err_exist_f:	dc.b	0		*エラーが１回でもあったか			 !!!
fm_master:	dc.b	0		*FM音源のマスターボリューム(コンパイルレベル)	 !!!
	endif

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
wv_dmy:	dc.l	1

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

	ifndef	tiny
num_of_seq:	equ	15		*幾つのコマンドがあるか
mus_com_tbl:				*［］コマンド系(8文字以内)
	dc.b	'D.C.',0		*0
  	dc.b	'D.S.',0 		*1
	dc.b	'SEGNO',0		*2
	dc.b	'$',0			*3
	dc.b	'TOCODA',0		*4
	dc.b	'*',0			*5
	dc.b	'FINE',0		*6
	dc.b	'^',0			*7
	dc.b	'CODA',0		*8
	dc.b	'DO',0			*9
	dc.b	'LOOP',0		*10
	dc.b	'!',0			*11
	dc.b	'END',0			*12
	dc.b	'@',0			*13
	dc.b	'K.SIGN',0		*14

num_of_shp:	equ	43		*＃コマンド系総数
shp_com_tbl:				*＃コマンド系(16文字以内)
					*パラメータを単一行で記述しなければならない
ADPCM_LIST:	equ			0	*read opmd cnf file
	dc.b	'ADPCM_LIST',0
O:	equ				1	*adpcm cnf
	dc.b	'O',0
PRINT:		equ			2	*print message
	dc.b	'PRINT',0
FM_VSET:	equ			3	*fm_vset
	dc.b	'FM_VSET',0
ADPCM_BANK:	equ			4	*select adpcm bank
	dc.b	'ADPCM_BANK',0
SC55_PRINT:	equ			5 	*sc55 message print
	dc.b	'SC55_PRINT',0
MT32_PRINT:	equ			6 	*mt32 message print
	dc.b	'MT32_PRINT',0
MIDI_DUMP:	equ			7 	*midi data dump
	dc.b	'MIDI_DUMP',0
U220_PRINT:	equ			8 	*u220 message print
	dc.b	'U220_PRINT',0
SEND_TO_M1:	equ			9 	*SEND PARAMETER TO M1
	dc.b	'SEND_TO_M1',0
M1_PRINT:	equ			10	*M1 message print
	dc.b	'M1_PRINT',0
ADPCM_BLOCK_DATA:	equ		11	*read adpcm block data
	dc.b	'ADPCM_BLOCK_DATA',0
COMMENT:	equ			12	*comment
	dc.b	'COMMENT',0
FM_MASTER_VOLUME:	equ		13	*FM master volume
	dc.b	'FM_MASTER_VOLUME',0
ERASE:		equ			14	*DUMMY(ZPCNV.X専用命令)
	dc.b	'ERASE',0
SC55_INIT:	equ			15	*SC55初期化
	dc.b	'SC55_INIT',0
MT32_INIT:	equ			16	*MT32初期化
	dc.b	'MT32_INIT',0
*---------- パラメータを複数行に渡って記述できる ------------
M1_EFFECT_SETUP:	equ		17	*M1 SEQ EFFECT PARAMETER
	dc.b	'M1_EFFECT_SETUP',0
ROLAND_EXCLUSIVE:	equ		18	*roland_exc
	dc.b	'ROLAND_EXCLUSIVE',0
EXCLUSIVE:	equ			19	*exclusive
	dc.b	'EXCLUSIVE',0
SC55_V_RESERVE:	equ			20	*sc55ﾎﾞｲｽﾘｻﾞｰﾌﾞ
	dc.b	'SC55_V_RESERVE',0
SC55_REVERB:	equ			21	*sc55 reverb parameter
	dc.b	'SC55_REVERB',0
SC55_CHORUS:	equ			22	*sc55 chorus parameter
	dc.b	'SC55_CHORUS',0
SC55_PART_SETUP:	equ		23	*sc55 part parameter
	dc.b	'SC55_PART_SETUP',0
SC55_DRUM_SETUP:	equ		24	*sc55 drum parameter
	dc.b	'SC55_DRUM_SETUP',0
SC55_DISPLAY:	equ			25	*sc55 graphic print
	dc.b	'SC55_DISPLAY',0
MT32_P_RESERVE:	equ			26	*mt32 pertial reserve
	dc.b	'MT32_P_RESERVE',0
MT32_REVERB:	equ			27	*mt32 reverb parameter
	dc.b	'MT32_REVERB',0
MT32_PART_SETUP:	equ		28	*mt32 part parameter
	dc.b	'MT32_PART_SETUP',0
MT32_DRUM_SETUP:	equ		29	*mt32 drum parameter
	dc.b	'MT32_DRUM_SETUP',0
MT32_COMMON:	equ			30	*mt32 timbre common parameter
	dc.b	'MT32_COMMON',0
MT32_PARTIAL:	equ			31	*mt32 timbre pirtial parameter
	dc.b	'MT32_PARTIAL',0
MT32_PATCH:	equ			32	*mt32 patch parameter
	dc.b	'MT32_PATCH',0
U220_SETUP:	equ			33	*u220 setup parameter
	dc.b	'U220_SETUP',0
U220_COMMON:	equ			34	*u220 patch common
	dc.b	'U220_COMMON',0
U220_DRUM_SETUP:	equ		35	*u220 patch drum parameter
	dc.b	'U220_DRUM_SETUP',0
U220_PART_SETUP:	equ		36	*u220 patch part parameter
	dc.b	'U220_PART_SETUP',0
U220_TIMBRE:	equ			37	*u220 timbre set
	dc.b	'U220_TIMBRE',0
U220_DRUM_INST:	equ			38	*u220 rhythm instrument change
	dc.b	'U220_DRUM_INST',0
M1_MIDI_CH:	equ			39	*M1 MIDI CH setup
	dc.b	'M1_MIDI_CH',0
M1_PART_SETUP:	equ			40	*M1 SEQ PART PARAMETER
	dc.b	'M1_PART_SETUP',0
WAVE_FORM:	equ			41	*user wave form define
	dc.b	'WAVE_FORM',0
MIDI_DATA:	equ			42	*生データ出力
	dc.b	'MIDI_DATA',0
	endif

m1_ef_dflt:			*M1デフォルトエフェクトデータ
	dc.b	$0B,$00,$1E,$1E,$19,$19,$00,$00,$1F
	dc.b	$3C,$09,$03,$00,$0A,$00,$00,$00
	dc.b	$1A,$00,$28,$32,$1C,$00,$00,$00

	.even
ZMD:		dc.b	'ZMD',0
ZMS:		dc.b	'ZMS',0
ZPD:		dc.b	'ZPD',0
CNF:		dc.b	'CNF',0
MDD:		dc.b	'MDD',0

	.even
v_buffer:	ds.b	256		*汎用小バッファ
read_mes1:	dc.b	"Block ADPCM data '",$1b,'[32m',0
read_mes2:	dc.b	"Start up file '",$1b,'[32m',0
cannot_read:	dc.b	$1b,"[33m' couldn't be found.",13,10,0
default_adp:	dc.b	$1b,"[33m' has been included.",13,10,0
vb_end:
	.even
filename:	ds.b	91		*読み込もうとするファイル名
open_fn:	ds.b	91		*実際にオープンするファイル名
	.even
h_work:		*スタートアップＺＰＤファイルのファイル名バッファ
gaiji:		*外字データ (32*6=192Bytes あとで汎用ワークとなる)
	dc.w	$2002,$783f,$c7c6,$801c,$0038,$0060,$0cc0,$0380
	dc.w	$06c0,$0c20,$1800,$3001,$63e3,$fc1e,$8006,$0000
	dc.w	$0000,$0000,$0000,$0000,$1ddc,$2626,$2444,$0444
	dc.w	$0888,$0888,$0889,$1111,$1112,$110c,$0000,$0000
	dc.w	$0000,$0000,$0000,$0000,$0608,$0c08,$1410,$2410
	dc.w	$4820,$0820,$0820,$1050,$1092,$0f0c,$0000,$0000
h_work2:
stup_fnsv:	*スタートアップファイルのファイル名バッファ
	dc.w	$03c0,$0460,$0870,$0c20,$0c03,$060c,$0730,$63c6
	dc.w	$63c6,$0ce0,$3060,$c030,$0430,$0e10,$0620,$03c0
	dc.w	$0001,$0006,$000e,$000c,$0c1c,$081c,$001c,$181c
	dc.w	$101c,$301c,$201c,$621c,$440c,$380e,$0006,$0001
	dc.w	$e000,$1000,$0800,$0400,$1c00,$1c00,$1c00,$0800
	dc.w	$0000,$0000,$0000,$0400,$0400,$0800,$1000,$e000

	if	(type<>3.and.type<>4)
*	-uスイッチ用ワーク
	dcb.b	14,-1
damasu:
	dcb.b	16,-1
	.even
	endif

*開発に使用した主なツール
*	SUPERED  v1.18		(C)T.Nishikawa
*	HAS v2.55		(C)Y.NAKAMURA
*	HLK v2.27		(C)SALT
*	DB  v2.00		(C)SHARP/Hudson
*	DI  v0.51+13		(C)S.OHYAMA/GORRY,CAT-K
