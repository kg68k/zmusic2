
open_er_mes:	dc.b	' … File not found.',COL_DEF,CRLF,0
write_er_mes:	dc.b	' … Write error.',COL_DEF,CRLF,0
read_er_mes:	dc.b	' … Unable to read.',COL_DEF,CRLF,0
work_er_mes:	dc.b	COL3_EM_REV,'Work area is not enough.',COL_DEF,CRLF,0
out_trk_mes:	dc.b	COL3_EM_REV,'Track buffer is not enough.',COL_DEF,CRLF,0
no_as_er_mes1:	dc.b	'TRACK ',0
no_as_er_mes2:	dc.b	' … NOT ASSIGNED.',CRLF,0
b_clr_st:	dc.b	$1b,'[J',0
b_era_st:	dc.b	$1b,'[2K',0
		*MML COMPILE ERROR MESSAGE
err_mes2:	dc.b	'| TRACK #',0
err_mes5:	dc.b	'TRACK BUFFER IS TOO SMALL',0
err_mes6:	dc.b	'| CHANNEL #',0
err_mes9:	dc.b	'I c ｴ',0
err_mes10:	dc.b	'DEBUG c HAS STILL EXISTED',0
err_mes11:	dc.b	'@X c ｴ',0
err_mes12:	dc.b	'| DEVICE ID',0
err_mes13:	dc.b	'RELATIVE VOLUME c ｴ',0
err_mes14:	dc.b	'@C c ｴ',0
err_mes15:	dc.b	'@A c ｴ',0
err_mes16:	dc.b	'NOTHING = FOR INTERNAL DEVICES',0
err_mes17:	dc.b	'@H c ｴ',0
err_mes18:	dc.b	'@S c ｴ',0
err_mes19:	dc.b	'UNDEFINED MML ｴ',0
err_mes20:	dc.b	'[～] c ｴ',0
err_mes22:	dc.b	'REPEAT c ｴ',0
err_mes25:	dc.b	'| OCTAVE ｴ',0
err_mes27:	dc.b	'| LENGTH ｴ',0
err_mes28:	dc.b	'TRACK BUFFER IS FULL',0
err_mes29:	dc.b	'@B c ｴ',0
err_mes30:	dc.b	'NOTHING = FOR ADPCM',0
err_mes31:	dc.b	'TIE c ｴ',0
err_mes32:	dc.b	'T c ｴ',0
err_mes33:	dc.b	'@T c ｴ',0
err_mes35:	dc.b	'V c ｴ',0
err_mes37:	dc.b	'@K c ｴ',0
err_mes38:	dc.b	'| NOTE #',0
err_mes39:	dc.b	'| PROGRAM #',0
err_mes40:	dc.b	'{～} c ｴ',0
err_mes43:	dc.b	'Q/@Q c ｴ',0
err_mes45:	dc.b	'Y c ｴ',0
err_mes46:	dc.b	'J c ｴ',0
err_mes47:	dc.b	'P/@P c ｴ',0
err_mes48:	dc.b	'K c ｴ',0
err_mes49:	dc.b	'CHORD c ｴ',0
err_mes51:	dc.b	'@V c ｴ',0
err_mes52:	dc.b	'PORTAMENT c ｴ',0
err_mes53:	dc.b	'| VELOCITY VALUE',0
err_mes54:	dc.b	'N/@N c ｴ',0
err_mes55:	dc.b	'@M c ｴ',0
err_mes56:	dc.b	'H c ｴ',0
err_mes57:	dc.b	'@Z c ｴ',0
err_mes58:	dc.b	'| % FORMAT',0
err_mes59:	dc.b	'DELAY TIME IS TOO LONG',0
err_mes62:	dc.b	'FILE READ ｴ',0
err_mes63:	dc.b	'W c ｴ',0
err_mes64:	dc.b	'WORK AREA IS TOO SMALL',0
err_mes69:	dc.b	'BAD % FOR THE MIDI INSTRUMENT',0
err_mes70:	dc.b	'| % ｴ',0
err_mes72:	dc.b	'X c ｴ',0
err_mes74:	dc.b	'| WAVE FORM #',0
err_mes75:	dc.b	'M c ｴ',0
err_mes76:	dc.b	'WAVE FORM SETTING c ｴ',0
err_mes77:	dc.b	'; c ｴ',0
err_mes78:	dc.b	'\ c ｴ',0
err_mes79:	dc.b	'? c ｴ',0
err_mes80:	dc.b	'@F c ｴ',0
err_mes81:	dc.b	'@G c ｴ',0
err_mes82:	dc.b	'@Y c ｴ',0
err_mes83:	dc.b	'@E c ｴ',0
		*COMMON COMMAND COMPILE ERROR
err_mes84:	dc.b	'SYNTAX ｴ',0
err_mes85:	dc.b	'(A) c ｴ',0
err_mes86:	dc.b	'(M) c ｴ',0
err_mes87:	dc.b	'(T) c ｴ',0
err_mes88:	dc.b	'FM VOICE SET c ｴ',0
err_mes89:	dc.b	'ADPCM CONFIGURATION c ｴ',0
err_mes90:	dc.b	"| % FORMAT IN COMMON c",0
err_mes91:	dc.b	'(O) c ｴ',0
err_mes92:	dc.b	'UNDEFINED c ｴ',0
err_mes93:	dc.b	"% OUT OF RANGE",0

ILLEGAL:	dc.b	'ILLEGAL',0	*|
ERROR:		dc.b	'ERROR',0	*ｴ
COMMAND:	dc.b	'COMMAND',0	*c
NUMBER:		dc.b	'NUMBER',0	*#
PARAMETER:	dc.b	'PARAMETER',0	*%
EFFECTS:	dc.b	'EFFECTS',0	*=

error_mes:	dc.b	' (No.',0
kakko_toji:	dc.b	')',CRLF,0

how_many_err:	dc.b	' ERROR(S)',CRLF,0
no_err_mes:	dc.b	'Operations are all set.',CRLF
		dc.b	'A ',COL3_EM,'♪SOUND',COL_DEF,' mind in a '
		dc.b	COL3_EM,'SOUND',COL_DEF,' body.',CRLF,0
print_calc:	dc.b	'Now calculating the total step time...',0
