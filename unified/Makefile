# Makefile for Z-MUSIC Ver.2

AS = has060
ASFLAGS = -w2 -c2 -i./include
LD ?= hlk
LDFLAGS =
CV = cv
LIB = lib
LIBFLAGS =

ifeq ($(findstring hlkx,$(LD)),hlkx)
USE_HLKX = true
endif

TARGET = zmusic.x zmusic_rs.x zmusic_po.x zmsc.x zmsc_rs.x zmsc_po.x
	# musicz.fnc zp.r zpcnv.r zplk.r zvt.x comchk.r zmd18.r \
	# ZMUSIC.L

ZM_OLD16BIT = -sOLD16BIT
ZM_TINY     = -sTINY

ZM_CZ6BMI   = -sCZ6BMI
ZM_RSMIDI   = -sRSMIDI
ZM_POLYPHON = -sPOLYPHON

ZMUSIC_SRCS = zmusic.s \
	work.s interrupt.s mml_compile.s mml_mes.s mml_error.s compiler.s \
	skipspc.s copyfn.s getfdate.s fileio.s chknum.s \
	dma.mac label.mac macro.mac version.mac


.PHONY: all
.PRECIOUS: %.o

all: $(TARGET)

%.x: %.s
%.o: %.s
	$(AS) $(ASFLAGS) -o$@ $<
%.x %.fnc: %.o
	$(LD) $(LDFLAGS) -o$@ $<

ifdef USE_HLKX
%.r: %.o
	$(LD) $(LDFLAGS) -r --omit-bss -o$@ $<
else
%.r: %.x
	$(CV) $< $@
endif

zmusic.o: $(ZMUSIC_SRCS)
	$(AS) $(ASFLAGS) $(ZM_CZ6BMI) -o$@ $<

zmusic_rs.o: $(ZMUSIC_SRCS)
	$(AS) $(ASFLAGS) $(ZM_RSMIDI) -o$@ $<

zmusic_po.o: $(ZMUSIC_SRCS)
	$(AS) $(ASFLAGS) $(ZM_POLYPHON) -o$@ $<

zmsc.o: $(ZMUSIC_SRCS)
	$(AS) $(ASFLAGS) $(ZM_TINY) $(ZM_CZ6BMI) -o$@ $<

zmsc_rs.o: $(ZMUSIC_SRCS)
	$(AS) $(ASFLAGS) $(ZM_TINY) $(ZM_RSMIDI) -o$@ $<

zmsc_po.o: $(ZMUSIC_SRCS)
	$(AS) $(ASFLAGS) $(ZM_TINY) $(ZM_POLYPHON) -o$@ $<

musicz.o: musicz.s
	$(AS) $(ASFLAGS) -sdbg=0 -o$@ $<

ZMLIB.s: zmlib/zmlib.s
	rm -f $@
	cp $< $@

ZMUSIC.L: ZMLIB.o
	touch 199405211803.40 $^
	$(LIB) $(LIBFLAGS) $@ $^
	touch $^ $@


# EOF
