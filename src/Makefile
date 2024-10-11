# Makefile for Z-MUSIC Ver.2

ifeq ($(notdir $(CURDIR)),src)
$(error do not execute make in src directory)
endif

SRCDIR_MK = ../srcdir.mk
SRC_DIR = ../src
-include $(SRCDIR_MK)

AS = has060
ASFLAGS = -w2 -c3 -i./include
LD ?= hlk
LDFLAGS =
CV = cv
LIB = lib
LIBFLAGS =

ifeq ($(findstring hlkx,$(LD)),hlkx)
USE_HLKX = true
endif

TARGET = zmusic.x zmusic_rs.x zmusic_po.x zmsc.x zmsc_rs.x zmsc_po.x \
	 musicz.fnc zp.r zpcnv.r zplk.r zvt.x comchk.r zmd18.r \
	 ZMUSIC.L

ZM_UNIVERSAL = -stype=0 -smpu=30 -sdebug=0
ZM_RSMIDI    = -stype=3 -smpu=30 -sdebug=0
ZM_POLYPHON  = -stype=4 -smpu=30 -sdebug=0

ZMUSIC_SRCS  = dma.mac label.mac macro.mac rsmidi_mbrs.s rsmidi_rate.s work.s zmsc2.s
ZMUSICX_SRCS = zmusic.s $(ZMUSIC_SRCS)
ZMSCX_SRCS   = zmsc.s   $(ZMUSIC_SRCS)


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

zmusic.o: $(ZMUSICX_SRCS)
	$(AS) $(ASFLAGS) $(ZM_UNIVERSAL) -o$@ $<

zmusic_rs.o: $(ZMUSICX_SRCS)
	$(AS) $(ASFLAGS) $(ZM_RSMIDI) -o$@ $<

zmusic_po.o: $(ZMUSICX_SRCS)
	$(AS) $(ASFLAGS) $(ZM_POLYPHON) -o$@ $<

zmsc.o: $(ZMSCX_SRCS)
	$(AS) $(ASFLAGS) $(ZM_UNIVERSAL) -o$@ $<

zmsc_rs.o: $(ZMSCX_SRCS)
	$(AS) $(ASFLAGS) $(ZM_RSMIDI) -o$@ $<

zmsc_po.o: $(ZMSCX_SRCS)
	$(AS) $(ASFLAGS) $(ZM_POLYPHON) -o$@ $<

musicz.o: musicz.s
	$(AS) $(ASFLAGS) -c2 -sdbg=0 -o$@ $<

zplk.o: zplk.s
	$(AS) $(ASFLAGS) -c2 -o$@ $<

ZMLIB.s: zmlib/zmlib.s
	rm -f $@
	cp $< $@

ZMUSIC.L: ZMLIB.o
	touch 199405211803.40 $^
	$(LIB) $(LIBFLAGS) $@ $^
	touch $^ $@


# EOF
