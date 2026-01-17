# Makefile for ZMLIB

AS = has060
ASFLAGS = -w2 -c2 -i../include
AR = oar
ARFLAGS = -c

TARGET = libzmusic.a

SRCS = zmlib.s
OBJS = $(SRCS:.s=.o)

.PHONY: all
.PRECIOUS: %.o

all: $(TARGET)

%.o: %.s
	$(AS) $(ASFLAGS) -o$@ $<

libzmusic.a: $(OBJS)
	$(AR) $(ARFLAGS) $@ $^


# EOF
