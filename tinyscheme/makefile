# Makefile for TinyScheme
# Time-stamp: <2002-06-24 14:13:27 gildea>

# Windows/2000
#CC = cl -nologo
#DEBUG= -W3 -Z7 -MD
#DL_FLAGS=
#SYS_LIBS=
#Osuf=obj
#SOsuf=dll
#LIBsuf=.lib
#EXE_EXT=.exe
#LD = link -nologo
#LDFLAGS = -debug -map -dll -incremental:no
#LIBPREFIX =
#OUT = -out:$@
#RM= -del
#AR= echo

# Unix, generally
CC = clang -fpic -pedantic
DEBUG=-g -Wall -Wno-char-subscripts -O
Osuf=o
SOsuf=so
LIBsuf=a
EXE_EXT=
LIBPREFIX=lib
OUT = -o $@
RM= -rm -f
AR= ar crs

# Linux
LD = clang
LDFLAGS = -shared
DEBUG=-g -Wno-char-subscripts -O
SYS_LIBS= -ldl -lm -lglfw -L/usr/local/lib
PLATFORM_FEATURES= -DSUN_DL=1

# Cygwin
#PLATFORM_FEATURES = -DUSE_STRLWR=0

# MinGW/MSYS
#SOsuf=dll
#PLATFORM_FEATURES = -DUSE_STRLWR=0

# Mac OS X
#LD = gcc
#LDFLAGS = --dynamiclib
#DEBUG=-g -Wno-char-subscripts -O
#SYS_LIBS= -ldl
#PLATFORM_FEATURES= -DUSE_STRLWR=1 -D__APPLE__=1 -DOSX=1


# Solaris
#SYS_LIBS= -ldl -lc
#Osuf=o
#SOsuf=so
#EXE_EXT=
#LD = ld
#LDFLAGS = -G -Bsymbolic -z text
#LIBPREFIX = lib
#OUT = -o $@

NO_STANDALONE?=-DSTANDALONE=0

FEATURES = $(PLATFORM_FEATURES) -DUSE_DL=1 -DUSE_MATH=1 -DUSE_ASCII_NAMES=0 -g

OBJS = scheme.$(Osuf) dynload.$(Osuf)

LIBTARGET = $(LIBPREFIX)tinyscheme.$(SOsuf)
STATICLIBTARGET = $(LIBPREFIX)tinyscheme.$(LIBsuf)

all: $(LIBTARGET) $(STATICLIBTARGET)

%.$(Osuf): %.c
	$(CC) $(NO_STANDALONE) -I/usr/local/include -I/tmp/raylib -I. -g -c $(DEBUG) $(FEATURES) $(DL_FLAGS) $<

$(LIBTARGET): $(OBJS)
	$(LD) $(LDFLAGS) $(OUT) $(OBJS) $(SYS_LIBS) -g

scheme$(EXE_EXT): $(OBJS)
	$(CC) -o $@ $(DEBUG) $(OBJS) $(SYS_LIBS) -g

$(STATICLIBTARGET): $(OBJS)
	$(AR) $@ $(OBJS)

$(OBJS): scheme.h scheme-private.h opdefines.h
dynload.$(Osuf): dynload.h

clean:
	$(RM) $(OBJS) $(LIBTARGET) $(STATICLIBTARGET) scheme$(EXE_EXT) scheme.exe
	$(RM) tinyscheme.ilk tinyscheme.map tinyscheme.pdb tinyscheme.exp
	$(RM) scheme.ilk scheme.map scheme.pdb scheme.lib scheme.exp
	$(RM) *~
	$(RM) *.dll *.so *.a

TAGS_SRCS = scheme.h scheme.c dynload.h dynload.c

tags: TAGS
TAGS: $(TAGS_SRCS)
	etags $(TAGS_SRCS)

build-windows:
	make clean
	mkdir -p /tmp/raylib
	[ -f "/tmp/raylib/libraylib.a" ] || wget -O /tmp/raylib/libraylib.a https://pub.krzysckh.org/libraylib.a
	cp /usr/local/include/raylib.h /tmp/raylib/
	make all SOsuf=dll PLATFORM_FEATURES="-DUSE_STRLWR=0" CC=x86_64-w64-mingw32-gcc LD=x86_64-w64-mingw32-gcc SYS_LIBS="-L/tmp/raylib -lm -lraylib -lm -lwinmm -lgdi32"
	mv libtinyscheme.a libtinyscheme-w64.a
