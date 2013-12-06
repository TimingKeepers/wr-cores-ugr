PREFIX   ?= /usr/local
STAGING  ?= 
EB       ?= no

ifeq ($(EB),no))
EB_LIB ?= -letherbone
EB_INC ?=
else
EB_LIB ?= -L$(EB) -letherbone
EB_INC ?= -I$(EB)
endif

EXTRA_FLAGS ?=
CXX         ?= g++
CXXFLAGS    ?= $(EXTRA_FLAGS) -Wall -O2 -I. $(EB_INC)

TARGETS  = lib/version.h libeca.a eca-ctl eca-table

all:	$(TARGETS)

install:
	mkdir -p $(STAGING)$(PREFIX)/bin $(STAGING)$(PREFIX)/include $(STAGING)$(PREFIX)/lib
	cp eca-ctl eca-table $(STAGING)$(PREFIX)/bin
	cp eca.h $(STAGING)$(PREFIX)/include
	cp libeca.a $(STAGING)$(PREFIX)/lib

clean:
	rm -f $(TARGETS) *.o lib/*.o git.*

eca-ctl:	eca-ctl.o libeca.a
	$(CXX) $(CXXFLAGS) -o $@ $^ -Wl,-rpath,$(PREFIX)/lib $(EB_LIB)

eca-table:	eca-table.o libeca.a
	$(CXX) $(CXXFLAGS) -o $@ $^ -Wl,-rpath,$(PREFIX)/lib $(EB_LIB) 

libeca.a:	lib/hw-eca.o lib/hw-stream.o lib/hw-channel.o \
		lib/load-table.o lib/store-table.o \
		lib/table.o lib/probe-eca.o
	rm -f $@
	ar rcs $@ $^
	ranlib $@

lib/version.h::
	git log -n1 --pretty="format:%H" . > git.version_full
	git log -n1 --pretty="format:%ci" . > git.date_full
	cut -b1-8 < git.version_full > git.version_short
	date +%Y%m%d -f git.date_full > git.date_short
	sed 's/@ECA_VERSION_FULL@/'"`cat git.version_full`"'/;s/@ECA_VERSION_SHORT@/'"`cat git.version_short`"'/;s/@ECA_DATE_FULL@/'"`cat git.date_full`"'/;s/@ECA_DATE_SHORT@/'"`cat git.date_short`"'/' < $@.in > $@.tmp
	if ! test -f $@ || ! diff $@ $@.tmp > /dev/null; then mv $@.tmp $@; fi
	rm -f $@.tmp git.version_full git.date_full git.version_short git.date_short

%.o:	%.cpp
	$(CXX) $(CXXFLAGS) -c $< -o $@
