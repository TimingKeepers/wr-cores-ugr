CXX      = g++
CXXFLAGS = -Wall -O2 -I.
TARGETS  = lib/version.h libeca.a eca-ctl eca-table

all:	$(TARGETS)

clean:
	rm -f $(TARGETS) *.o lib/*.o git.*

eca-ctl:	eca-ctl.o libeca.a
	$(CXX) $(CXXFLAGS) -o $@ $^ -letherbone

eca-table:	eca-table.o libeca.a
	$(CXX) $(CXXFLAGS) -o $@ $^ -letherbone

libeca.a:	lib/hw-eca.o lib/hw-stream.o lib/hw-channel.o \
		lib/load-search.o lib/load-walk.o lib/load-queue.o lib/load-eca.o \
		lib/program-search.o lib/program-walk.o lib/program-table.o
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
