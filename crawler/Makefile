RTS_OPTS=-N4 -T -sstderr -I0 -A64M -n2M -K256M  # -p -h -L80 -i5

HSDEPS=*.hs */*.hs Generated/h_ffi.urp Generated/BinaryInstancesParser.h bazqux.cabal cabal.project*

BIN=../bin
QUEUE=$(BIN)/Queue
PARSE_SERVER=$(BIN)/ParseServer
PUBSUBHUBBUB=$(BIN)/PubSubHubbub
READER=$(BIN)/Reader

all: $(QUEUE)

reader: $(READER)
$(READER): Generated/webapp.c

include Makefile.ghc

LOGS_PATH=./dist-newstyle

test: $(PARSE_SERVER) $(QUEUE)
	killall ParseServer
	time make update 2>&1 | gawk '{ print strftime("%H:%M:%S"), $$0; fflush(); }' >$(LOGS_PATH)/queue.log

pubsubhubbub:
	$(PUBSUBHUBBUB) +RTS -M1.5G $(RTS_OPTS)

parse_server:
ifdef LINUX
	$(PARSE_SERVER) +RTS -M3G $(RTS_OPTS)
else
	$(PARSE_SERVER) +RTS -M3G $(RTS_OPTS) >$(LOGS_PATH)/parse_server.log 2>&1
endif

ifdef LINUX
ULIMIT=ulimit -c 1000000
# 512MB -- нужен ли нам такой core?
else
ULIMIT=ulimit -c 0
# под macOS core не включаем, а то может из-за проблем с динамическими
# библиотеками (после смены настроек cabal) начать без конца
# перегружать Reader и забивать диск core-файлами
endif

update:
	$(QUEUE) +RTS -M6G $(RTS_OPTS)

run_pubsubhubbub: $(PUBSUBHUBBUB)
	daemon -r -n pshb -o local0.warning --chdir `pwd` -- sh -c "$(ULIMIT); while true; do make pubsubhubbub; done;"

run_parse_server: $(PARSE_SERVER)
	daemon -r -n ps -o local0.warning --chdir `pwd` -- sh -c "$(ULIMIT); while true; do make parse_server; done;"

run_crawler: $(QUEUE)
	rm -f stop
	daemon -r -n cr -o local0.warning --chdir `pwd` -- sh -c "$(ULIMIT); while true; do make update; done;"

run_reader_internal: # вынес сюда для использования ULIMIT
	daemon -r -L 10 -n bqr -o local0.warning --chdir `pwd`/.. -- sh -c "$(ULIMIT); while true; do make reader; done;"

stop_crawler:
	daemon -n cr --stop || true
	touch stop

stop_parse_server:
	daemon -n ps --stop || true
	killall ParseServer

stop_pubsubhubbub:
	daemon -n pshb --stop

gen Generated/h_ffi.urp: Gen.hs
	@echo Generating data types, I/O and FFI code
	@runghc Gen
	@touch Generated/h_ffi.urp
ifndef LINUX
# в production всегда компилируем с нуля без daemon-а
	@$(MAKE) -C .. uw_daemon >/dev/null
#       ^ перезапускаем daemon, т.к. он не ловит обновление binary instance-ов
endif

LINUX_BIN=dist-newstyle/build/x86_64-linux/ghc-$(GHC_VER)/bazqux-0.1/x/exe/opt/build/exe/exe
MACOS_BIN=dist-newstyle/build/x86_64-osx/ghc-$(GHC_VER)/bazqux-0.1/x/exe/noopt/build/exe/exe

$(BIN)/%: $(HSDEPS)
	cabal build $*
	@mkdir -p $(BIN)
ifdef LINUX
	@cp `pwd`/$(subst exe,$*,$(LINUX_BIN)) $@.new
# развязываем !
else
	@ln -fs `pwd`/$(subst exe,$*,$(MACOS_BIN)) $@.new
endif
	@mv $@.new $@
	@touch $@

json_benchmark:
	cabal run JsonBenchmark --executable-static -- +RTS -s -M1G $(RTS_OPTS)
test_filters:
	cabal run FiltersTest -- +RTS -s -M5G $(RTS_OPTS)
test_proxy:
	cabal run ProxyTest --enable-profiling -- +RTS -s -M1G $(RTS_OPTS) -p
profile_parse_server:
	cabal run ParseServer --enable-profiling -- +RTS -s -M3G $(RTS_OPTS) -p
profile_queue:
	cabal run Queue --enable-profiling -- +RTS -s -M3G $(RTS_OPTS) -p
clear_old_scan_lists:
	$(UTIL) ClearOldScanLists +RTS -M2G $(RTS_OPTS) -N2 >$@.log 2>&1 &
clear_old_url_to_scans:
	$(UTIL) ClearOldUrlToScans +RTS -M2G $(RTS_OPTS) -N2 >$@.log 2>&1 &

INDEX=cabal run Index --
UTIL=cabal run Util --

reindex:
	$(INDEX) ReindexAllPosts +RTS -M6G $(RTS_OPTS) -N2 >ri 2>&1 &

reindex_tags:
	$(INDEX) ReindexAllTags +RTS -M6G $(RTS_OPTS) -N2 >ritags 2>&1 &

prepare_index_subscriptions_data:
	time $(INDEX) PrepareIndexSubscriptionsData +RTS -M10G $(RTS_OPTS)

index_subscriptions:
	rm -f paidUsersCountryStats normalizeSubsTags
	time $(INDEX) IndexSubscriptions +RTS -M10G $(RTS_OPTS)

reindex_feeds: prepare_index_subscriptions_data index_subscriptions

backup_discovery: discovery.tar.bz2
	scp -q discovery.tar.bz2 t:~/discovery.tar.bz2_
	ssh t "touch discovery.tar.bz2; mv discovery.tar.bz2 discovery.tar.bz2.1"
	ssh t "mv discovery.tar.bz2_ discovery.tar.bz2"

backup_discovery_mac: discovery.tar.bz2
	scp -q t:~/discovery.tar.bz2 ~/Dropbox/BazQux/

discovery.tar.bz2: statsUsers subsStatsImm subFeedInfos
	tar cjf discovery.tar.bz2 statsUsers subsStatsImm subFeedInfos

# TrainLanguageDetector: $(HSDEPS)
# 	$(GHC:build=$@) -o TrainLanguageDetector Lib/LanguageDetectorTrainer.hs -main-is Lib.LanguageDetectorTrainer.trainAll
# train_language_detector: TrainLanguageDetector
# 	./TrainLanguageDetector +RTS -M6G $(RTS_OPTS)

# FixNonScanningBlogs: $(HSDEPS)
# 	$(GHC:build=$@) -o FixNonScanningBlogs UrCalls -main-is UrCalls.fixNonScanningBlogs
# fix_non_scanning_blogs: FixNonScanningBlogs
# 	./FixNonScanningBlogs +RTS -M6G $(RTS_OPTS) >FixNonScanningBlogs.log 2>&1 &

strict_derive:
	sed 's%\([^ ]* <- get\)%!\1%' Generated/BinaryInstances_nonstrict.h \
		>Generated/BinaryInstances.h
Generated/BinaryInstancesParser.h: Parser/Types.hs
	cabal exec derive Parser/Types.hs
	sed 's%\([^ ]* <- get\)%!\1%' Generated/BinaryInstancesParser_nonstrict.h \
		>Generated/BinaryInstancesParser.h

clean:
	rm -rf dist-newbuild Generated
	git checkout Generated/README.txt
