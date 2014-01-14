ifeq ($(shell uname), Linux)
LINUX = 1
endif

BIN = dist/build/crawler/crawler

all: Queue

include Makefile.ghc

BP = dist/build/crawler

run: $(BIN) crawler

GHC_OPTS = -rtsopts -threaded -hidir $(BP) -odir $(BP) -fwarn-incomplete-patterns # -prof -fprof-auto
ifdef LINUX
GHC_OPTS += -O2
endif

test: Queue
	time make update 2>&1 | gawk '{ print strftime("%H:%M:%S"), $$0; fflush(); }' >1

parse_server: ParseServer
	./ParseServer +RTS -M1G -N4 -T -sstderr -I0 -A10m -K32M # -p -h -L80 -i5

update: Queue
	./Queue +RTS -M5G -N4 -T -sstderr -I0 -A10m -K32M # -p -h -L80 -i5
# -xc
# -xt  -- невероятно затормаживает профилирование
# видимо из-за того, что много ниток и он офигевает бегать по их стекам
# с -H1G стал отжирать 2.1G, хотя до этого максимум было 900, а среднее 600
# и стал больше жрать проц, при этом кол-во полезной работы не изменилось
# появилась фрагментация на гиг
# productivity 50->33

# -N8 -qg падает
# c -N8 вроде не падает, но медленнее -N4, хотя и жрет много проца
# -N8 -qb кажется, жрет чуть меньше памяти и CPU и чуть шустрее
# убирание -I10 практически ничего не меняет
# -N8 -N2 вроде тоже не влияет (при наличии -qg) -- упираемся в диск
# -N2 с -qg/без -qg примерно одно и то же, только cpu больше кушает
# без -S -sstderr  может грамулечку шустрее
# -N4 вроде пошустрее -N2 (и -N8), но проц жрет
# -G3 больше памяти жрет и медленнее работает
# -N4 -qg жрет раза в 2.5 меньше CPU, но работает чуть медленнее
# -N4 -qg1 жрет CPU как -qg, но кажется еще медленнее выгребает

# кажется, -N4 без отрубания параллельного сборщика -- оптимальный вариант

Generated/hsffi.urp: Gen.hs
	runghc Gen

Queue: *.hs Lib/*.hs Generated/hsffi.urp Generated/BinaryInstancesParser.h
	mkdir -p $(BP)
	$(GHC) $(GHC_OPTS) Queue -main-is Queue.test

ParseServer: *.hs Lib/*.hs Generated/hsffi.urp Generated/BinaryInstancesParser.h
	mkdir -p $(BP)
	$(GHC) $(GHC_OPTS) ParseServer -main-is ParseServer.runParseServer

test_parser: Parser
	./Parser # +RTS -xc
Parser: *.hs Lib/*.hs Generated/hsffi.urp
	mkdir -p $(BP)
	$(GHC) $(GHC_OPTS) Parser -main-is Parser.perfTest
check_negative_unreads: CheckNegativeUnreads
	time ./CheckNegativeUnreads +RTS -N4 -M4G -A10m >negative_unreads.txt 2>&1 &
CheckNegativeUnreads: *.hs Lib/*.hs Generated/hsffi.urp
	mkdir -p $(BP)
	$(GHC) $(GHC_OPTS) UrCalls -o CheckNegativeUnreads \
		-main-is UrCalls.checkNegativeUnreads
enclosure_stats: EnclosureStats
	time ./EnclosureStats +RTS -A20M
# ghci      548sec
# -O0       266
# -A10M     244
# -A10M -O2 128
EnclosureStats: *.hs Lib/*.hs Generated/hsffi.urp
	mkdir -p $(BP)
	$(GHC) $(GHC_OPTS) -O2 -o EnclosureStats UrCalls -main-is UrCalls.enclosureStats
MuninPlugin: *.hs Lib/*.hs Generated/hsffi.urp
	mkdir -p $(BP)
	$(GHC) $(GHC_OPTS) MuninPlugin -main-is MuninPlugin.main
PubSubHubbub: *.hs Lib/*.hs
	mkdir -p $(BP)
	$(GHC) $(GHC_OPTS) PubSubHubbub -main-is PubSubHubbub.pubSubHubbubClient
ClearUnusedPosts: *.hs Lib/*.hs Generated/hsffi.urp
	mkdir -p $(BP)
	$(GHC) $(GHC_OPTS) -o ClearUnusedPosts Queue -main-is Queue.clearUnusedPosts
TestCompression: *.hs Lib/*.hs Generated/hsffi.urp
	mkdir -p $(BP)
	$(GHC) $(GHC_OPTS) -o TestCompression UrCalls -main-is UrCalls.testCompression
	LD_LIBRARY_PATH=/usr/local/lib ./TestCompression
test_dnscache:
	mkdir -p $(BP)
	$(GHC) $(GHC_OPTS) -O2 -o TestDnsCache Lib.DnsCache -main-is Lib.DnsCache.massResolve
	cat domains | time ./TestDnsCache +RTS -N4 -M1G -A10M # -sstderr # -p -h
#
# 	cat TestDnsCache.hp | hp2ps -d -c >TestDnsCache.ps
# 	ps2pdf TestDnsCache.ps
# 	scp TestDnsCache.pdf "b:/home/volodya/bazqux/web/stuff/"
# 	rm TestDnsCache.ps TestDnsCache.pdf TestDnsCache

run_pubsubhubbub: PubSubHubbub
	daemon -r -n pshb -o local0.warning --chdir `pwd` -- ./PubSubHubbub +RTS -I0 -M1.5G -A10M -N4
stop_pubsubhubbub:
	daemon -n pshb --stop

run_crawler: Queue
	rm -f stop
	daemon -r -n cr -o local0.warning --chdir `pwd` -- sh -c "ulimit -c unlimited; while true; do make update; done;"

run_parse_server: ParseServer
	daemon -r -n ps -o local0.warning --chdir `pwd` -- sh -c "ulimit -c unlimited; while true; do make parse_server; done;"

stop_parse_server:
	daemon -n ps --stop || true
	killall ParseServer

stop_crawler:
	daemon -n cr --stop || true
	touch stop

$(BIN): *.hs dist
	$(CABAL) -v0 build

dist: crawler.cabal
	$(CABAL) configure

clean:
	rm -rf dist DBTest Queue Parser *.o *.hi */*.o */*.hi

dependencies: ghc packages cabal-install cabal-packages

packages:
	sudo apt-get install

ifndef LINUX
CABAL_INTALL_OPTS += --extra-include-dirs=/opt/local/include --extra-lib-dirs=/usr/lib --extra-lib-dirs=/opt/local/lib
GHC_OPTS += -I/opt/local/include /usr/lib/libiconv.dylib -L/opt/local/lib  -optl-Wl,-no_pie -optl-Wl,-no_compact_unwind
endif

install-package:
	$(CABAL) $(CABAL_INTALL_OPTS)  --reinstall --force-reinstall install $(AUTO_ALL) $(P)

AUTO_ALL = --ghc-option=-auto-all

cabal-packages:
	$(CABAL) install $(AUTO_ALL) containers template-haskell --force-reinstalls
# для containers-0.5.0.1 требует пересобрать template-haskell
# и говорит, что пакет ghc-7.6.1 сломается, но вроде все работает
	$(CABAL) install $(AUTO_ALL) utf8-string mtl syb zlib happy alex #certificate-1.2.8 cryptocipher-0.3.5
	$(CABAL) install xml-conduit --ghc-option -XRankNTypes
	$(CABAL) $(CABAL_INTALL_OPTS) install $(AUTO_ALL) zip-archive pretty-show network fast-tagsoup text text-icu regex-tdfa hsdns-cache base16-bytestring warp language-javascript url timerep mime-mail ekg derive SafeSemaphore http-conduit-downloader authenticate authenticate-oauth wai-extra
	make riak-pkg
	ghc-pkg hide monads-tf
# откуда-то взялся monads-tf, который конфликтует с mtl
#
#	make auth-pkgs
# убрал authenticate-oauth.cabal верхние границы зависимостей

riak-pkg:
# ghc 7.6.1:
# riak выдает
# Unacceptable result type in foreign declaration: IO CInt
# поправлено в https://github.com/bos/riak-haskell-client
# но так и не выложено на hackage
	cd Lib/riak-protobuf-0.16.0.1/ && \
		$(CABAL_) clean && $(CABAL) $(AUTO_ALL) install --force-reinstalls
	cd Lib/riak-0.7.0.1/ && \
		$(CABAL_) clean && $(CABAL) $(AUTO_ALL) install # --force-reinstalls
auth-pkgs:
#	$(CABAL) install $(AUTO_ALL) RSA-1.2.1.0
	$(CABAL) install $(AUTO_ALL) authenticate-oauth


riak_linux:
	sudo sh -c 'echo "ulimit -n 60000" > /etc/default/riak'
	sudo sh -c '(echo "ulimit -n 60000"; echo "ulimit -l unlimited") > /etc/default/es'

hslib:
	$(MAKE) -C ../hslib


ifndef LINUX
clean_db:
	ulimit -n 10000
	make stop_crawler
	killall java || true
	~/riak-1.3.1/bin/riak stop || true
	rm -rf ~/riak-1.3.1/data/*
	rm -rf ~/es/data/*
	~/es/bin/elasticsearch || true
	~/riak-1.3.1/bin/riak start
	sleep 10
	rm stop
	../conf/riak/allow_mult.sh
	../conf/es/msg_index_setup.sh
	killall reader.exe
endif

strict_derive:
	sed 's%\([^ ]* <- get\)%!\1%' Generated/BinaryInstances_nonstrict.h \
		>Generated/BinaryInstances.h
parser_bin Generated/BinaryInstancesParser.h: ParserTypes.hs
	derive ParserTypes.hs
	sed 's%\([^ ]* <- get\)%!\1%' Generated/BinaryInstancesParser_nonstrict.h \
		>Generated/BinaryInstancesParser.h
