all: update

SHELL = bash

browse:
	http_proxy= firefox http://coreader.com

uw_daemon:
	PATH=`pwd`:$$PATH urweb @MLton fixed-heap 1.5g -- daemon start
# реально отжирает память только при работе

#run: compile nginx reader

run: run_reader run_crawler

stop: stop_reader stop_crawler

run_wo_sudo: compile reader

restart_reader: reader.exe stop_reader run_reader

update: reader.exe gzip
	killall reader.exe
# daemon перезапустит

gzip: css/basic.css.gz js/utils.js.gz js/libs.js.gz

%.gz: %
	gzip -9 <$< >$@

# daemon ...   -f   пускает в foreground, но при этом в syslog тоже пишет
run_reader: stop_reader reader.exe gzip
	LANG=C TZ=/usr/share/zoneinfo/UCT daemon -r -a 10 -A 50 -L 10 -n bqr -o local0.warning --chdir `pwd` -- ./reader.exe -a 127.0.0.1 -p 8123 -t 32
# siege 1000 -d 1 keep-alive
# -N2 1270 0.28  99.08
# -N3 1540 0.14  99.05
# -N4 1630 0.11  99.07 ***   -t 16
# -N4 1750 0.07  99.06 ***** -t 32
# -N5 1580 0.13  99.06
# -N8  600 1.03 100 (увеличивает cpu 300->540, но уменьшает rps 1600->600)
# с -t 32 почти 3 гига съел -- но это без кеша
stop_reader:
	(daemon -n bqr --running && daemon -n bqr --stop) || true

run_crawler:
	$(MAKE) -C crawler run_crawler
#	rm -f crawler/stop
#	$(MAKE) -s -C crawler crawler
stop_crawler:
	$(MAKE) -C crawler stop_crawler
#	touch crawler/stop

reader:
	(sleep 1 && $(MAKE) run_crawler) &
	$(MAKE) run_reader || $(MAKE) stop_crawler
	$(MAKE) stop_crawler

reader.exe: *.ur* lib/* crawler/*.hs crawler/Generated/hsffi.urp crawler/Generated/BinaryInstancesParser.h
	mkdir -p dist
	@echo script /js/utils_v`date +%s`.js >crawler/Generated/utils_js.urp
	PATH=`pwd`:$$PATH urweb @MLton fixed-heap 1.5g -- reader # 2>&1
# | \
# 		sed "s/:\([0-9]*\):\([0-9]*\)-\([0-9]*\):\([0-9]*\)/:\1.\2-\3.\4/" && \
# 		exit $${PIPESTATUS[0]}
crawler:
	$(MAKE) -C crawler
crawler/Generated/hsffi.urp: crawler/Gen.hs
	cd crawler && runghc Gen
crawler/Generated/BinaryInstancesParser.h: crawler/ParserTypes.hs
	cd crawler && derive ParserTypes.hs && \
		sed 's%\([^ ]* <- get\)%!\1%' Generated/BinaryInstancesParser_nonstrict.h \
		>Generated/BinaryInstancesParser.h

clean:
	rm -rf reader.exe dist
	make -C crawler clean

ur:
	rm -rf urweb
	hg clone http://hg.impredicative.com/urweb
	cd urweb && ./autogen.sh && ./configure && make && sudo make install
	rm -rf urweb

ifeq ($(shell uname), Linux)
LINUX = 1
endif

HOSTNAME=$(shell hostname)

dist/nginx/%.conf: conf/nginx/%.conf
	@mkdir -p dist/nginx
	@sed "s%# .*%%" $< > $@
nginx: dist/nginx/$(HOSTNAME).conf dist/nginx/header.conf dist/nginx/frontend_choose.conf dist/nginx/frontend_getUserIdBySession.conf dist/nginx/frontend.conf dist/nginx/api.conf dist/nginx/common.conf dist/nginx/elasticsearch.conf dist/nginx/reader.conf dist/nginx/beta.conf dist/nginx/crawler.conf dist/nginx/nginx_status.conf dist/nginx/maintenance.conf

ifdef LINUX

nginx:
	cat dist/nginx/$(HOSTNAME).conf | cpp -I. -P -C -DPWD=`pwd` -DLINUX=1 >dist/nginx/$(HOSTNAME).linux.conf
	sudo rm -rf /etc/nginx/sites-enabled/*
	sudo ln -fs `pwd`/dist/nginx/$(HOSTNAME).linux.conf /etc/nginx/sites-enabled/
	if [ ! -e /var/run/nginx.pid ]; then sudo nginx; fi
	sudo nginx -s reload

else # Mac OS X

erlang:
	CFLAGS=-O0 ./configure --enable-hipe --enable-smp-support --enable-threads --enable-kernel-poll --enable-darwin-64bit

NGINX=/usr/local/openresty/nginx/sbin/nginx
nginx:
	cat dist/nginx/$(HOSTNAME).conf | cpp -I. -P -C -DPWD=`pwd` -Wno-invalid-pp-token >dist/nginx/$(HOSTNAME).mac.conf
	sudo ln -fs `pwd`/dist/nginx/$(HOSTNAME).mac.conf /opt/local/etc/nginx/sites-enabled/
# 	if [ ! -e /opt/local/var/run/nginx/nginx.pid ]; then sudo nginx; fi
# 	sudo nginx -s reload
	if [ ! -e /usr/local/openresty/nginx/logs/nginx.pid ]; then sudo $(NGINX); fi
	sudo $(NGINX) -s reload
# wget http://openresty.org/download/ngx_openresty-1.2.7.8.tar.gz
# ./configure --with-luajit && make && make install
# /usr/local/openresty/nginx/conf/nginx.conf:
#   include /opt/local/etc/nginx/sites-enabled/*;

endif # Mac OS X

munin:
	make -C crawler MuninPlugin
	sudo ln -fs `pwd`/crawler/munin/elasticsearch /etc/munin/plugin-conf.d/
	sudo ln -fs `pwd`/crawler/munin/elasticsearch_* /etc/munin/plugins/
	sudo ln -fs `pwd`/crawler/munin/crawler_cpu_usage /etc/munin/plugins/
	sudo ln -fs `pwd`/crawler/munin/crawler_mem_usage /etc/munin/plugins/
	sudo ln -fs `pwd`/crawler/MuninPlugin /etc/munin/plugins/active_paid_users
	sudo ln -fs `pwd`/crawler/MuninPlugin /etc/munin/plugins/active_trial_users
	sudo ln -fs `pwd`/crawler/MuninPlugin /etc/munin/plugins/crawler_scan_stats
	sudo ln -fs `pwd`/crawler/MuninPlugin /etc/munin/plugins/crawler_wc_stats
	sudo ln -fs `pwd`/crawler/MuninPlugin /etc/munin/plugins/crawler_ws_stats
	sudo ln -fs `pwd`/crawler/MuninPlugin /etc/munin/plugins/crawler_rs_stats
	sudo ln -fs `pwd`/crawler/MuninPlugin /etc/munin/plugins/crawler_rc_stats
	sudo ln -fs `pwd`/crawler/MuninPlugin /etc/munin/plugins/crawler_ds_stats
	sudo ln -fs `pwd`/crawler/MuninPlugin /etc/munin/plugins/crawler_dc_stats
	sudo ln -fs `pwd`/crawler/MuninPlugin /etc/munin/plugins/crawler_pc_stats
# 	sudo ln -fs /usr/share/munin/plugins/if_ /etc/munin/plugins/if_eth1
	sudo ln -fs `pwd`/crawler/munin/nginx_combined /etc/munin/plugins/nginx_combined_public.l.bazqux.com
# 	sudo ln -fs `pwd`/crawler/munin/nginx_combined /etc/munin/plugins/nginx_combined_public.crawler.bazqux.com
# 	sudo ln -fs `pwd`/crawler/munin/nginx_combined /etc/munin/plugins/nginx_combined_public.e.bazqux.com
	sudo ln -fs `pwd`/crawler/munin/riak_gets_puts /etc/munin/plugins/
	sudo /etc/init.d/munin-node restart
slow-connection:
	sudo ipfw pipe 1 config bw 15KByte/s delay 200
	sudo ipfw add 1 pipe 1 src-port 8123
# 	sudo ipfw add 1 pipe 1 dst-port 80 # src-port 8123
# 	sudo ipfw add 1 pipe 1 dst-port 443 # src-port 8123

disable-slow-connection:
	sudo ipfw delete 1

HTTPERF=httperf --hog --server coreader.com --num-conns=20 --num-calls=1000 --rate 100 --timeout=5.0

httperf:
	make -s httperf_s | grep "\(Request rate\)\|==="

httperf_s:
ifndef LINUX
#	ulimit -n 1024
endif
	echo ========== NGINX ==========
	$(HTTPERF) --uri=/css/basic.css
# 23000 (а с --rate 100 только 10000)
	echo ========== Главная страница ==========
	$(HTTPERF)
#  6000
	echo ========== Дерево комментариев ==========
	for x in `seq 1 10`; do $(HTTPERF) --uri=/feedMsgForest/PublicUser/http.3A.2F.2Ffeeds2.2Efeedburner.2Ecom.2F5nak/5/4 --add-header="Accept-Encoding: gzip, deflate\\n"; done;

# /userSubscriptions/PublicUser

#  1000
#  CPU:
#    postgres  : 176
#    coreader  : 280
#    nginx     :  50
#    httperf   : 100
#    idle/other: 200

siege:
	siege -c 1000 -t 10m -d 1 -R siegerc http://bazqux.com/feedMsgForest/PublicUser/http.3A.2F.2Ffeeds2.2Efeedburner.2Ecom.2F5nak/5/4 | grep -v "HTTP/1.1 200"
# держит, 250% cpu, 1krps
#	siege -c 1000 -t 10m -d 2 -R siegerc http://bazqux.com/userSubscriptions/PublicUser | grep -v "HTTP/1.1 200"
# держит, но грузит 250% cpu на маке
