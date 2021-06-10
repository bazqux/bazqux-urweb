all: update

ifeq ($(shell uname), Linux)
LINUX = 1
endif

HOSTNAME=$(shell hostname)
READER=./bin/Reader

URWEB = urweb @MLton fixed-heap 2g

uw_daemon: uw_daemon_stop
# если запустить urweb daemon из Emacs, то он сразу же прибивается,
# по-этому используем daemon
	PATH=`pwd`:$$PATH daemon -r -n uw_daemon -o local0.warning --chdir `pwd` -- make run_uw_daemon

uw_daemon_stop:
	pkill -9 -f "^daemon.* uw_daemon" || true
	pkill -fx "make run_uw_daemon" || true
	pkill -x urweb || true

run_uw_daemon:
	rm -f .urweb_daemon
	$(URWEB) -- daemon start
# реально отжирает память только при работе
	while pgrep urweb > /dev/null; do sleep 1; done

WEBPACK_HTML=dist/assets/scripts.html

update:
	@time make update_
	killall Reader
# daemon перезапустит

update_: $(WEBPACK_HTML) $(READER) gzip

gzip: images/favicon.ico.gz

%.gz: %
	gzip -9n <$< >$@

# daemon ...   -f   пускает в foreground, но при этом в syslog тоже пишет
run_reader: $(READER) gzip stop_reader
	@make -C crawler run_reader_internal
reader:
	LANG=C TZ=/usr/share/zoneinfo/UCT ./$(READER) -a 127.0.0.1 -p 8123 -t 32 -m 10485760
# делать -t меньше 32 не стоит -- есть медленные вызовы --
# readability/subscriptions, 32 одновременно врядли, но фиг знает
stop_reader:
	(daemon -n bqr --running && daemon -n bqr --stop) || true

$(READER): crawler/Generated/webapp.c crawler/*.hs crawler/*/*.hs crawler/bazqux.cabal crawler/cabal.project*
	@make -C crawler reader

URWEB_LOG=crawler/Generated/urweb.log

crawler/Generated/webapp.c: copy_cc *.ur* lib/*.ur* crawler/Generated/root.ur crawler/Generated/neverInline.urp
	@echo Compiling Ur/Web part...
	@$(URWEB) -- -ccompiler ./copy_cc reader >$(URWEB_LOG) \
		|| (head -n 300 $(URWEB_LOG) && exit 1)
# ограничиваем вывод urweb, чтобы emacs не тормозил

crawler/Generated/root.ur crawler/Generated/root.urs: root.ur root.urs
	cp root.urs crawler/Generated
	cat root.ur | $(CPP) >crawler/Generated/root.ur

crawler/Generated/neverInline.urp: backgroundRpc.ur settings.ur popups.ur pager.ur subItem.ur uim.ur share.ur feeds.ur articles.ur discovery.ur filters.ur appearance.ur account.ur pages.ur main.ur feedback.ur crawler/Generated/datatypesBinary.ur
	@perl -n -e 'use File::Basename; $$ARGV =~ s/.ur//; s/\(\*.*\*\)//; /.*( val|^val| fun|^fun) +([^ ()]+)/ && print "neverInline $$2\nneverInline ",ucfirst(basename($$ARGV)),"/$$2\n"' $^ | grep -vE " (.*/)?(show|a|b|p|li|redirect|link|title|kb|qa|qaX|ifP|queueCustomUpdate.*|queueCancellableCustomUpdate|validateEmail|setReferrer|.*binary_.*)$$" | sort | uniq | grep -ve "^$$" > $@

crawler/Generated/datatypesBinary.ur: crawler/Gen.hs
	@make -C crawler gen

$(WEBPACK_HTML): js/*.js css/*.styl *.js webpack.config.js package*.json node_modules
	npx webpack --mode=production --no-color

buildjs: $(WEBPACK_HTML)
	killall Reader
watch: node_modules
	npx webpack --mode=development --watch
node_modules:
	npm i

clean:
	rm -rf $(READER) dist node_modules
	make -C crawler clean

ur:
	rm -rf urweb
	git clone https://github.com/urweb/urweb
	cd urweb && git checkout 24cb4d7ac2fa5fb8b5af511f05ed9a673b6f35ec && git apply ../conf/urweb/Binary_ffi.xbodyString-unurlification.patch && git apply ../conf/urweb/dir-attr.patch && ./autogen.sh && ./configure && make && sudo make install
# в Ur/Web что-то напортачили и перестала работать десериализация binary,
# используем старую версию
	rm -rf urweb

nginx_preprocess:
	rm -rf dist/nginx
	mkdir -p dist/nginx
	cd conf/nginx && (for f in *.conf; do sed "s%# .*%%" $$f > ../../dist/nginx/$$f; done)

ifdef LINUX
ifeq ($(HOSTNAME), b.bazqux.com)
CPP=cpp -I. -ansi -traditional -P -DSTABLE_PWD=`pwd` -DPWD=`pwd` -DLINUX=1
else # master
CPP=cpp -I. -ansi -traditional -P -DSTABLE_PWD=/home/volodya/bazqux -DPWD=`pwd` -DLINUX=1
endif
else # Mac OS X
CPP=cpp -I. -P -C -DSTABLE_PWD=`pwd` -DPWD=`pwd` -Wno-invalid-pp-token
endif

ifeq ($(HOSTNAME), b.bazqux.com)
CPP+=-DBAZQUX_ADMIN=1
endif

ifdef LINUX

nginx: nginx_preprocess
	cat dist/nginx/$(HOSTNAME).conf | $(CPP) >dist/nginx/$(HOSTNAME).linux.conf
	sudo rm -rf /etc/nginx/sites-enabled/*
	sudo ln -fs `pwd`/dist/nginx/$(HOSTNAME).linux.conf /etc/nginx/sites-enabled/
	sudo mkdir -p /ssd/nginx/cache/tmp
	sudo chown www-data /ssd/nginx/cache /ssd/nginx/cache/tmp
	sudo nginx -s reload || sudo service nginx start

cron:
	@echo -e "\nВНИМАНИЕ: включи только на одной машине (фронтенде, чтобы никому не мешал)\n"
	sudo ln -fs `pwd`/crawler/cron/reindex_feeds.sh /etc/cron.daily/z-bazqux-reindex-feeds
	@echo -e "\nПорядок запуска скриптов cron.daily:\n"
	run-parts --test /etc/cron.daily
# Важно, чтобы в имени ссылки не было точек (даже в конце .sh -- cron их
# игнорирует). Ссылка начинается с 'z', чтобы скрипт запускался в конце.

cron_letsencrypt:
	sudo ln -fs `pwd`/conf/letsencrypt/renew.sh /etc/cron.daily/z-renew-letsencrypt
	@echo -e "\nПорядок запуска скриптов cron.daily:\n"
	run-parts --test /etc/cron.daily

else # Mac OS X

erlang:
	CFLAGS=-O0 ./configure --enable-hipe --enable-smp-support --enable-threads --enable-kernel-poll --enable-darwin-64bit

NGINX=/usr/local/openresty/nginx/sbin/nginx
nginx: nginx_preprocess
	cat dist/nginx/$(HOSTNAME).conf | $(CPP) >dist/nginx/$(HOSTNAME).mac.conf
	sudo ln -fs `pwd`/dist/nginx/$(HOSTNAME).mac.conf /opt/local/etc/nginx/sites-enabled/
	sudo mkdir -p /tmp/nginx/cache/tmp
	sudo chown nobody /tmp/nginx/cache /tmp/nginx/cache/tmp
	sudo $(NGINX) -s reload || sudo $(NGINX)

cron:
	sudo ln -fs `pwd`/crawler/cron/backup_discovery_mac.sh /opt/local/etc/cron.daily/z-bazqux-backup_discovery
#	run-parts /opt/local/etc/cron.daily
#	--test нет, просто запускает

endif # Mac OS X

munin:
	make -C crawler ../bin/MuninPlugin
	sudo ln -fs `pwd`/crawler/munin/bazqux /etc/munin/plugin-conf.d/
	sudo ln -fs `pwd`/crawler/munin/crawler_cpu_usage /etc/munin/plugins/
	sudo ln -fs `pwd`/crawler/munin/crawler_mem_usage /etc/munin/plugins/
	sudo ln -fs `pwd`/crawler/munin/riak_gets_puts /etc/munin/plugins/
	sudo ln -fs `pwd`/crawler/munin/hddtemp_smartctl /etc/munin/plugins/
	sudo ln -fs `pwd`/crawler/munin/elasticsearch_* /etc/munin/plugins/
	sudo ln -fs `pwd`/bin/MuninPlugin /etc/munin/plugins/active_paid_users
	sudo ln -fs `pwd`/bin/MuninPlugin /etc/munin/plugins/active_trial_users
	sudo ln -fs `pwd`/bin/MuninPlugin /etc/munin/plugins/crawler_scan_stats
	sudo ln -fs `pwd`/bin/MuninPlugin /etc/munin/plugins/crawler_wc_stats
	sudo ln -fs `pwd`/bin/MuninPlugin /etc/munin/plugins/crawler_ws_stats
	sudo ln -fs `pwd`/bin/MuninPlugin /etc/munin/plugins/crawler_rs_stats
	sudo ln -fs `pwd`/bin/MuninPlugin /etc/munin/plugins/crawler_rc_stats
	sudo ln -fs `pwd`/bin/MuninPlugin /etc/munin/plugins/crawler_ds_stats
	sudo ln -fs `pwd`/bin/MuninPlugin /etc/munin/plugins/crawler_dc_stats
	sudo ln -fs `pwd`/bin/MuninPlugin /etc/munin/plugins/crawler_pc_stats
	sudo /etc/init.d/munin-node restart

ImageMagick:
	sudo cp conf/ImageMagick/policy.xml /etc/ImageMagick-6/
