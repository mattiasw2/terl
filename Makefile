_ARCH := $(shell arch)

ifeq ($(_ARCH),armv7l)
_NO_NATIVE = --no_native
else
_NO_NATIVE =
endif


all:
	rebar compile

run:
	werl.exe -pa ./ebin -pa ./deps/*/ebin -config cleaner.config

relx:	all
	~/erl-src/relx/relx

install:
	_rel/cleaner/bin/cleaner install

start:
	_rel/cleaner/bin/cleaner start

uninstall:
	_rel/cleaner/bin/cleaner uninstall

stop:
	_rel/cleaner/bin/cleaner stop

console:
	_rel/cleaner/bin/cleaner console


wc:
	wc src/shc*.erl


dialyzer:
	cd ~/data3/erlang/shc/; rebar clean compile
	cd ~/data3/erlang/shc/;  dialyzer $(_NO_NATIVE) --fullpath --verbose -Wunderspecs -Wunmatched_returns -Werror_handling ebin/*.beam

dialyzer_over:
	cd ~/data3/erlang/shc/; rebar clean compile
	cd ~/data3/erlang/shc/;  dialyzer $(_NO_NATIVE) --fullpath --verbose -Wunderspecs -Wunmatched_returns -Werror_handling -Woverspecs ebin/*.beam

build_dialyzer:
	dialyzer $(_NO_NATIVE) --build_plt --apps erts kernel stdlib mnesia eunit
	dialyzer --add_to_plt --apps ./deps/*/ebin/*.beam

etags:
	etags src/*.erl include/*.hrl

erl:
	erl +sbwt none -pa ./ebin -pa ./deps/*/ebin -pa ./libs/sync/ebin


typer:
	cd ~/data3/erlang/shc/; rebar compile
	cd ~/data3/erlang/shc/; typer ebin/*.beam
