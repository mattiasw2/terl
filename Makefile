# maybe get a real makefile: https://github.com/ninenines/erlang.mk

_ARCH := $(shell arch)

ifeq ($(_ARCH),armv7l)
_NO_NATIVE = --no_native
else
_NO_NATIVE =
endif

all:
	rebar compile

run:
	cd src; erl -config ../release/sys.config -sync log all

#run:
#	werl.exe -pa ./ebin -pa ./deps/*/ebin -config terl.config

# relx not working, searching for hipe and dialyzer
relx:	all
	~/erl-src/relx/relx

install:
	_rel/terl/bin/terl install

start:
	_rel/terl/bin/terl start

uninstall:
	_rel/terl/bin/terl uninstall

stop:
	_rel/terl/bin/terl stop

console:
	_rel/terl/bin/terl console


wc:
	wc src/shc*.erl


dialyzer:
	cd ~/data3/repos/terl/; rebar clean compile
	cd ~/data3/repos/terl/;  dialyzer $(_NO_NATIVE) --fullpath --verbose -Wunderspecs -Wunmatched_returns -Werror_handling ebin/*.beam

dialyzer_sample:
	cd ~/data3/repos/terl/samples/; erlc +debug_info *.erl
	cd ~/data3/repos/terl/samples/; dialyzer $(_NO_NATIVE) --fullpath --verbose -Wunderspecs -Wunmatched_returns -Werror_handling *.beam

dialyzer_over:
	cd ~/data3/repos/terl/; rebar clean compile
	cd ~/data3/repos/terl/;  dialyzer $(_NO_NATIVE) --fullpath --verbose -Wunderspecs -Wunmatched_returns -Werror_handling -Woverspecs ebin/*.beam

build_dialyzer:
	dialyzer $(_NO_NATIVE) --build_plt --apps erts kernel stdlib mnesia eunit
	dialyzer --add_to_plt --apps ./deps/*/ebin/*.beam

etags:
	etags src/*.erl include/*.hrl

erl:
	erl +sbwt none -pa ./ebin -pa ./deps/*/ebin -pa ./libs/sync/ebin


typer:
	cd ~/data3/repos/terl/; rebar compile
	cd ~/data3/repos/terl/; typer ebin/*.beam
