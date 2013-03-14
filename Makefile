#!make

all: get-deps compile

get-deps:
	rebar get-deps

compile:
	rebar compile

eunit: compile
	rebar skip_deps=true eunit -k

ct: compile
	rebar skip_deps=true ct apps=erpc
	
test: compile eunit ct

clean:
	rebar clean
