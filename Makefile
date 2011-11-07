
all: compile

compile: deps
	rebar compile
	rebar escriptize

deps:
	rebar get-deps
