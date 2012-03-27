all: compile

compile: deps
	rebar compile
	rebar escriptize

deps:
	rebar get-deps

docs:
	$(EMACS) -q -batch -l docs/publish.el --eval='(handlebar-publish)'


.PHONY: docs
