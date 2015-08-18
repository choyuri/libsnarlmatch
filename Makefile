include tools.mk

.PHONY: deps

compile:
	$(REBAR) compile

deps:
	$(REBAR) get-deps

clean:
	$(REBAR) clean

qc:
	rebar3 as eqc eqc
