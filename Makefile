REBAR=./rebar

.PHONY: all

all:
	$(REBAR) compile

clean:
	$(REBAR) clean

test:
	$(REBAR) skip_deps=true eunit
