REBAR3 ?= "rebar3"
ERL := $(shell command -v erl 2> /dev/null)


ifndef ERL
	$(error "Could not found Erlang/OTP on this system.")
endif

	
default: compile

compile:
	@ $(REBAR3) compile

doc:
	@ $(REBAR3) edoc

shell: compile
	@ $(ERL) -pa _build/default/lib/director/ebin \
	             _build/default/lib/etcp/ebin 

ct:
	@ echo "Running tests:" && $(REBAR3) ct

dialyzer:
	@ echo "Running Dialyzer:" && $(REBAR3) dialyzer

test: dialyzer ct

clean:
	@ $(REBAR3) clean

dist-clean: clean
	@ rm -rf _build rebar.lock doc


.PHONY: default compile doc shell ct dialyzer test clean dist-clean
