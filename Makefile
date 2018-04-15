REBAR3 ?= "$(CURDIR)/rebar3"
ERL := $(shell command -v erl 2> /dev/null)
GRIP := $(shell command -v grip 2> /dev/null)
GIT := $(shell command -v git 2> /dev/null)
GIT_COMMIT := $c

.PHONY: default compile shell ct dialyzer test clean dist-clean push compile-examples


ifndef ERL
	$(error "Could not found Erlang/OTP on this system.")
endif

	
default: compile

compile-examples:
	@ cd ./examples/client && $(REBAR3) compile

compile: compile-examples
	@ $(REBAR3) compile

doc: check-erl
	@ $(REBAR3) edoc

md2html: check-grip
	@ $(GRIP) README.md --export README.html --title "Overview"
	@ $(GRIP) ./wiki/overview.md --export ./wiki/overview.html --title "Overview"
	@ $(GRIP) ./wiki/build.md --export ./wiki/build.html --title "Build"
	@ $(GRIP) ./wiki/etcp_behavior.md --export ./wiki/etcp_behavior.html --title "ETCP Behavior"
	@ $(GRIP) ./examples/client/README.md --export ./examples/client/README.html --title "ETCP Example - Client"
	@ $(GRIP) ./examples/server/README.md --export ./examples/server/README.html --title "ETCP Example - Server"
	@ $(GRIP) ./examples/pool/README.md --export ./examples/pool/README.html --title "ETCP Example - Connection pool"

shell: compile
	@ $(ERL) -pa _build/default/lib/director/ebin \
	             _build/default/lib/etcp/ebin \
	             _examples/client/_build/default/lib/etcp_client/ebin 

ct:
	@ echo "Running tests:" && $(REBAR3) ct

dialyzer:
	@ echo "Running Dialyzer:" && $(REBAR3) dialyzer

test: compile-examples dialyzer ct

clean: check-erl
	@ $(REBAR3) clean

dist-clean: clean
	@ rm -rf _build rebar.lock *.html ./wiki/*.html ./examples/*/*.html doc

git-add: check-git compile test
	@echo "Adding files to git" && git add examples/*/src       \
	                                       examples/*/README.md \
	                                       include              \
	                                       src                  \
	                                       test                 \
	                                       wiki/*.md            \
	                                       .travis.yml          \
	                                       Makefile             \
	                                       README.md            \
	                                       rebar.config         \
	                                       rebar.config.script  \
	                                       rebar3


git-commit: check-git-commit git-add
	@ echo "Commiting changes to master" && git commit -m "$c"

push: git-commit
	@ echo "Pushing changes to master" && git push origin master
