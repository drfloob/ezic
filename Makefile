ERLC_WARNINGS := -W1
MNESIA_DIR := db
RUN_INIT := 
DEBUG := +debug_info -Ddebug
TEST := 
TZSET := northamerica
SHELL := /bin/bash

all : clean compile

nowarn : ERLC_WARNINGS = -W0
nowarn : clean compile

test : ERLC_WARNINGS := -W0
test : TEST := -DTEST
test : all
	erlc $(ERLC_WARNINGS) $(TEST) -o ./ebin ./test/*.erl
	erl -noshell -pa ebin -run test all -s erlang halt

compile :
	erlc $(DEBUG) $(TEST) $(ERLC_WARNINGS) $(OPTIONS) -o ./ebin ./src/*.erl
	-erl -noshell -pa ebin -s erldev make_app . -s erlang halt

clean : 
	-@rm ebin/*


run :
	erl -pa ebin -mnesia dir $(MNESIA_DIR) $(RUN_INIT)

build : DEBUG = 
build : all


devstart : RUN_INIT = -s ezic dev_start
devstart : all run


tzdata :
	-cat priv/tzdata/$(TZSET)  | sed '/\s*\#/d' |  sed '/^\s*$$/d' | less -S


diff :
	-git diff > /tmp/ezic.tmp.diff
	emacs /tmp/ezic.tmp.diff