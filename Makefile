ERLC_WARNINGS := -W1
MNESIA_DIR := db
RUN_INIT := -run ezic_db init
DEBUG := -DNODEBUG -Ddebug
TEST := 
TZSET := northamerica
SHELL := /bin/bash
COMPILE := ./src/*.erl

all : clean compile


nowarn : ERLC_WARNINGS = -W0
nowarn : clean compile


test : ERLC_WARNINGS := -W0
test : TEST := -DTEST 
test : RUN_INIT += -run test all -s erlang halt
test : COMPILE += ./test/*.erl
test : all run


compile :
	erlc $(DEBUG) $(TEST) $(ERLC_WARNINGS) $(OPTIONS) -o ./ebin $(COMPILE)
	-erl -noshell -pa ebin -s erldev make_app . -s erlang halt

clean : 
	-@rm ebin/*
	-@rm erl_crash.dump


run :
	erl -pa ebin -mnesia dir $(MNESIA_DIR) $(RUN_INIT)


devstart : RUN_INIT += -s ezic dev -s erlang halt
devstart : all run


tzdata :
	-cat priv/tzdata/$(TZSET)  | sed '/\s*\#/d' |  sed '/^\s*$$/d' | less -S


diff :
	-git diff > /tmp/ezic.tmp.diff
	emacs /tmp/ezic.tmp.diff


debug : DEBUG += +debug_info
debug : all run