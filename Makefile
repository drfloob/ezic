ERLC_WARNINGS = -W1
MNESIA_DIR = db
RUN_INIT = 

all : clean compile

nowarn : ERLC_WARNINGS = -W0
nowarn : clean compile

test : ERLC_WARNINGS = -W0
test : OPTIONS := $(OPTIONS) -DTEST
test : all
	erlc $(ERLC_WARNINGS) $(OPTIONS) -o ./ebin ./tests/*.erl
	erl -noshell -pa ebin -s test_master start -s erlang halt

compile :
	erlc $(ERLC_WARNINGS) $(OPTIONS) -o ./ebin ./src/*.erl
	-erl -noshell -pa ebin -s erldev make_app . -s erlang halt

clean : 
	-@rm ebin/*


run :
	erl -pa ebin -mnesia dir $(MNESIA_DIR) $(RUN_INIT)