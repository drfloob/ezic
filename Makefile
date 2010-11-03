ERLC_WARNINGS = -W1
MNESIA_DIR = db
RUN_INIT = 
DEBUG = +debug_info -Ddebug
TEST = 

all : clean compile

nowarn : ERLC_WARNINGS = -W0
nowarn : clean compile

test : ERLC_WARNINGS = -W0
test : TEST := -DTEST
test : all
	erlc $(ERLC_WARNINGS) $(TEST) -o ./ebin ./tests/*.erl
	erl -noshell -pa ebin -s test_master start -s erlang halt

compile :
	erlc $(DEBUG) $(ERLC_WARNINGS) $(OPTIONS) -o ./ebin ./src/*.erl
	-erl -noshell -pa ebin -s erldev make_app . -s erlang halt

clean : 
	-@rm ebin/*


run :
	erl -pa ebin -mnesia dir $(MNESIA_DIR) $(RUN_INIT)

build : DEBUG = 
build : all