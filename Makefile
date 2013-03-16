.PHONY: test

all:
	./rebar compile

clean:
	./rebar clean

test: all
	./rebar eunit

repl:
	erl -pa ebin
