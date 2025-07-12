.PHONY: all compile clean test bench shell

all: compile

compile:
	rebar3 compile

clean:
	rebar3 clean

test:
	rebar3 ct

bench: compile
	rebar3 as bench compile
	erlc -o _build/bench/lib/snow/ebin bench/snow_bench.erl
	erl -pa _build/bench/lib/*/ebin -noshell -s snow_bench run -s init stop

shell:
	rebar3 shell

dialyzer:
	rebar3 dialyzer