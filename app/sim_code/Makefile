ERLC_OPTS =

SOURCES=$(wildcard src/*.erl)
BEAMS=$(SOURCES:src/%.erl=ebin/%.beam)

all: $(BEAMS)

ebin/%.beam: src/%.erl  | ebin
	erlc -o ebin $<

clean:
	rm -rf ebin

ebin:
	mkdir $@

