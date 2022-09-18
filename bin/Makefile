P2H = ../local/p2h
CURL = curl

all: build

build: wttjs.html

$(P2H): ../local
	$(CURL) -sSfL https://raw.githubusercontent.com/manakai/manakai.github.io/master/p2h > $@
	chmod u+x $@

../local:
	mkdir -p ../local

%.html: %.pl $(P2H)
	$(P2H) webidl2tests $< > $@

## License: Public Domain.
