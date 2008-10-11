POD2HTML = pod2html --css "http://suika.fam.cx/www/style/html/pod.css" \
  --htmlroot "http://suika.fam.cx/www/markup/html/whatpm"

all: wttjs.html

%.html: %.pl
	$(POD2HTML) $< > $@

## License: Public Domain.
## $Date: 2008/10/11 05:37:30 $
