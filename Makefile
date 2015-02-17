DESTDIR=/usr/local

all: libopenal-racket.rkt main.rkt
	raco make main.rkt

install: all
	mkdir -pv $(DESTDIR)/share/racket/pkgs/libopenal-racket
	install -m 0644 *.rkt $(DESTDIR)/share/racket/pkgs/libopenal-racket
	install -m 0644 LICENSE $(DESTDIR)/share/racket/pkgs/libopenal-racket
	cp -Rv compiled $(DESTDIR)/share/racket/pkgs/libopenal-racket

link: install
	raco link -i $(DESTDIR)/share/racket/pkgs/libopenal-racket

clean:
	rm -Rv compiled/
