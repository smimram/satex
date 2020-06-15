all:
	$(MAKE) -C src

install:
	$(MAKE) -C src $@

test: all
	$(MAKE) -C test

ci:
	git ci . -m "Worked on satex."
	git push
