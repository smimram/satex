all:
	$(MAKE) -C src

test: all
	$(MAKE) -C test

ci:
	git ci . -m "Worked on satex."
	git push
