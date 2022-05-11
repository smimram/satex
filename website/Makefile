all: index.html
	@$(MAKE) -C .. fig && mkdir -p fig && cp ../fig/*.svg fig

index.html: ../README.md
	pandoc -s -c github.css --metadata title="SaTeX" --metadata author="Samuel Mimram" -N --toc $< -o $@
