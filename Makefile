
%.html: %.adoc
	asciidoc $< > $@

all: index.html
