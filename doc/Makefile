MARKDOWNS=$(wildcard *.md)
PDFS=$(MARKDOWNS:md=pdf)
ASSETS=assets/ambiata.png
TEMPLATE=templates/pdf.tex

all: $(PDFS)

%.pdf: %.md $(TEMPLATE) $(ASSETS)
	pandoc -V geometry:margin=2cm --template=$(TEMPLATE) $< -o $@

clean:
	rm -f $(PDFS)

.PHONY: clean
