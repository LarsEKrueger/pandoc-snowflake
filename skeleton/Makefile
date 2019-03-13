PANDOC=pandoc
NCFILT=pandoc-narrative-charts
VWFILT=pandoc-vimwiki
SFFILT=pandoc-snowflake
KINDLEGEN=kindlegen

NAMEBASE=@NAMEBASE@
OUTPUT=output

DEPS=\
  title.mdwiki \
  @CHARACTERFILES@

design: ${OUTPUT}/design_${NAMEBASE}.html
html: ${OUTPUT}/book_${NAMEBASE}.html
epub: ${OUTPUT}/${NAMEBASE}.epub
mobi: ${OUTPUT}/${NAMEBASE}.mobi

all: design html epub mobi

${OUTPUT}/book_${NAMEBASE}.html: book.mdwiki ${DEPS} Makefile include/book_header.html
	mkdir -p output
	${PANDOC} --from markdown --to html --toc --filter ${VWFILT} -s -H include/book_header.html -o $@ $<

${OUTPUT}/design_${NAMEBASE}.html: design.mdwiki ${DEPS} include/design_header.html Makefile
	mkdir -p output
	${PANDOC} --from markdown --to html --filter ${VWFILT} --filter ${NCFILT} --filter ${SFFILT} -s -H include/design_header.html -o $@ $<

${OUTPUT}/${NAMEBASE}.epub: book.mdwiki ${DEPS} Makefile include/epub.css # title-image.jpg
	mkdir -p output
	${PANDOC} --from markdown --to epub3 --toc \
	  --filter ${VWFILT} \
	  -s \
	  --epub-stylesheet=include/epub.css \
	  -o ${OUTPUT}/${NAMEBASE}.epub \
	  book.mdwiki
	  # --epub-cover-image=title-image.jpg \

${OUTPUT}/${NAMEBASE}.mobi: ${OUTPUT}/${NAMEBASE}.epub Makefile
	mkdir -p output
	-which ${KINDLEGEN} && ${KINDLEGEN} ${OUTPUT}/${NAMEBASE}.epub
