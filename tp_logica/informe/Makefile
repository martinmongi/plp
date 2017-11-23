rtp2.pdf: main.pdf
	cp main.pdf rtp2.pdf

main.pdf: main.tex
	pdflatex main.tex

.PHONY: main.tex
main.tex:
	cat main_head.tex ../batalla_naval.pl main_end.tex > main.tex
