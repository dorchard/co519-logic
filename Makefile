default: quick

quick:
	pdflatex notes.tex

full:
	pdflatex notes.tex
	bibtex notes
	pdflatex notes.tex
	pdflatex notes.tex
