FILENAME=defense
ENGINE=pdflatex ### for higher quality compiler with xelatex; can change to pdflatex or lualatex
all:
	$(ENGINE) -shell-escape $(FILENAME).tex
	bibtex $(FILENAME).aux
	$(ENGINE) -shell-escape $(FILENAME).tex
	$(ENGINE) -shell-escape $(FILENAME).tex


RM_LIST=aux log dvi idx blg bbl rtf out vrb snm toc nav
RM_STR:=$(RM_LIST:%=*.%) $(RM_LIST:%=Chapters/*.%)
clean:
	@rm -f $(RM_STR)
