SRC = thesis

all: clean buildanimation pdf

SRCS = thesis.tex

pdf:
	pdflatex  -shell-escape $(SRC)
	bibtex $(SRC)
	pdflatex  -shell-escape $(SRC)
	pdflatex  -shell-escape thesis.tex

clean:
	rm -f *.aux *.log thesis.pdf *.out *.gz

buildanimation: clean
	pdflatex -shell-escape thesis.tex
	pdflatex -shell-escape thesis.tex
