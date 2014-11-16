
DEFAULT: all

all: pdf wordcount

pdf:
	make -C src

wordcount:
	make -C src wordcount
