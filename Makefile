
DEFAULT: all

all: pdf wordcount

pdf:
	make -C src

pvc:
	make -C src pvc

wordcount:
	make -C src wordcount
