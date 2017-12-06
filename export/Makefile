MODNAME = gen

.PHONY: all link byte run clean

all: compile

compile: byte link

link: byte
	tjlink $(MODNAME)

byte:
	tjcc proofsystem && tjcc termrep && tjcc  $(MODNAME)

run: all
	tjsim $(MODNAME)

clean:
	rm *.lpo *lp
