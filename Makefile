PACKAGES=graph graph-dot graph-json

all: index.html $(PACKAGES:=.html)

%.pre: %.lisp %.asd document
	./document $*

%.html: %.lisp %.pre
	(cat $*.pre|sed -n '/<html/,/INTRODUCTION_PASTE/p'|head -n -1; \
        cat $*.lisp|sed -n '/Commentary/,/Code/p'|cut -c4-|head -n -2|tail -n +3|markdown; \
        cat $*.pre|sed -n '/INTRODUCTION_PASTE/,$$p'|tail -n +2) > $@

index.html: graph.html
	cp $< $@

clean:
	rm -f index.html $(PACKAGES:=.html)
