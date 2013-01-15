PACKAGE=graph

$(PACKAGE).html: $(PACKAGE).lisp $(PACKAGE).asd document
	./document

index.html: $(PACKAGE).lisp $(PACKAGE).html
	(cat $(PACKAGE).html|sed -n '/<html/,/INTRODUCTION_PASTE/p'|head -n -1; \
        cat $(PACKAGE).lisp|sed -n '/Commentary/,/Code/p'|cut -c4-|head -n -2|tail -n +3|markdown; \
        cat $(PACKAGE).html|sed -n '/INTRODUCTION_PASTE/,$$p'|tail -n +2) > index.html; \
	rm $(PACKAGE).html
