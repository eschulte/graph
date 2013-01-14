
index.html: /tmp/graph.html
	(cat /tmp/graph.html|sed -n '/<html/,/INTRODUCTION_PASTE/p'|head -n -1; \
	 cat graph.lisp|sed -n '/Commentary/,/Code/p'|cut -c4-|head -n -2|tail -n +3|markdown; \
	 cat /tmp/graph.html|sed -n '/INTRODUCTION_PASTE/,$$p'|tail -n +2) > index.html
