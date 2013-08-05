all: build move test

build:
	./node_modules/.bin/jison lib/grammar.y lib/lexer.l
	mv grammar.js dist/parser.js

move: lib
	cp lib/*.js dist/

test: move dist
	node dist/cli.js test/ops-sample.dab | node

standalone: move dist
	node scripts/standalone.js | uglifyjs > standalone/dabble.js

