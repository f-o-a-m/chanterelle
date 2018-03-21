.PHONY: test

NODE_URL ?= "http://localhost:8545"

all: install
	@echo prereqs that are newer than install: $?

install: package.json
	npm install

compile-contracts: build/
	truffle compile/
	npm run generator;

build: src/
	pulp build

deploy: output/
	pulp run

test:
	npm run test
