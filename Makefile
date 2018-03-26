.PHONY: test build deploy install compile-contracts

NODE_URL ?= "http://localhost:8545"

all: install
	@echo prereqs that are newer than install: $?

install:
	npm install

compile-contracts:
	./node_modules/.bin/truffle compile
	npm run generator
	make build

build:
	pulp build

deploy:
	pulp run

test:
	npm run test
