.PHONY: install compile-contracts build deploy test

NODE_URL ?= "http://localhost:8545"

all: install

install:
	npm install

compile-contracts:
	truffle compile/
	npm run generator;

build:
	pulp build

deploy:
	pulp run

test:
	npm run test
