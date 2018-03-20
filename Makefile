NODE_URL ?= "http://localhost:8545"

all: install

install:
	npm install

compile-contracts: install
	truffle compile/
	npm run generator;

build: compile-contracts
	pulp build

deploy: build
	pulp run

test: deploy
	npm run test
