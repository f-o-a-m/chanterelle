.PHONY: test build deploy install compile-contracts

NODE_URL ?= "http://localhost:8545"

all: install
	@echo prereqs that are newer than install: $?

install:
	npm install

compile-contracts:
	make build
	pulp build --src-path src -m Compile --to compile.js && node compile.js --abis build/contracts --dest src --truffle true; rm compile.js

build:
	pulp build

deploy:
	pulp build --src-path src -m Main --to deploy.js && node deploy.js; rm deploy.js

test:
	npm run test
