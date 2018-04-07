.PHONY: test build-deploy build-compile deploy install compile-contracts

NODE_URL ?= "http://localhost:8545"

all: install
	@echo prereqs that are newer than install: $?

install:
	npm install

build-deploy:
	pulp build -I deploy

build-compile:
	pulp build --src-path compile

compile-contracts: build-compile
	pulp build --src-path compile -m Compile --to compile.js && node compile.js --abis build/contracts --dest src --truffle true; rm compile.js

deploy: build-deploy
	pulp build -I deploy --src-path src -m Main --to deploy.js && node deploy.js; rm deploy.js

test:
	pulp test -I deploy
