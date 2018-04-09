.PHONY: test build-deploy build-compile deploy install compile-contracts

NODE_URL ?= "http://localhost:8545"

all: install
	@echo prereqs that are newer than install: $?

install:
	npm install

build-chanterelle:
	pulp build --src-path chanterelle

compile-contracts: build-chanterelle
	pulp build --src-path chanterelle -m Chanterelle.Internal.Compile --to compile.js && node compile.js; rm compile.js

deploy: compile-contracts build-chanterelle
	pulp build -I chanterelle --src-path src -m Main --to deploy.js && node deploy.js; rm deploy.js

test:
	pulp test -I chanterelle
