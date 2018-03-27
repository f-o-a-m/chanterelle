.PHONY: test build deploy install compile-contracts

NODE_URL ?= "http://localhost:8545"

all: install
	@echo prereqs that are newer than install: $?

install:
	npm install

compile-contracts:
	./node_modules/.bin/truffle compile
	npm run generator

deploy-blank:
	pulp run --src-path deploy --main Blank --include src

deploy-foam:
	pulp run --src-path deploy --main Foam --include src

migrate-foam:
	pulp run --src-path migrations --main ParkingAuthorityMigration --include src

test:
	npm run test
