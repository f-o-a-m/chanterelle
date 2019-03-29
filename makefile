.PHONY: clear install build

# see https://stackoverflow.com/a/26936855/1798418
PATH  := node_modules/.bin:$(PATH)
SHELL := /bin/bash

clear:
	rm -rf ./node_modules ./bower_components ./output ./.pulp-cache ./chanterelle.js

install:
	npm i & bower i & await

build:
	pulp build -m ChanterelleMain --to chanterelle.js