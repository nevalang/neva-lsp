.PHONY: install test test-web test-all web-install web-build view

install: web-build
	go install .

test:
	go test ./...

test-web: web-install
	cd web && npm test

test-all: test test-web

web-install:
	cd web && npm ci

web-build: web-install
	cd web && npm run build

view: install web-build
	neva-lsp --view --view-port=7792 --view-workspace=.
