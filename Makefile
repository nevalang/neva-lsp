.PHONY: build test web

PORT ?= 7792
WORKDIR ?= ../neva/e2e/echo_verbose
BIN ?= neva-lsp

build:
	go build -o $(BIN) .

test:
	go test ./...

web: build
	cd $(WORKDIR) && ../neva-lsp/$(BIN) -view -view-port $(PORT)
