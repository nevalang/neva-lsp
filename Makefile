.PHONY: install test
# .PHONY: install test web

install:
	go install .

test:
	go test ./...

# web: install
# 	neva-lsp --view --view-port=7792
