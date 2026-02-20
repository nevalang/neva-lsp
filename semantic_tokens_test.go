package main

import (
	"strings"
	"testing"

	protocol "github.com/tliron/glsp/protocol_3_16"
)

func TestRangeMatchesFileText(t *testing.T) {
	t.Parallel()

	lines := []string{
		"import {",
		"def Main(start any) (stop any) {",
	}

	ok := rangeMatchesFileText(
		protocol.Range{
			Start: protocol.Position{Line: 1, Character: 4},
			End:   protocol.Position{Line: 1, Character: 8},
		},
		"Main",
		lines,
	)
	if !ok {
		t.Fatal("rangeMatchesFileText() = false, want true for exact match")
	}
}

func TestTokenFromNamedRangeDropsMismatchedText(t *testing.T) {
	t.Parallel()

	lines := []string{
		"import {",
	}

	_, ok := tokenFromNamedRange(
		protocol.Range{
			Start: protocol.Position{Line: 0, Character: 0},
			End:   protocol.Position{Line: 0, Character: 3},
		},
		1,
		"any",
		lines,
	)
	if ok {
		t.Fatal("tokenFromNamedRange() = ok for mismatched source text, want false")
	}
}

func TestSemanticTokensNoOverlapForPortAddresses(t *testing.T) {
	t.Parallel()

	mainFile := strings.TrimSpace(`
def Echo(data any) (res any) {
	:data -> :res
}

def Main(start any) (stop any) {
	echo Echo
	---
	:start -> echo:data
	echo:res -> :stop
}
`) + "\n"

	server, docURI, _ := buildIndexedServerWithSingleMainFile(t, mainFile)
	tokens, err := server.TextDocumentSemanticTokensFull(nil, &protocol.SemanticTokensParams{
		TextDocument: protocol.TextDocumentIdentifier{URI: docURI},
	})
	if err != nil {
		t.Fatalf("TextDocumentSemanticTokensFull() error = %v", err)
	}
	if tokens == nil {
		t.Fatal("TextDocumentSemanticTokensFull() returned nil")
	}
	if len(tokens.Data) == 0 {
		t.Fatal("TextDocumentSemanticTokensFull() returned empty payload")
	}

	type absoluteToken struct {
		line   int
		start  int
		length int
	}
	absolute := make([]absoluteToken, 0, len(tokens.Data)/5)
	line := 0
	start := 0
	for i := 0; i+4 < len(tokens.Data); i += 5 {
		deltaLine := int(tokens.Data[i])
		deltaStart := int(tokens.Data[i+1])
		length := int(tokens.Data[i+2])

		line += deltaLine
		if deltaLine == 0 {
			start += deltaStart
		} else {
			start = deltaStart
		}

		absolute = append(absolute, absoluteToken{
			line:   line,
			start:  start,
			length: length,
		})
	}

	for i := 1; i < len(absolute); i++ {
		prev := absolute[i-1]
		curr := absolute[i]
		if curr.line == prev.line && curr.start < prev.start+prev.length {
			t.Fatalf(
				"overlapping semantic tokens: prev=%+v curr=%+v payload=%v",
				prev,
				curr,
				tokens.Data,
			)
		}
	}
}
