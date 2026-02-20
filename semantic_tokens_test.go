package main

import (
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
