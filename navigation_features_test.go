package main

import (
	"context"
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"sync"
	"testing"

	neva "github.com/nevalang/neva/pkg"
	"github.com/nevalang/neva/pkg/indexer"
	"github.com/tliron/commonlog"
	protocol "github.com/tliron/glsp/protocol_3_16"

	"github.com/nevalang/neva/pkg/core"
)

func TestInitializeAdvertisesCodeLensResolveProvider(t *testing.T) {
	t.Parallel()

	idx, err := indexer.NewDefault(commonlog.GetLoggerf("neva-lsp.init_capabilities_test"))
	if err != nil {
		t.Fatalf("create indexer: %v", err)
	}

	h := BuildHandler(commonlog.GetLoggerf("neva-lsp.init_capabilities_test"), "neva", idx)
	workspace := t.TempDir()
	result, err := h.Initialize(nil, &protocol.InitializeParams{RootPath: &workspace})
	if err != nil {
		t.Fatalf("Initialize() error = %v", err)
	}

	initResult, ok := result.(protocol.InitializeResult)
	if !ok {
		t.Fatalf("Initialize() type=%T, want protocol.InitializeResult", result)
	}
	if initResult.Capabilities.CodeLensProvider == nil {
		t.Fatal("Initialize() codeLensProvider is nil")
	}
	if initResult.Capabilities.CodeLensProvider.ResolveProvider == nil {
		t.Fatal("Initialize() codeLensProvider.resolveProvider is nil")
	}
	if !*initResult.Capabilities.CodeLensProvider.ResolveProvider {
		t.Fatal("Initialize() codeLensProvider.resolveProvider = false, want true")
	}
}

func TestImportedPackageAndEntityNavigationAndHover(t *testing.T) {
	t.Parallel()

	mainFile := strings.TrimSpace(`
import {
	fmt
	runtime
	streams
}

def Main(start any) (stop any) {
	for_each streams.ForEach<int>{Print2Lines}
	range streams.Range
	wait streams.Wait
	panic runtime.Panic
	---
	:start -> [
		99 -> range:from,
		-1 -> range:to
	]
	range -> for_each
	for_each:res -> wait -> :stop
	for_each:err -> panic
}

def Print2Lines(data int) (res any, err error) {
	print_first_line Tap<int>{PrintFirstLine}?
	dec Dec
	print_second_line PrintSecondLine?
	---
	:data -> print_first_line -> dec -> print_second_line -> :res
}

def PrintFirstLine(data int) (res any, err error) {
	p1 fmt.Println?
	p2 fmt.Println?
	p3 fmt.Printf?
	switch Switch<int>
	---
	:data -> [
		switch:data,
		0 -> switch:case[0] -> 'No more bottles of beer on the wall, no more bottles of beer.' -> p1,
		1 -> switch:case[1] -> '1 bottle of beer on the wall, 1 bottle of beer.' -> p2
	]
	switch:else -> [
		p3:args[0],
		'$0 bottles of beer on the wall, $0 bottles of beer.\n' -> p3:tpl
	]
	[p1, p2, p3] -> :res
}

def PrintSecondLine(data int) (res any, err error) {
	p1 fmt.Println?
	p2 fmt.Println?
	p3 fmt.Println?
	p4 fmt.Printf?
	switch Switch<int>
	---
	:data -> [
		switch:data,
		-1 -> switch:case[0] -> 'Go to the store and buy some more, 99 bottles of beer on the wall.' -> p1,
		0 -> switch:case[1] -> 'Take one down and pass it around, no more bottles of beer on the wall.\n' -> p2,
		1 -> switch:case[2] -> 'Take one down and pass it around, 1 bottle of beer on the wall.\n' -> p3
	]
	switch:else -> [
		p4:args[0],
		'Take one down and pass it around, $0 bottles of beer on the wall.\n\n' -> p4:tpl
	]
	[p1, p2, p3, p4] -> :res
}
`) + "\n"

	server, docURI, content := buildIndexedServerWithSingleMainFile(t, mainFile)

	forEachPos := positionForNth(t, content, "ForEach", 0, 3)
	defResult, err := server.TextDocumentDefinition(nil, &protocol.DefinitionParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: docURI},
			Position:     forEachPos,
		},
	})
	if err != nil {
		t.Fatalf("TextDocumentDefinition(ForEach) error = %v", err)
	}
	defLocations := definitionLocationsFromResult(t, defResult)
	if len(defLocations) != 1 {
		t.Fatalf("TextDocumentDefinition(ForEach) count=%d, want 1", len(defLocations))
	}
	if !uriHasPathSuffix(defLocations[0].URI, "/neva/std/streams/for_each.neva") {
		t.Fatalf("TextDocumentDefinition(ForEach) uri=%q, want suffix %q", defLocations[0].URI, "/neva/std/streams/for_each.neva")
	}

	importStreamsPos := positionForNth(t, content, "streams", 0, 2)
	importDefResult, err := server.TextDocumentDefinition(nil, &protocol.DefinitionParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: docURI},
			Position:     importStreamsPos,
		},
	})
	if err != nil {
		t.Fatalf("TextDocumentDefinition(import streams) error = %v", err)
	}
	importDefLocations := definitionLocationsFromResult(t, importDefResult)
	if len(importDefLocations) != 1 {
		t.Fatalf("TextDocumentDefinition(import streams) count=%d, want 1", len(importDefLocations))
	}
	if !uriHasPathSuffix(importDefLocations[0].URI, "/neva/std/streams/streams.neva") {
		t.Fatalf("TextDocumentDefinition(import streams) uri=%q, want suffix %q", importDefLocations[0].URI, "/neva/std/streams/streams.neva")
	}

	usageStreamsPos := positionForNth(t, content, "streams.ForEach", 0, 2)
	usagePkgHover, err := server.TextDocumentHover(nil, &protocol.HoverParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: docURI},
			Position:     usageStreamsPos,
		},
	})
	if err != nil {
		t.Fatalf("TextDocumentHover(streams pkg usage) error = %v", err)
	}
	usagePkgHoverValue := hoverMarkupValue(t, usagePkgHover)
	if !strings.Contains(usagePkgHoverValue, "import streams std:streams") {
		t.Fatalf("TextDocumentHover(streams pkg usage) value=%q, expected package import snippet", usagePkgHoverValue)
	}
	if !strings.Contains(usagePkgHoverValue, "[ForEach](") {
		t.Fatalf("TextDocumentHover(streams pkg usage) value=%q, expected clickable package entity links", usagePkgHoverValue)
	}

	entityHover, err := server.TextDocumentHover(nil, &protocol.HoverParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: docURI},
			Position:     forEachPos,
		},
	})
	if err != nil {
		t.Fatalf("TextDocumentHover(ForEach) error = %v", err)
	}
	entityHoverValue := hoverMarkupValue(t, entityHover)
	if !strings.Contains(entityHoverValue, "def ForEach") {
		t.Fatalf("TextDocumentHover(ForEach) value=%q, expected ForEach signature", entityHoverValue)
	}

	intPos := positionForNth(t, content, "ForEach<int>", 0, len("ForEach<"))
	builtinDefResult, err := server.TextDocumentDefinition(nil, &protocol.DefinitionParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: docURI},
			Position:     intPos,
		},
	})
	if err != nil {
		t.Fatalf("TextDocumentDefinition(int) error = %v", err)
	}
	builtinDefLocations := definitionLocationsFromResult(t, builtinDefResult)
	if len(builtinDefLocations) != 1 {
		t.Fatalf("TextDocumentDefinition(int) count=%d, want 1", len(builtinDefLocations))
	}
	if !uriHasPathSuffix(builtinDefLocations[0].URI, "/neva/std/builtin/types.neva") {
		t.Fatalf("TextDocumentDefinition(int) uri=%q, want suffix %q", builtinDefLocations[0].URI, "/neva/std/builtin/types.neva")
	}
	if builtinDefLocations[0].Range.Start.Line == 0 {
		t.Fatalf("TextDocumentDefinition(int) start line=%d, want non-zero line for builtin declaration", builtinDefLocations[0].Range.Start.Line)
	}

	builtinHover, err := server.TextDocumentHover(nil, &protocol.HoverParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: docURI},
			Position:     intPos,
		},
	})
	if err != nil {
		t.Fatalf("TextDocumentHover(int) error = %v", err)
	}
	builtinHoverValue := hoverMarkupValue(t, builtinHover)
	if !strings.Contains(builtinHoverValue, "type int") {
		t.Fatalf("TextDocumentHover(int) value=%q, expected builtin type signature", builtinHoverValue)
	}
}

func TestHoverIncludesLeadingCommentsAcrossDirectives(t *testing.T) {
	t.Parallel()

	mainFile := strings.TrimSpace(`
// sends input through unchanged
// used to validate hover docs without directives
def LocalComponent(data any) (res any) {
	:data -> :res
}

// docs should still appear in hover
// even with directives between docs and declaration
#extern(external_component)
def ExternalComponent(data any) (res any)

def Main(start any) (stop any) {
	local LocalComponent
	external ExternalComponent
	---
	:start -> local -> external -> :stop
}
`) + "\n"

	server, docURI, content := buildIndexedServerWithSingleMainFile(t, mainFile)

	localPos := positionForNth(t, content, "LocalComponent", 1, 2)
	localHover, err := server.TextDocumentHover(nil, &protocol.HoverParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: docURI},
			Position:     localPos,
		},
	})
	if err != nil {
		t.Fatalf("TextDocumentHover(LocalComponent) error = %v", err)
	}
	localHoverValue := hoverMarkupValue(t, localHover)
	if !strings.Contains(localHoverValue, "sends input through unchanged\nused to validate hover docs without directives") {
		t.Fatalf("TextDocumentHover(LocalComponent) value=%q, expected leading comment block", localHoverValue)
	}
	if !strings.Contains(localHoverValue, "def LocalComponent") {
		t.Fatalf("TextDocumentHover(LocalComponent) value=%q, expected component signature", localHoverValue)
	}

	externalPos := positionForNth(t, content, "ExternalComponent", 1, 2)
	externalHover, err := server.TextDocumentHover(nil, &protocol.HoverParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: docURI},
			Position:     externalPos,
		},
	})
	if err != nil {
		t.Fatalf("TextDocumentHover(ExternalComponent) error = %v", err)
	}
	externalHoverValue := hoverMarkupValue(t, externalHover)
	if !strings.Contains(
		externalHoverValue,
		"docs should still appear in hover\neven with directives between docs and declaration",
	) {
		t.Fatalf("TextDocumentHover(ExternalComponent) value=%q, expected directive-separated comment block", externalHoverValue)
	}
	if !strings.Contains(externalHoverValue, "def ExternalComponent") {
		t.Fatalf("TextDocumentHover(ExternalComponent) value=%q, expected component signature", externalHoverValue)
	}
}

func TestNodeAndPortNavigationAndHover(t *testing.T) {
	t.Parallel()

	mainFile := strings.TrimSpace(`
def Main(start any) (stop any) {
    echo Echo
    ---
    :start -> echo:data
    echo:res -> :stop
}

def Echo(data any) (res any) {
    :data -> :res
}
`) + "\n"

	server, docURI, content := buildIndexedServerWithSingleMainFile(t, mainFile)

	nodeDefPos := positionForNth(t, content, "echo Echo", 0, 1)
	nodeRefPos := positionForNth(t, content, "echo:res", 0, 1)

	nodeDefResult, err := server.TextDocumentDefinition(nil, &protocol.DefinitionParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: docURI},
			Position:     nodeDefPos,
		},
	})
	if err != nil {
		t.Fatalf("TextDocumentDefinition(node def) error = %v", err)
	}
	nodeRefResult, err := server.TextDocumentDefinition(nil, &protocol.DefinitionParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: docURI},
			Position:     nodeRefPos,
		},
	})
	if err != nil {
		t.Fatalf("TextDocumentDefinition(node ref) error = %v", err)
	}
	nodeDefLocations := definitionLocationsFromResult(t, nodeDefResult)
	nodeRefLocations := definitionLocationsFromResult(t, nodeRefResult)
	if len(nodeDefLocations) != 1 || len(nodeRefLocations) != 1 {
		t.Fatalf(
			"unexpected node definition location counts def=%d ref=%d",
			len(nodeDefLocations),
			len(nodeRefLocations),
		)
	}
	if nodeDefLocations[0] != nodeRefLocations[0] {
		t.Fatalf("node ref location=%+v, want node def location=%+v", nodeRefLocations[0], nodeDefLocations[0])
	}

	nodeHover, err := server.TextDocumentHover(nil, &protocol.HoverParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: docURI},
			Position:     nodeRefPos,
		},
	})
	if err != nil {
		t.Fatalf("TextDocumentHover(node ref) error = %v", err)
	}
	nodeHoverValue := hoverMarkupValue(t, nodeHover)
	if !strings.Contains(nodeHoverValue, "Node `echo`") {
		t.Fatalf("TextDocumentHover(node ref) value=%q, expected node label", nodeHoverValue)
	}
	if !strings.Contains(nodeHoverValue, "def Echo") {
		t.Fatalf("TextDocumentHover(node ref) value=%q, expected target component signature", nodeHoverValue)
	}

	portRefPos := positionForNth(t, content, "echo:res", 0, len("echo:")+1)
	build, ok := server.getBuild()
	if !ok {
		t.Fatal("missing build for port debug")
	}
	ctx, err := server.findFile(build, docURI)
	if err != nil {
		t.Fatalf("findFile() port debug error = %v", err)
	}
	if _, found := server.findPortHitAtPosition(build, ctx, lspToCorePosition(portRefPos)); !found {
		t.Fatal("expected port hit at echo:res")
	}
	portDefResult, err := server.TextDocumentDefinition(nil, &protocol.DefinitionParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: docURI},
			Position:     portRefPos,
		},
	})
	if err != nil {
		t.Fatalf("TextDocumentDefinition(port ref) error = %v", err)
	}
	portDefLocations := definitionLocationsFromResult(t, portDefResult)
	if len(portDefLocations) != 1 {
		t.Fatalf("TextDocumentDefinition(port ref) count=%d, want 1", len(portDefLocations))
	}
	if !uriHasPathSuffix(portDefLocations[0].URI, "/main.neva") {
		t.Fatalf("TextDocumentDefinition(port ref) uri=%q, want current file", portDefLocations[0].URI)
	}

	portHover, err := server.TextDocumentHover(nil, &protocol.HoverParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: docURI},
			Position:     portRefPos,
		},
	})
	if err != nil {
		t.Fatalf("TextDocumentHover(port ref) error = %v", err)
	}
	portHoverValue := hoverMarkupValue(t, portHover)
	if !strings.Contains(portHoverValue, "echo:res any") {
		t.Fatalf("TextDocumentHover(port ref) value=%q, expected typed port snippet", portHoverValue)
	}
	if !strings.Contains(portHoverValue, "out port") {
		t.Fatalf("TextDocumentHover(port ref) value=%q, expected direction", portHoverValue)
	}

	componentPortPos := positionForNth(t, content, ":start", 0, 1)
	componentPortDefResult, err := server.TextDocumentDefinition(nil, &protocol.DefinitionParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: docURI},
			Position:     componentPortPos,
		},
	})
	if err != nil {
		t.Fatalf("TextDocumentDefinition(component port ref) error = %v", err)
	}
	componentPortDefLocations := definitionLocationsFromResult(t, componentPortDefResult)
	if len(componentPortDefLocations) != 1 {
		t.Fatalf("TextDocumentDefinition(component port ref) count=%d, want 1", len(componentPortDefLocations))
	}
	if !uriHasPathSuffix(componentPortDefLocations[0].URI, "/main.neva") {
		t.Fatalf("TextDocumentDefinition(component port ref) uri=%q, want current file", componentPortDefLocations[0].URI)
	}
}

func TestImportKeywordDoesNotResolveToImplicitAnyReference(t *testing.T) {
	t.Parallel()

	mainFile := strings.TrimSpace(`
import {
	fmt
}

pub type Box<T> struct {
	value T
}
`) + "\n"

	server, docURI, content := buildIndexedServerWithSingleMainFile(t, mainFile)

	importKeywordPos := positionForNth(t, content, "import", 0, 0)
	hover, err := server.TextDocumentHover(nil, &protocol.HoverParams{
		TextDocumentPositionParams: protocol.TextDocumentPositionParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: docURI},
			Position:     importKeywordPos,
		},
	})
	if err != nil {
		t.Fatalf("TextDocumentHover(import keyword) error = %v", err)
	}
	if hover != nil {
		t.Fatalf("TextDocumentHover(import keyword) = %+v, want nil", hover)
	}
}

func TestStdFilterReferencesCodeLensResolvesToShowReferencesCommand(t *testing.T) {
	t.Parallel()

	mainFile := strings.TrimSpace(`
import {
	fmt
	runtime
	streams
}

const numbers list<int> = [2, 4, 6, 8, 10]

def Main(start any) (stop any) {
	list_to_stream streams.FromList<int>
	filter_even streams.Filter<int>{predicate IsEven}
	for_print streams.ForEach<int>{fmt.Println<int>}
	wait streams.Wait
	panic runtime.Panic
	---
	:start -> $numbers -> list_to_stream -> filter_even -> for_print
	for_print:res -> wait -> :stop
	for_print:err -> panic
}

def IsEven(data int) (res bool) {
	mod Mod
	eq Eq<int>
	---
	:data -> [mod:left, 2 -> mod:right]
	mod -> [eq:left, 0 -> eq:right]
	eq -> :res
}
`) + "\n"

	server, _, _ := buildIndexedServerWithSingleMainFile(t, mainFile)

	filterURI := pathToURI(server.pathForLocation(core.Location{
		ModRef: core.ModuleRef{
			Path:    "std",
			Version: neva.Version,
		},
		Package:  "streams",
		Filename: "filter",
	}))
	if filterURI == "file://" || filterURI == "file:" || filterURI == "" {
		t.Fatalf("unexpected std filter URI: %q", filterURI)
	}

	lenses, err := server.TextDocumentCodeLens(nil, &protocol.CodeLensParams{
		TextDocument: protocol.TextDocumentIdentifier{URI: filterURI},
	})
	if err != nil {
		t.Fatalf("TextDocumentCodeLens(std/streams/filter) error = %v", err)
	}
	if len(lenses) == 0 {
		t.Fatal("TextDocumentCodeLens(std/streams/filter) returned no lenses")
	}

	var filterRefsLens *protocol.CodeLens
	var predicateImplLens *protocol.CodeLens
	for i := range lenses {
		parsed, ok := parseCodeLensData(lenses[i].Data)
		if !ok {
			continue
		}
		if parsed.Name == "Filter" && parsed.Kind == codeLensKindReferences {
			filterRefsLens = &lenses[i]
		}
		if parsed.Name == "IPredicate" && parsed.Kind == codeLensKindImplementations {
			predicateImplLens = &lenses[i]
		}
	}
	if filterRefsLens == nil {
		t.Fatal("missing references code lens for std streams.Filter")
	}
	if predicateImplLens == nil {
		t.Fatal("missing implementations code lens for std streams.IPredicate")
	}

	resolvedLens, err := server.CodeLensResolve(nil, filterRefsLens)
	if err != nil {
		t.Fatalf("CodeLensResolve(Filter references) error = %v", err)
	}
	if resolvedLens.Command == nil {
		t.Fatal("CodeLensResolve(Filter references) command is nil")
	}
	if resolvedLens.Command.Command != showReferencesClientCommand {
		t.Fatalf(
			"CodeLensResolve(Filter references) command=%q, want %q",
			resolvedLens.Command.Command,
			showReferencesClientCommand,
		)
	}
	if len(resolvedLens.Command.Arguments) != 3 {
		t.Fatalf("CodeLensResolve(Filter references) args len=%d, want 3", len(resolvedLens.Command.Arguments))
	}

	locationsJSON, err := json.Marshal(resolvedLens.Command.Arguments[2])
	if err != nil {
		t.Fatalf("marshal locations argument: %v", err)
	}
	var locations []protocol.Location
	if err := json.Unmarshal(locationsJSON, &locations); err != nil {
		t.Fatalf("unmarshal locations argument: %v", err)
	}
	if len(locations) == 0 {
		t.Fatal("CodeLensResolve(Filter references) returned empty locations")
	}

	resolvedImplLens, err := server.CodeLensResolve(nil, predicateImplLens)
	if err != nil {
		t.Fatalf("CodeLensResolve(IPredicate implementations) error = %v", err)
	}
	if resolvedImplLens.Command == nil {
		t.Fatal("CodeLensResolve(IPredicate implementations) command is nil")
	}
	if !strings.Contains(resolvedImplLens.Command.Title, "implementations") {
		t.Fatalf(
			"CodeLensResolve(IPredicate implementations) title=%q, expected implementations count",
			resolvedImplLens.Command.Title,
		)
	}
}

func buildIndexedServerWithSingleMainFile(t *testing.T, mainFile string) (*Server, string, string) {
	t.Helper()

	workspace := t.TempDir()
	writeNavigationTestFile(t, filepath.Join(workspace, "neva.yml"), fmt.Sprintf("neva: %s\n", neva.Version))
	writeNavigationTestFile(t, filepath.Join(workspace, "main.neva"), mainFile)

	idx, err := indexer.NewDefault(commonlog.GetLoggerf("neva-lsp.navigation_test"))
	if err != nil {
		t.Fatalf("create indexer: %v", err)
	}

	server := &Server{
		workspacePath:   workspace,
		logger:          commonlog.GetLoggerf("neva-lsp.navigation_server_test"),
		indexer:         idx,
		indexMutex:      &sync.Mutex{},
		problemsMutex:   &sync.Mutex{},
		problemFiles:    make(map[string]struct{}),
		activeFileMutex: &sync.Mutex{},
	}

	build, found, compilerErr := idx.FullScan(context.Background(), workspace)
	if !found {
		t.Fatalf("FullScan() found=false for workspace %q", workspace)
	}
	if compilerErr != nil {
		t.Fatalf(
			"FullScan() compiler error: message=%q meta=%#v cause=%v",
			compilerErr.Message,
			compilerErr.Meta,
			compilerErr.Unwrap(),
		)
	}
	server.setBuild(build)

	if _, ok := server.getBuild(); !ok {
		t.Fatalf(
			"expected indexed build snapshot: found=%v err=%v modules=%d",
			found,
			compilerErr,
			len(build.Modules),
		)
	}

	return server, pathToURI(filepath.Join(workspace, "main.neva")), mainFile
}

func writeNavigationTestFile(t *testing.T, path string, content string) {
	t.Helper()

	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		t.Fatalf("mkdir %q: %v", filepath.Dir(path), err)
	}
	if err := os.WriteFile(path, []byte(content), 0o600); err != nil {
		t.Fatalf("write %q: %v", path, err)
	}
}

func positionForNth(t *testing.T, content, needle string, occurrence int, offset int) protocol.Position {
	t.Helper()

	idx := nthIndex(content, needle, occurrence)
	if idx < 0 {
		t.Fatalf("needle %q occurrence %d not found", needle, occurrence)
	}
	cursorIdx := idx + offset
	if cursorIdx < 0 || cursorIdx >= len(content) {
		t.Fatalf("cursor index %d out of range for needle %q", cursorIdx, needle)
	}

	prefix := content[:cursorIdx]
	line := strings.Count(prefix, "\n")
	lastNewline := strings.LastIndex(prefix, "\n")
	column := cursorIdx
	if lastNewline >= 0 {
		column = cursorIdx - lastNewline - 1
	}

	return protocol.Position{
		Line:      uint32(line),
		Character: uint32(column),
	}
}

func nthIndex(text, needle string, occurrence int) int {
	if occurrence < 0 {
		return -1
	}
	index := -1
	searchStart := 0
	for i := 0; i <= occurrence; i++ {
		relative := strings.Index(text[searchStart:], needle)
		if relative < 0 {
			return -1
		}
		index = searchStart + relative
		searchStart = index + len(needle)
	}
	return index
}

func definitionLocationsFromResult(t *testing.T, result any) []protocol.Location {
	t.Helper()

	switch typed := result.(type) {
	case []protocol.Location:
		return typed
	case protocol.Location:
		return []protocol.Location{typed}
	default:
		t.Fatalf("unexpected definition result type: %T", result)
		return nil
	}
}

func hoverMarkupValue(t *testing.T, hover *protocol.Hover) string {
	t.Helper()

	if hover == nil {
		t.Fatal("hover is nil")
	}

	markup, ok := hover.Contents.(protocol.MarkupContent)
	if !ok {
		t.Fatalf("hover contents type=%T, want protocol.MarkupContent", hover.Contents)
	}

	return markup.Value
}

func uriHasPathSuffix(uri, suffix string) bool {
	pathValue, err := uriToPath(uri)
	if err != nil {
		return false
	}
	return strings.HasSuffix(pathValue, suffix)
}
