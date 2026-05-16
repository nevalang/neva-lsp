package main

import (
	"encoding/json"
	"testing"

	"github.com/nevalang/neva/pkg/view"
)

func TestViewAPI_GetProgramAndFileView(t *testing.T) {
	t.Parallel()

	mainFile := `
const Greeting string = 'Hello'

type Name string

interface Printer(data any) (res any)

def Main(start any) (stop any) {
	echo Printer
	---
	:start -> echo:data
	echo:res -> :stop
}
`

	server, _, _ := buildIndexedServerWithSingleMainFile(t, mainFile)

	programAny, err := server.GetProgramView(nil, GetProgramViewRequest{})
	if err != nil {
		t.Fatalf("GetProgramView() error = %v", err)
	}
	program, ok := programAny.(view.Program)
	if !ok {
		t.Fatalf("GetProgramView() type = %T, want view.Program", programAny)
	}
	fileID := firstFileID(t, program)

	fileAny, err := server.GetFileView(nil, GetFileViewRequest{FileID: fileID})
	if err != nil {
		t.Fatalf("GetFileView() error = %v", err)
	}
	fileView, ok := fileAny.(view.File)
	if !ok {
		t.Fatalf("GetFileView() type = %T, want view.File", fileAny)
	}
	if fileView.Name != "main" {
		t.Fatalf("GetFileView() file name = %q, want main", fileView.Name)
	}

	encodedProgram1, err := json.Marshal(program)
	if err != nil {
		t.Fatalf("marshal program #1: %v", err)
	}
	programAny2, err := server.GetProgramView(nil, GetProgramViewRequest{})
	if err != nil {
		t.Fatalf("GetProgramView() second call error = %v", err)
	}
	encodedProgram2, err := json.Marshal(programAny2)
	if err != nil {
		t.Fatalf("marshal program #2: %v", err)
	}
	if string(encodedProgram1) != string(encodedProgram2) {
		t.Fatal("GetProgramView() is not deterministic")
	}
}

func TestViewAPI_GetProgram_FilterGroups(t *testing.T) {
	t.Parallel()

	mainFile := `
def Main(start any) (stop any) {
	echo Echo
	---
	:start -> echo:data
	echo:res -> :stop
}

def Echo(data any) (res any) {
	:data -> :res
}
`

	server, _, _ := buildIndexedServerWithSingleMainFile(t, mainFile)

	allAny, err := server.GetProgramView(nil, GetProgramViewRequest{})
	if err != nil {
		t.Fatalf("GetProgramView(all) error = %v", err)
	}
	all := allAny.(view.Program)
	if len(all.Modules) == 0 {
		t.Fatal("GetProgramView(all) returned empty module list")
	}
	if countModulesByKind(all, "current") == 0 {
		t.Fatal("GetProgramView(all) missing current module")
	}
	if countModulesByKind(all, "std") == 0 {
		t.Fatal("GetProgramView(all) missing std module")
	}

	currentOnlyAny, err := server.GetProgramView(nil, GetProgramViewRequest{
		IncludeCurrent: boolRef(true),
		IncludeDeps:    boolRef(false),
		IncludeStd:     boolRef(false),
	})
	if err != nil {
		t.Fatalf("GetProgramView(current-only) error = %v", err)
	}
	currentOnly := currentOnlyAny.(view.Program)
	if len(currentOnly.Modules) != countModulesByKind(currentOnly, "current") {
		t.Fatalf("GetProgramView(current-only) returned non-current modules: %#v", currentOnly.Modules)
	}

	stdOnlyAny, err := server.GetProgramView(nil, GetProgramViewRequest{
		IncludeCurrent: boolRef(false),
		IncludeDeps:    boolRef(false),
		IncludeStd:     boolRef(true),
	})
	if err != nil {
		t.Fatalf("GetProgramView(std-only) error = %v", err)
	}
	stdOnly := stdOnlyAny.(view.Program)
	if len(stdOnly.Modules) != countModulesByKind(stdOnly, "std") {
		t.Fatalf("GetProgramView(std-only) returned non-std modules: %#v", stdOnly.Modules)
	}
}

func TestViewAPI_ResolveEntityRef_ByCanonicalAddress(t *testing.T) {
	t.Parallel()

	mainFile := `
import {
	runtime
}

const Greeting string = 'Hello'
type Name string
interface Printer(data any) (res any)

def Main(start any) (stop any) {
	panic runtime.Panic
	echo Echo
	---
	:start -> echo:data
	echo:res -> [panic, :stop]
}

def Echo(data any) (res any) {
	:data -> :res
}
`

	server, _, _ := buildIndexedServerWithSingleMainFile(t, mainFile)

	programAny, err := server.GetProgramView(nil, GetProgramViewRequest{})
	if err != nil {
		t.Fatalf("GetProgramView() error = %v", err)
	}
	program := programAny.(view.Program)
	fileID := firstFileID(t, program)
	fileAny, err := server.GetFileView(nil, GetFileViewRequest{FileID: fileID})
	if err != nil {
		t.Fatalf("GetFileView() error = %v", err)
	}
	fileView := fileAny.(view.File)

	mainComponent := findComponentByName(fileView, "Main")
	if mainComponent == nil || len(mainComponent.Nodes) == 0 || mainComponent.Nodes[0].ResolvedRef == nil {
		t.Fatalf("expected resolved node reference in file view")
	}

	resolved := mainComponent.Nodes[0].ResolvedRef
	nodeTargetAny, err := server.ResolveEntityRef(nil, ResolveEntityRefRequest{
		TargetFileID:   resolved.FileID,
		TargetEntityID: resolved.EntityID,
	})
	if err != nil {
		t.Fatalf("ResolveEntityRef(node target) error = %v", err)
	}
	nodeTarget := nodeTargetAny.(ResolveEntityRefResult)
	if nodeTarget.TargetKind == "" || nodeTarget.TargetEntityID == "" {
		t.Fatalf("ResolveEntityRef(node target) incomplete result: %#v", nodeTarget)
	}
	if nodeTarget.TargetKind != "component_entity" {
		t.Fatalf("ResolveEntityRef(node target) kind = %q, want component_entity", nodeTarget.TargetKind)
	}

	componentTargetAny, err := server.ResolveEntityRef(nil, ResolveEntityRefRequest{
		TargetFileID:   fileID,
		TargetEntityID: fileView.Components[0].ID,
	})
	if err != nil {
		t.Fatalf("ResolveEntityRef(component) error = %v", err)
	}
	if got := componentTargetAny.(ResolveEntityRefResult).TargetKind; got != "component_entity" {
		t.Fatalf("ResolveEntityRef(component) kind = %q, want component_entity", got)
	}

	constTargetAny, err := server.ResolveEntityRef(nil, ResolveEntityRefRequest{
		TargetFileID:   fileID,
		TargetEntityID: fileView.Consts[0].ID,
	})
	if err != nil {
		t.Fatalf("ResolveEntityRef(const) error = %v", err)
	}
	if got := constTargetAny.(ResolveEntityRefResult).TargetKind; got != "const_entity" {
		t.Fatalf("ResolveEntityRef(const) kind = %q, want const_entity", got)
	}

	typeTargetAny, err := server.ResolveEntityRef(nil, ResolveEntityRefRequest{
		TargetFileID:   fileID,
		TargetEntityID: fileView.Types[0].ID,
	})
	if err != nil {
		t.Fatalf("ResolveEntityRef(type) error = %v", err)
	}
	if got := typeTargetAny.(ResolveEntityRefResult).TargetKind; got != "type_entity" {
		t.Fatalf("ResolveEntityRef(type) kind = %q, want type_entity", got)
	}

	ifaceTargetAny, err := server.ResolveEntityRef(nil, ResolveEntityRefRequest{
		TargetFileID:   fileID,
		TargetEntityID: fileView.Interfaces[0].ID,
	})
	if err != nil {
		t.Fatalf("ResolveEntityRef(interface) error = %v", err)
	}
	if got := ifaceTargetAny.(ResolveEntityRefResult).TargetKind; got != "interface_entity" {
		t.Fatalf("ResolveEntityRef(interface) kind = %q, want interface_entity", got)
	}
}

func TestViewAPI_ResolveEntityRef_NotFound(t *testing.T) {
	t.Parallel()

	mainFile := `
def Main(start any) (stop any) {
	:start -> :stop
}
`
	server, _, _ := buildIndexedServerWithSingleMainFile(t, mainFile)

	programAny, err := server.GetProgramView(nil, GetProgramViewRequest{})
	if err != nil {
		t.Fatalf("GetProgramView() error = %v", err)
	}
	program := programAny.(view.Program)
	fileID := firstFileID(t, program)

	_, err = server.ResolveEntityRef(nil, ResolveEntityRefRequest{
		TargetFileID:   fileID,
		TargetEntityID: "module/@/package/main/file/main/component/DoesNotExist@0",
	})
	if err == nil {
		t.Fatal("ResolveEntityRef(not found) error=nil, want error")
	}
}

func TestViewAPI_NavigationMainToStdDependency(t *testing.T) {
	t.Parallel()

	mainFile := `
import {
	runtime
}

def Main(start any) (stop any) {
	panic runtime.Panic
	echo Echo
	---
	:start -> echo:data
	echo:res -> [panic, :stop]
}

def Echo(data any) (res any) {
	:data -> :res
}
`

	server, _, _ := buildIndexedServerWithSingleMainFile(t, mainFile)

	programAny, err := server.GetProgramView(nil, GetProgramViewRequest{})
	if err != nil {
		t.Fatalf("GetProgramView() error = %v", err)
	}
	program := programAny.(view.Program)
	mainID := mainFileID(t, program)

	mainAny, err := server.GetFileView(nil, GetFileViewRequest{FileID: mainID})
	if err != nil {
		t.Fatalf("GetFileView(main) error = %v", err)
	}
	mainView := mainAny.(view.File)
	mainComponent := findComponentByName(mainView, "Main")
	if mainComponent == nil {
		t.Fatal("Main component not found in main file view")
	}

	var panicRef *view.ResolvedRef
	for i := range mainComponent.Nodes {
		node := mainComponent.Nodes[i]
		if node.Name == "panic" {
			panicRef = node.ResolvedRef
			break
		}
	}
	if panicRef == nil {
		t.Fatal("resolved ref for panic node not found")
	}
	if panicRef.FileID == mainID {
		t.Fatalf("expected dependency target file, got same file id %q", panicRef.FileID)
	}

	targetAny, err := server.ResolveEntityRef(nil, ResolveEntityRefRequest{
		TargetFileID:   panicRef.FileID,
		TargetEntityID: panicRef.EntityID,
	})
	if err != nil {
		t.Fatalf("ResolveEntityRef(std target) error = %v", err)
	}
	target := targetAny.(ResolveEntityRefResult)
	if target.TargetKind != "component_entity" {
		t.Fatalf("ResolveEntityRef(std target) kind = %q, want component_entity", target.TargetKind)
	}
	if target.TargetFileID != panicRef.FileID || target.TargetEntityID != panicRef.EntityID {
		t.Fatalf("ResolveEntityRef(std target) mismatch: got (%q,%q), want (%q,%q)",
			target.TargetFileID,
			target.TargetEntityID,
			panicRef.FileID,
			panicRef.EntityID,
		)
	}

	targetFileAny, err := server.GetFileView(nil, GetFileViewRequest{FileID: target.TargetFileID})
	if err != nil {
		t.Fatalf("GetFileView(target) error = %v", err)
	}
	targetFile := targetFileAny.(view.File)
	if !entityIDExists(targetFile, target.TargetEntityID) {
		t.Fatalf("target entity %q not found in target file %q", target.TargetEntityID, target.TargetFileID)
	}
}

func findComponentByName(file view.File, name string) *view.Component {
	for idx := range file.Components {
		if file.Components[idx].Name == name {
			return &file.Components[idx]
		}
	}
	return nil
}

func firstFileID(t *testing.T, program view.Program) string {
	t.Helper()
	for _, module := range program.Modules {
		for _, pkg := range module.Packages {
			for _, file := range pkg.FileSummaries {
				return file.ID
			}
		}
	}
	t.Fatal("no files in program")
	return ""
}

func mainFileID(t *testing.T, program view.Program) string {
	t.Helper()
	for _, module := range program.Modules {
		for _, pkg := range module.Packages {
			for _, file := range pkg.FileSummaries {
				if file.Name == "main" {
					return file.ID
				}
			}
		}
	}
	t.Fatal("main file not found in program")
	return ""
}

func entityIDExists(file view.File, entityID string) bool {
	for _, c := range file.Components {
		if c.ID == entityID {
			return true
		}
	}
	for _, i := range file.Interfaces {
		if i.ID == entityID {
			return true
		}
	}
	for _, tp := range file.Types {
		if tp.ID == entityID {
			return true
		}
	}
	for _, cn := range file.Consts {
		if cn.ID == entityID {
			return true
		}
	}
	return false
}

func countModulesByKind(program view.Program, kind string) int {
	count := 0
	for _, module := range program.Modules {
		switch kind {
		case "current":
			if module.Path == "@" {
				count++
			}
		case "std":
			if module.Path == "std" {
				count++
			}
		case "deps":
			if module.Path != "@" && module.Path != "std" {
				count++
			}
		}
	}
	return count
}

func boolRef(value bool) *bool {
	return &value
}
