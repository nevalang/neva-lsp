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
			for _, file := range pkg.Files {
				return file.ID
			}
		}
	}
	t.Fatal("no files in program")
	return ""
}
