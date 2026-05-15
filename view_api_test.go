package main

import (
	"encoding/json"
	"testing"

	"github.com/nevalang/neva/pkg/core"
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

func TestViewAPI_ResolveEntityRef_LocalImportedBuiltin(t *testing.T) {
	t.Parallel()

	mainFile := `
import {
	fmt
}

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

	programAny, err := server.GetProgramView(nil, GetProgramViewRequest{})
	if err != nil {
		t.Fatalf("GetProgramView() error = %v", err)
	}
	program := programAny.(view.Program)
	fileID := firstFileID(t, program)

	localAny, err := server.ResolveEntityRef(nil, ResolveEntityRefRequest{
		FileID:    fileID,
		EntityRef: core.EntityRef{Name: "Echo"},
	})
	if err != nil {
		t.Fatalf("ResolveEntityRef(local) error = %v", err)
	}
	local := localAny.(ResolveEntityRefResult)
	if local.TargetKind == "" || local.TargetFileID == "" || local.TargetEntityID == "" {
		t.Fatalf("ResolveEntityRef(local) incomplete result: %#v", local)
	}

	importedAny, err := server.ResolveEntityRef(nil, ResolveEntityRefRequest{
		FileID: fileID,
		EntityRef: core.EntityRef{
			Pkg:  "fmt",
			Name: "Println",
		},
	})
	if err != nil {
		t.Fatalf("ResolveEntityRef(imported) error = %v", err)
	}
	imported := importedAny.(ResolveEntityRefResult)
	if imported.TargetAnchor.Package != "fmt" {
		t.Fatalf("ResolveEntityRef(imported) package = %q, want fmt", imported.TargetAnchor.Package)
	}

	builtinAny, err := server.ResolveEntityRef(nil, ResolveEntityRefRequest{
		FileID:    fileID,
		EntityRef: core.EntityRef{Name: "int"},
	})
	if err != nil {
		t.Fatalf("ResolveEntityRef(builtin) error = %v", err)
	}
	builtin := builtinAny.(ResolveEntityRefResult)
	if builtin.TargetAnchor.Package != "builtin" {
		t.Fatalf("ResolveEntityRef(builtin) package = %q, want builtin", builtin.TargetAnchor.Package)
	}
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
