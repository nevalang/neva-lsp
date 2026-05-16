package main

import (
	"errors"
	"fmt"

	src "github.com/nevalang/neva/pkg/ast"
	"github.com/nevalang/neva/pkg/view"
	"github.com/tliron/glsp"
)

func (s *Server) GetProgramView(_ *glsp.Context, _ GetProgramViewRequest) (any, error) {
	build, ok := s.getBuild()
	if !ok {
		return nil, errors.New("program index is not ready")
	}

	return view.ProjectProgram(*build), nil
}

func (s *Server) GetFileView(_ *glsp.Context, params GetFileViewRequest) (any, error) {
	if params.FileID == "" {
		return nil, errors.New("fileId is required")
	}

	build, ok := s.getBuild()
	if !ok {
		return nil, errors.New("program index is not ready")
	}

	fileView, found := view.ProjectFileByID(*build, params.FileID)
	if !found {
		return nil, fmt.Errorf("file not found: %s", params.FileID)
	}

	return fileView, nil
}

func (s *Server) ResolveEntityRef(_ *glsp.Context, params ResolveEntityRefRequest) (any, error) {
	if params.TargetFileID == "" {
		return nil, errors.New("targetFileId is required")
	}
	if params.TargetEntityID == "" {
		return nil, errors.New("targetEntityId is required")
	}

	build, ok := s.getBuild()
	if !ok {
		return nil, errors.New("program index is not ready")
	}

	// ResolveEntityRef works on canonical IDs produced by pkg/view projection.
	// It is a stable lookup by normalized address, not a fresh scope-based AST resolve.
	fileView, found := view.ProjectFileByID(*build, params.TargetFileID)
	if !found {
		return nil, fmt.Errorf("file not found: %s", params.TargetFileID)
	}

	result, found := findEntityInFile(fileView, params.TargetEntityID)
	if !found {
		return nil, fmt.Errorf("entity not found: %s", params.TargetEntityID)
	}
	return result, nil
}

// ResolveFileLegacy is retained only as migration reference.
// It is intentionally NOT registered in LSP method dispatch anymore.
//
// Deprecated: use neva/view/getFileView.
func (s *Server) ResolveFileLegacy(_ *glsp.Context, params LegacyGetFileViewRequest) (any, error) {
	s.logger.Info("resolve_file is deprecated; use neva/view/getFileView")

	build, ok := s.getBuild()
	if !ok {
		return nil, errors.New("program index is not ready")
	}

	uriPath := params.Document.URI.Path
	if uriPath == "" {
		uriPath = params.Document.URI.FSPath
	}
	ctx, err := s.findFile(build, uriPath)
	if err != nil {
		return nil, err
	}

	return LegacyGetFileViewResponse{
		File:  ctx.file,
		Extra: Extra{NodesPorts: map[string]map[string]src.Interface{}},
	}, nil
}

func findEntityInFile(file view.File, targetEntityID string) (ResolveEntityRefResult, bool) {
	for _, component := range file.Components {
		if component.ID == targetEntityID {
			return ResolveEntityRefResult{
				TargetKind:     "component_entity",
				TargetName:     component.Name,
				TargetFileID:   file.ID,
				TargetEntityID: component.ID,
				TargetAnchor:   component.Anchor,
			}, true
		}
	}

	for _, iface := range file.Interfaces {
		if iface.ID == targetEntityID {
			return ResolveEntityRefResult{
				TargetKind:     "interface_entity",
				TargetName:     iface.Name,
				TargetFileID:   file.ID,
				TargetEntityID: iface.ID,
				TargetAnchor:   iface.Anchor,
			}, true
		}
	}

	for _, typ := range file.Types {
		if typ.ID == targetEntityID {
			return ResolveEntityRefResult{
				TargetKind:     "type_entity",
				TargetName:     typ.Name,
				TargetFileID:   file.ID,
				TargetEntityID: typ.ID,
				TargetAnchor:   typ.Anchor,
			}, true
		}
	}

	for _, cnst := range file.Consts {
		if cnst.ID == targetEntityID {
			return ResolveEntityRefResult{
				TargetKind:     "const_entity",
				TargetName:     cnst.Name,
				TargetFileID:   file.ID,
				TargetEntityID: cnst.ID,
				TargetAnchor:   cnst.Anchor,
			}, true
		}
	}

	return ResolveEntityRefResult{}, false
}
