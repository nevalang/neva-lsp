package main

import (
	"errors"
	"fmt"

	src "github.com/nevalang/neva/pkg/ast"
	"github.com/nevalang/neva/pkg/core"
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
	if params.FileID == "" {
		return nil, errors.New("fileId is required")
	}
	if params.EntityRef.Name == "" {
		return nil, errors.New("entityRef.name is required")
	}

	build, ok := s.getBuild()
	if !ok {
		return nil, errors.New("program index is not ready")
	}

	loc, found := fileLocationByID(build, params.FileID)
	if !found {
		return nil, fmt.Errorf("file not found: %s", params.FileID)
	}

	scope := src.NewScope(*build, loc)
	entity, targetLoc, err := scope.Entity(params.EntityRef)
	if err != nil {
		return nil, fmt.Errorf("resolve entity ref: %w", err)
	}

	anchor := view.SourceAnchor{
		ModulePath:    targetLoc.ModRef.Path,
		ModuleVersion: targetLoc.ModRef.Version,
		Package:       targetLoc.Package,
		File:          targetLoc.Filename,
	}
	if meta := entity.Meta(); meta != nil {
		anchor = viewAnchorFromMeta(*meta)
	}

	return ResolveEntityRefResult{
		TargetKind:     entity.Kind,
		TargetName:     params.EntityRef.Name,
		TargetFileID:   view.ResolveFileViewID(targetLoc),
		TargetEntityID: view.ResolveEntityViewID(targetLoc, params.EntityRef.Name, entity.Kind, params.OverloadIndex),
		TargetAnchor:   anchor,
	}, nil
}

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

func fileLocationByID(build *src.Build, wantedID string) (core.Location, bool) {
	for modRef, mod := range build.Modules {
		for packageName, pkg := range mod.Packages {
			for fileName := range pkg {
				loc := core.Location{ModRef: modRef, Package: packageName, Filename: fileName}
				if view.ResolveFileViewID(loc) == wantedID {
					return loc, true
				}
			}
		}
	}
	return core.Location{}, false
}

func viewAnchorFromMeta(meta core.Meta) view.SourceAnchor {
	return view.SourceAnchor{
		ModulePath:    meta.Location.ModRef.Path,
		ModuleVersion: meta.Location.ModRef.Version,
		Package:       meta.Location.Package,
		File:          meta.Location.Filename,
		Text:          meta.Text,
		StartLine:     meta.Start.Line,
		StartCol:      meta.Start.Column,
		EndLine:       meta.Stop.Line,
		EndCol:        meta.Stop.Column,
	}
}
