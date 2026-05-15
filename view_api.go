package main

import (
	src "github.com/nevalang/neva/pkg/ast"
	"github.com/nevalang/neva/pkg/core"
	"github.com/nevalang/neva/pkg/view"
)

const (
	methodGetProgramView    = "neva/view/getProgram"
	methodGetFileView       = "neva/view/getFileView"
	methodResolveEntityRef  = "neva/view/resolveEntityRef"
	methodResolveFileLegacy = "resolve_file"
)

// GetProgramViewRequest is currently empty and reserved for future filters.
type GetProgramViewRequest struct{}

// GetFileViewRequest requests one file view by stable file ID.
type GetFileViewRequest struct {
	FileID string `json:"fileId"`
}

// ResolveEntityRefRequest resolves an entity reference from a file context.
type ResolveEntityRefRequest struct {
	FileID        string         `json:"fileId"`
	EntityRef     core.EntityRef `json:"entityRef"`
	OverloadIndex *int           `json:"overloadIndex,omitempty"`
}

// ResolveEntityRefResult returns canonical navigation target information.
type ResolveEntityRefResult struct {
	TargetKind     src.EntityKind    `json:"targetKind"`
	TargetName     string            `json:"targetName"`
	TargetFileID   string            `json:"targetFileId"`
	TargetEntityID string            `json:"targetEntityId"`
	TargetAnchor   view.SourceAnchor `json:"targetAnchor"`
}
