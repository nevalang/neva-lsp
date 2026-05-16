package main

import "github.com/nevalang/neva/pkg/view"

const (
	methodGetProgramView   = "neva/view/getProgram"
	methodGetFileView      = "neva/view/getFileView"
	methodResolveEntityRef = "neva/view/resolveEntityRef"
)

// GetProgramViewRequest is currently empty and reserved for future filters.
type GetProgramViewRequest struct{}

// GetFileViewRequest requests one file view by stable file ID.
type GetFileViewRequest struct {
	FileID string `json:"fileId"`
}

// ResolveEntityRefRequest resolves an entity by canonical target address.
// The address is expected to be pre-normalized during projection (pkg/view).
// LSP does not re-run AST scope resolution here.
type ResolveEntityRefRequest struct {
	TargetFileID   string `json:"targetFileId"`
	TargetEntityID string `json:"targetEntityId"`
}

// ResolveEntityRefResult returns canonical navigation target information.
type ResolveEntityRefResult struct {
	TargetKind     string            `json:"targetKind"`
	TargetName     string            `json:"targetName"`
	TargetFileID   string            `json:"targetFileId"`
	TargetEntityID string            `json:"targetEntityId"`
	TargetAnchor   view.SourceAnchor `json:"targetAnchor"`
}
