package main

import "github.com/nevalang/neva/pkg/view"

const (
	methodGetProgramView   = "neva/view/getProgram"
	methodGetFileView      = "neva/view/getFileView"
	methodResolveEntityRef = "neva/view/resolveEntityRef"
	methodSearchEntities   = "neva/view/searchEntities"
)

// GetProgramViewRequest controls optional server-side explorer filtering.
// Zero-value keeps backward-compatible behavior (all groups included).
type GetProgramViewRequest struct {
	IncludeCurrent *bool `json:"includeCurrent,omitempty"`
	IncludeDeps    *bool `json:"includeDeps,omitempty"`
	IncludeStd     *bool `json:"includeStd,omitempty"`
}

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

// SearchEntitiesRequest performs name search across projected entities.
type SearchEntitiesRequest struct {
	Query          string   `json:"query"`
	Kinds          []string `json:"kinds,omitempty"`
	ModuleFilter   string   `json:"moduleFilter,omitempty"`  // deprecated: prefer ModuleFilters
	PackageFilter  string   `json:"packageFilter,omitempty"` // deprecated: prefer PackageFilters
	ModuleFilters  []string `json:"moduleFilters,omitempty"`
	PackageFilters []string `json:"packageFilters,omitempty"`
	Limit          int      `json:"limit,omitempty"`
}

// SearchEntitiesResultItem is one search match returned to UI clients.
type SearchEntitiesResultItem struct {
	Label    string            `json:"label"`
	Kind     string            `json:"kind"`
	Module   string            `json:"module"`
	Package  string            `json:"package"`
	FileID   string            `json:"fileId"`
	EntityID string            `json:"entityId"`
	Anchor   view.SourceAnchor `json:"anchor"`
}
