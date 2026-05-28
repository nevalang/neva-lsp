package main

import (
	"fmt"
	"strconv"
	"strings"

	"github.com/nevalang/neva/pkg/ast"
	"github.com/nevalang/neva/pkg/core"
	"github.com/nevalang/neva/pkg/view"
)

func projectFileView(build ast.Build, fileID string) (view.File, bool) {
	fileView, found := view.ProjectFileByID(build, fileID)
	if !found {
		return view.File{}, false
	}
	addChainTriggerConnections(&fileView, build)
	return fileView, true
}

func addChainTriggerConnections(fileView *view.File, build ast.Build) {
	astFile, found := astFileForView(build, *fileView)
	if !found {
		return
	}

	for idx := range fileView.Components {
		componentView := &fileView.Components[idx]
		entity, found := astFile.Entities[componentView.Name]
		if !found || entity.Kind != ast.ComponentEntity || componentView.OverloadIndex >= len(entity.Component) {
			continue
		}
		addComponentChainTriggerConnections(componentView, entity.Component[componentView.OverloadIndex])
	}
}

func astFileForView(build ast.Build, fileView view.File) (ast.File, bool) {
	for modRef, mod := range build.Modules {
		if modRef.Path != fileView.Location.ModulePath {
			continue
		}
		pkg, found := mod.Packages[fileView.Location.Package]
		if !found {
			continue
		}
		file, found := pkg[fileView.Location.File]
		return file, found
	}
	return ast.File{}, false
}

func addComponentChainTriggerConnections(componentView *view.Component, component ast.Component) {
	existing := map[string]struct{}{}
	for _, connection := range componentView.Connections {
		existing[connectionEndpointKey(connection.Sender)+"->"+connectionEndpointKey(connection.Receiver)] = struct{}{}
	}

	nextOrdinal := len(componentView.Connections)
	for _, connection := range component.Net {
		nextOrdinal = addChainTriggersFromReceivers(componentView, existing, nextOrdinal, connection.Senders, connection.Receivers, connection.Meta, 0)
	}
}

func addChainTriggersFromReceivers(
	componentView *view.Component,
	existing map[string]struct{},
	nextOrdinal int,
	outerSenders []ast.ConnectionSender,
	receivers []ast.ConnectionReceiver,
	anchorMeta core.Meta,
	depth int,
) int {
	for _, receiver := range receivers {
		if receiver.ChainedConnection == nil {
			continue
		}
		chained := receiver.ChainedConnection
		for _, outerSender := range outerSenders {
			source := endpointFromConnectionSender(outerSender)
			for _, chainHead := range chained.Senders {
				target := endpointFromConnectionSender(chainHead)
				key := connectionEndpointKey(source) + "->" + connectionEndpointKey(target)
				if _, found := existing[key]; found {
					continue
				}
				existing[key] = struct{}{}
				componentView.Connections = append(componentView.Connections, view.Connection{
					ID:               componentView.ID + "/connection/chain-trigger/" + strconv.Itoa(nextOrdinal),
					Sender:           source,
					Receiver:         target,
					Anchor:           sourceAnchorFromMeta(anchorMeta),
					ChainDepth:       depth,
					ChainPath:        []string{"chain:trigger"},
					Signature:        key,
					DuplicateOrdinal: 0,
				})
				nextOrdinal++
			}
		}
		nextOrdinal = addChainTriggersFromReceivers(componentView, existing, nextOrdinal, chained.Senders, chained.Receivers, chained.Meta, depth+1)
	}
	return nextOrdinal
}

func endpointFromConnectionSender(sender ast.ConnectionSender) view.ConnectionEndpoint {
	if sender.Const != nil {
		return view.ConnectionEndpoint{
			Kind:       "const",
			ConstType:  sender.Const.TypeExpr.String(),
			ConstValue: sender.Const.Value.String(),
			Selector:   append([]string{}, sender.StructSelector...),
			Anchor:     sourceAnchorFromMeta(sender.Const.Meta),
		}
	}
	endpoint := endpointFromPortAddr(sender.PortAddr)
	endpoint.Selector = append([]string{}, sender.StructSelector...)
	endpoint.Anchor = sourceAnchorFromMeta(sender.Meta)
	return endpoint
}

func endpointFromPortAddr(addr *ast.PortAddr) view.ConnectionEndpoint {
	if addr == nil {
		return view.ConnectionEndpoint{Kind: "port"}
	}
	return view.ConnectionEndpoint{
		Kind:     "port",
		Node:     addr.Node,
		Port:     addr.Port,
		Index:    addr.Idx,
		Selector: []string{},
		Anchor:   sourceAnchorFromMeta(addr.Meta),
	}
}

func connectionEndpointKey(endpoint view.ConnectionEndpoint) string {
	if endpoint.Kind == "const" {
		return "const:" + endpoint.ConstType + "=" + endpoint.ConstValue + "." + strings.Join(endpoint.Selector, ".")
	}
	index := ""
	if endpoint.Index != nil {
		index = fmt.Sprintf("[%d]", *endpoint.Index)
	}
	return "port:" + endpoint.Node + ":" + endpoint.Port + index + "." + strings.Join(endpoint.Selector, ".")
}

func sourceAnchorFromMeta(meta core.Meta) view.SourceAnchor {
	return view.SourceAnchor{
		ModulePath: meta.Location.ModRef.Path,
		Package:    meta.Location.Package,
		File:       meta.Location.Filename,
		Text:       meta.Text,
		StartLine:  meta.Start.Line,
		StartCol:   meta.Start.Column,
		EndLine:    meta.Stop.Line,
		EndCol:     meta.Stop.Column,
	}
}
