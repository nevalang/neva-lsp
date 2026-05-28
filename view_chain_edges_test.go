package main

import (
	"testing"

	"github.com/nevalang/neva/pkg/ast"
	"github.com/nevalang/neva/pkg/view"
)

func TestAddChainTriggersFromReceiversConnectsOuterSenderToChainHead(t *testing.T) {
	componentView := view.Component{ID: "component/Main@0"}
	outerSenders := []ast.ConnectionSender{{PortAddr: &ast.PortAddr{Node: "in", Port: "start"}}}
	receivers := []ast.ConnectionReceiver{{
		ChainedConnection: &ast.Connection{
			Senders: []ast.ConnectionSender{{PortAddr: &ast.PortAddr{Node: "wait"}}},
			Receivers: []ast.ConnectionReceiver{{
				PortAddr: &ast.PortAddr{Node: "out", Port: "stop"},
			}},
		},
	}}

	addChainTriggersFromReceivers(&componentView, map[string]struct{}{}, 0, outerSenders, receivers, ast.Connection{}.Meta, 0)

	if len(componentView.Connections) != 1 {
		t.Fatalf("connections len = %d, want 1", len(componentView.Connections))
	}
	got := componentView.Connections[0]
	if got.Sender.Node != "in" || got.Sender.Port != "start" {
		t.Fatalf("sender = %s:%s, want in:start", got.Sender.Node, got.Sender.Port)
	}
	if got.Receiver.Node != "wait" || got.Receiver.Port != "" {
		t.Fatalf("receiver = %s:%s, want wait:", got.Receiver.Node, got.Receiver.Port)
	}
}
