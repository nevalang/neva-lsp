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

func TestAddChainTriggersFromReceiversSkipsSelectorOnlyHeadAndConnectsConcreteDescendant(t *testing.T) {
	componentView := view.Component{ID: "component/Main@0"}
	outerSenders := []ast.ConnectionSender{{PortAddr: &ast.PortAddr{Node: "in", Port: "state"}}}
	receivers := []ast.ConnectionReceiver{{
		ChainedConnection: &ast.Connection{
			Senders: []ast.ConnectionSender{{StructSelector: []string{"rate"}}},
			Receivers: []ast.ConnectionReceiver{{
				ChainedConnection: &ast.Connection{
					Senders: []ast.ConnectionSender{{PortAddr: &ast.PortAddr{Node: "mul_rate_balance", Port: "left"}}},
				},
			}},
		},
	}}

	addChainTriggersFromReceivers(&componentView, map[string]struct{}{}, 0, outerSenders, receivers, ast.Connection{}.Meta, 0)

	if len(componentView.Connections) != 1 {
		t.Fatalf("connections len = %d, want 1", len(componentView.Connections))
	}
	got := componentView.Connections[0]
	if got.Sender.Node != "in" || got.Sender.Port != "state" {
		t.Fatalf("sender = %s:%s, want in:state", got.Sender.Node, got.Sender.Port)
	}
	if len(got.Sender.Selector) != 1 || got.Sender.Selector[0] != "rate" {
		t.Fatalf("sender selector = %#v, want [rate]", got.Sender.Selector)
	}
	if got.Receiver.Node != "mul_rate_balance" || got.Receiver.Port != "left" {
		t.Fatalf("receiver = %s:%s, want mul_rate_balance:left", got.Receiver.Node, got.Receiver.Port)
	}
}
