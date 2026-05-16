package main

import (
	"flag"

	"github.com/nevalang/neva/pkg/indexer"
	"github.com/tliron/commonlog"
	_ "github.com/tliron/commonlog/simple"
	"github.com/tliron/glsp/server"
)

func main() {
	const serverName = "neva"

	isDebug := flag.Bool("debug", false, "-debug")
	standaloneView := flag.Bool("standalone-view", false, "-standalone-view")
	viewWorkspace := flag.String("view-workspace", ".", "-view-workspace")
	viewListen := flag.String("view-listen", "127.0.0.1:7788", "-view-listen")
	viewOpen := flag.Bool("view-open", false, "-view-open")
	flag.Parse()

	loglvl := 1
	if *isDebug {
		loglvl = 2
	}

	commonlog.Configure(loglvl, nil)
	logger := commonlog.GetLoggerf("%s.server", serverName)

	if *standaloneView {
		if err := runStandaloneView(logger, *viewWorkspace, *viewListen, *viewOpen); err != nil {
			panic(err)
		}
		return
	}

	indexer := indexer.MustNewDefault(logger)

	handler := BuildHandler(logger, serverName, indexer)

	srv := server.NewServer(
		handler,
		serverName,
		*isDebug,
	)

	if *isDebug {
		if err := srv.RunTCP("localhost:6007"); err != nil {
			panic(err)
		}
	} else {
		if err := srv.RunStdio(); err != nil {
			panic(err)
		}
	}
}
