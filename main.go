package main

import (
	"flag"
	"fmt"

	"github.com/nevalang/neva/pkg/indexer"
	"github.com/tliron/commonlog"
	_ "github.com/tliron/commonlog/simple"
	"github.com/tliron/glsp/server"
)

func main() {
	const serverName = "neva"
	const defaultStandalonePort = 7788

	isDebug := flag.Bool("debug", false, "-debug")
	standaloneView := flag.Bool("view", false, "-view")
	viewPort := flag.Int("view-port", defaultStandalonePort, "-view-port")
	viewOpen := flag.Bool("view-open", false, "-view-open")
	flag.Parse()

	loglvl := 1
	if *isDebug {
		loglvl = 2
	}

	commonlog.Configure(loglvl, nil)
	logger := commonlog.GetLoggerf("%s.server", serverName)

	if *standaloneView {
		listenAddr, err := standaloneListenAddr(*viewPort)
		if err != nil {
			panic(err)
		}
		if err := runStandaloneView(logger, ".", listenAddr, *viewOpen); err != nil {
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

func standaloneListenAddr(port int) (string, error) {
	if port < 1 || port > 65535 {
		return "", fmt.Errorf("view-port must be in range [1,65535], got %d", port)
	}
	return fmt.Sprintf("127.0.0.1:%d", port), nil
}
