package main

import (
	"embed"
	"io/fs"
)

//go:embed web/dist/*
var embeddedWebDist embed.FS

func embeddedWebDistFS() (fs.FS, error) {
	return fs.Sub(embeddedWebDist, "web/dist")
}
