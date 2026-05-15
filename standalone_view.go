package main

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"net/http"
	"os/exec"
	"runtime"

	src "github.com/nevalang/neva/pkg/ast"
	"github.com/nevalang/neva/pkg/indexer"
	"github.com/nevalang/neva/pkg/view"
	"github.com/tliron/commonlog"
)

func runStandaloneView(logger commonlog.Logger, workspacePath string, listenAddr string, openBrowserFlag bool) error {
	idx, err := indexer.NewDefault(logger)
	if err != nil {
		return fmt.Errorf("create indexer: %w", err)
	}

	build, found, scanErr := idx.FullScan(context.Background(), workspacePath)
	if scanErr != nil {
		return fmt.Errorf("scan workspace: %w", scanErr)
	}
	if !found {
		return errors.New("no Neva module found in workspace")
	}

	mux := http.NewServeMux()
	mux.HandleFunc("/api/view/program", func(w http.ResponseWriter, _ *http.Request) {
		writeJSON(w, view.ProjectProgram(build))
	})
	mux.HandleFunc("/api/view/file", func(w http.ResponseWriter, req *http.Request) {
		fileID := req.URL.Query().Get("id")
		if fileID == "" {
			http.Error(w, "missing id", http.StatusBadRequest)
			return
		}
		fileView, ok := view.ProjectFileByID(build, fileID)
		if !ok {
			http.Error(w, "file not found", http.StatusNotFound)
			return
		}
		writeJSON(w, fileView)
	})
	mux.HandleFunc("/api/view/resolve", func(w http.ResponseWriter, req *http.Request) {
		if req.Method != http.MethodPost {
			http.Error(w, "method not allowed", http.StatusMethodNotAllowed)
			return
		}
		var payload ResolveEntityRefRequest
		if err := json.NewDecoder(req.Body).Decode(&payload); err != nil {
			http.Error(w, "invalid request", http.StatusBadRequest)
			return
		}
		result, err := resolveEntityRefInBuild(&build, payload)
		if err != nil {
			http.Error(w, err.Error(), http.StatusBadRequest)
			return
		}
		writeJSON(w, result)
	})
	mux.HandleFunc("/", func(w http.ResponseWriter, _ *http.Request) {
		_, _ = w.Write([]byte(standaloneViewHTML))
	})

	url := "http://" + listenAddr
	logger.Info("standalone visual view running", "url", url)
	if openBrowserFlag {
		_ = openBrowser(url)
	}

	server := &http.Server{Addr: listenAddr, Handler: mux}
	return server.ListenAndServe()
}

func resolveEntityRefInBuild(build *src.Build, params ResolveEntityRefRequest) (ResolveEntityRefResult, error) {
	if params.FileID == "" {
		return ResolveEntityRefResult{}, errors.New("fileId is required")
	}
	if params.EntityRef.Name == "" {
		return ResolveEntityRefResult{}, errors.New("entityRef.name is required")
	}

	loc, found := fileLocationByID(build, params.FileID)
	if !found {
		return ResolveEntityRefResult{}, fmt.Errorf("file not found: %s", params.FileID)
	}

	scope := src.NewScope(*build, loc)
	entity, targetLoc, err := scope.Entity(params.EntityRef)
	if err != nil {
		return ResolveEntityRefResult{}, fmt.Errorf("resolve entity ref: %w", err)
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

func writeJSON(w http.ResponseWriter, payload any) {
	w.Header().Set("Content-Type", "application/json")
	if err := json.NewEncoder(w).Encode(payload); err != nil {
		panic(err)
	}
}

func openBrowser(url string) error {
	var cmd *exec.Cmd
	switch runtime.GOOS {
	case "darwin":
		cmd = exec.Command("open", url)
	case "windows":
		cmd = exec.Command("rundll32", "url.dll,FileProtocolHandler", url)
	default:
		cmd = exec.Command("xdg-open", url)
	}
	return cmd.Start()
}

const standaloneViewHTML = `<!doctype html>
<html>
<head>
  <meta charset="utf-8" />
  <title>Neva View (Read-only)</title>
  <style>
    body { font-family: sans-serif; margin: 0; display: grid; grid-template-columns: 420px 1fr; height: 100vh; }
    #left { border-right: 1px solid #ddd; overflow: auto; padding: 12px; }
    #right { overflow: auto; padding: 12px; }
    h2,h3,h4 { margin: 8px 0; }
    button { margin: 4px 0; width: 100%; text-align: left; }
    pre { background: #f6f6f6; padding: 10px; overflow: auto; }
  </style>
</head>
<body>
  <div id="left">
    <h2>Program Explorer</h2>
    <div id="tree"></div>
  </div>
  <div id="right">
    <h2>Details</h2>
    <div id="details">Select a file or entity.</div>
  </div>
<script>
(async function () {
  const details = document.getElementById('details');
  const tree = document.getElementById('tree');
  const program = await (await fetch('/api/view/program')).json();

  for (const mod of program.modules) {
    const modTitle = document.createElement('h3');
    modTitle.textContent = 'Module: ' + mod.path + (mod.version ? '@' + mod.version : '');
    tree.appendChild(modTitle);

    for (const pkg of mod.packages) {
      const pkgTitle = document.createElement('h4');
      pkgTitle.textContent = 'Package: ' + pkg.name;
      tree.appendChild(pkgTitle);

      for (const file of pkg.files) {
        const btn = document.createElement('button');
        btn.textContent = 'File ' + file.name;
        btn.onclick = async () => {
          const fileView = await (await fetch('/api/view/file?id=' + encodeURIComponent(file.id))).json();
          details.innerHTML = '';
          const title = document.createElement('h3');
          title.textContent = fileView.name;
          details.appendChild(title);

          const meta = document.createElement('pre');
          meta.textContent = JSON.stringify(fileView, null, 2);
          details.appendChild(meta);
        };
        tree.appendChild(btn);
      }
    }
  }
})();
</script>
</body>
</html>`
