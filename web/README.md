# neva-lsp web

Standalone UI for `neva/view/*` APIs.

Boundary contract:
- UI consumes only HTTP JSON endpoints under `/api/view/*`.
- UI does not import compiler/indexer/AST internals.
- Backend stays in Go (`neva-lsp`), frontend can be moved to a separate repo later without API changes.

## Dev

```bash
npm install
npm run dev
```

## Build

```bash
npm run build
```

Build output is served by `neva-lsp -view` from `web/dist`.
