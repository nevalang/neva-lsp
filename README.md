# neva-lsp

Language server for the Neva programming language.

## Install

```bash
make install
```

`make install` builds `web/dist` first and then installs `neva-lsp` with embedded UI assets.

## Tests

```bash
make test-all
```

- `make test` runs Go tests.
- `make test-web` runs frontend unit tests (`vitest`).

## Standalone view

Run from any directory and point to a Neva workspace explicitly:

```bash
neva-lsp --view --view-port=7792 --view-workspace=/absolute/path/to/workspace
```

If you are already inside the workspace:

```bash
make view
```

If you run `neva-lsp --view` directly, make sure UI assets are built first:

```bash
cd web
npm ci
npm run build
```
