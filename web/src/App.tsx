import { useEffect, useMemo, useState } from 'react'
import { ProgramExplorer } from './components/ProgramExplorer'
import { SearchPanel } from './components/SearchPanel'
import { GraphCanvas } from './components/GraphCanvas'
import { DetailsPanel } from './components/DetailsPanel'
import { ManifestPanel } from './components/ManifestPanel'
import { viewClient } from './lib/viewClient'
import type {
  FileSummary,
  FileView,
  ManifestView,
  ModuleSummary,
  SearchEntitiesResultItem,
} from './lib/types'

type NavState = {
  fileId: string
  fileName: string
}

type Breadcrumb = {
  key: string
  label: string
  action: () => Promise<void> | void
}

function parseFileIDFromHash(): string | null {
  const raw = window.location.hash.replace(/^#/, '').trim()
  if (!raw) {
    return null
  }
  const decoded = decodeURIComponent(raw)
  const split = decoded.split(':')
  return split[0] || null
}

export function App() {
  const [modules, setModules] = useState<ModuleSummary[]>([])
  const [selectedFile, setSelectedFile] = useState<FileView | null>(null)
  const [selectedFileId, setSelectedFileId] = useState<string | null>(null)
  const [manifest, setManifest] = useState<ManifestView | null>(null)
  const [filters, setFilters] = useState({ includeCurrent: true, includeDeps: true, includeStd: true })
  const [backStack, setBackStack] = useState<NavState[]>([])
  const [forwardStack, setForwardStack] = useState<NavState[]>([])

  useEffect(() => {
    void reloadProgram()
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [filters.includeCurrent, filters.includeDeps, filters.includeStd])

  useEffect(() => {
    function onPopState() {
      const fileID = parseFileIDFromHash()
      if (!fileID) {
        return
      }
      void openFile({ id: fileID, name: fileID.split('/').pop() ?? fileID }, false)
    }

    window.addEventListener('popstate', onPopState)
    return () => window.removeEventListener('popstate', onPopState)
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [])

  async function reloadProgram() {
    const program = await viewClient.getProgram(filters)
    setModules(program.modules)

    if (!selectedFileId) {
      const fromHash = parseFileIDFromHash()
      if (fromHash) {
        await openFile({ id: fromHash, name: fromHash.split('/').pop() ?? fromHash }, false)
        return
      }

      const firstFile = program.modules[0]?.packages[0]?.fileSummaries[0]
      if (firstFile) {
        await openFile({ id: firstFile.id, name: firstFile.name }, false)
      }
    }
  }

  function currentNavState(): NavState | null {
    if (!selectedFileId || !selectedFile) {
      return null
    }
    return { fileId: selectedFileId, fileName: selectedFile.name }
  }

  async function openFile(file: FileSummary, trackNav = true) {
    const current = currentNavState()
    if (trackNav && current) {
      setBackStack((prev) => [...prev, current])
      setForwardStack([])
    }

    const fileView = await viewClient.getFileView(file.id)
    setManifest(null)
    setSelectedFile(fileView)
    setSelectedFileId(file.id)
    window.history.pushState({}, '', `#${encodeURIComponent(file.id)}`)
  }

  async function openManifest(modulePath: string) {
    const nextManifest = await viewClient.getManifest(modulePath)
    setManifest(nextManifest)
  }

  async function search(query: string, kinds: string[], packages: string[], modulePaths: string[]): Promise<SearchEntitiesResultItem[]> {
    return viewClient.searchEntities({ query, kinds, packages, modules: modulePaths })
  }

  async function openFromSearch(item: SearchEntitiesResultItem) {
    await openFile({ id: item.fileId, name: item.fileId.split('/').pop() ?? item.label }, true)
    setManifest(null)
  }

  async function openResolved(target: { fileId: string; entityId: string }) {
    const result = await viewClient.resolveEntityRef(target.fileId, target.entityId)
    await openFile({ id: result.targetFileId, name: result.targetName }, true)
    setManifest(null)
  }

  async function goBack() {
    const prev = backStack[backStack.length - 1]
    if (!prev) {
      return
    }

    const current = currentNavState()
    if (current) {
      setForwardStack((items) => [...items, current])
    }

    setBackStack((items) => items.slice(0, -1))
    await openFile({ id: prev.fileId, name: prev.fileName }, false)
  }

  async function goForward() {
    const next = forwardStack[forwardStack.length - 1]
    if (!next) {
      return
    }

    const current = currentNavState()
    if (current) {
      setBackStack((items) => [...items, current])
    }

    setForwardStack((items) => items.slice(0, -1))
    await openFile({ id: next.fileId, name: next.fileName }, false)
  }

  const breadcrumbs = useMemo<Breadcrumb[]>(() => {
    if (!selectedFileId) {
      return []
    }

    const parts = selectedFileId.split('/')
    const moduleIdx = parts.indexOf('module')
    const packageIdx = parts.indexOf('package')
    const fileIdx = parts.indexOf('file')

    const modulePath = moduleIdx >= 0 ? parts[moduleIdx + 1] : ''
    const packageName = packageIdx >= 0 ? parts[packageIdx + 1] : ''
    const fileName = fileIdx >= 0 ? parts[fileIdx + 1] : ''

    if (!(modulePath && packageName && fileName)) {
      return []
    }

    return [{
      key: `filepath:${selectedFileId}`,
      label: `${modulePath} / ${packageName} / ${fileName}.neva`,
      action: async () => {
        await openFile({ id: selectedFileId, name: fileName }, false)
      },
    }]
  }, [modules, selectedFileId])

  return (
    <div className="layout">
      <aside className="left">
        <section className="panel">
          <h2>Scope</h2>
          <label><input type="checkbox" checked={filters.includeCurrent} onChange={(e) => setFilters((prev) => ({ ...prev, includeCurrent: e.target.checked }))} /> current (@)</label>
          <label><input type="checkbox" checked={filters.includeDeps} onChange={(e) => setFilters((prev) => ({ ...prev, includeDeps: e.target.checked }))} /> deps</label>
          <label><input type="checkbox" checked={filters.includeStd} onChange={(e) => setFilters((prev) => ({ ...prev, includeStd: e.target.checked }))} /> std</label>
        </section>

        <SearchPanel modules={modules} onSearch={search} onOpenResult={openFromSearch} />
        <ProgramExplorer
          modules={modules}
          selectedFileId={selectedFileId}
          onSelectFile={(file) => { void openFile(file, true) }}
          onOpenManifest={openManifest}
        />
      </aside>

      <main className="right">
        <div className="topbar">
          <h1>Neva View</h1>
        </div>

        {breadcrumbs.length > 0 && (
          <div className="breadcrumbs panel">
            <div className="breadcrumbs-nav">
              <button onClick={() => void goBack()} disabled={backStack.length === 0}>←</button>
              <button onClick={() => void goForward()} disabled={forwardStack.length === 0}>→</button>
            </div>
            <div className="breadcrumbs-items">
              {breadcrumbs.map((crumb, index) => (
                <span key={crumb.key}>
                  <button className="breadcrumb-link" onClick={() => void crumb.action()}>{crumb.label}</button>
                  {index < breadcrumbs.length - 1 ? <span className="breadcrumb-sep"> / </span> : null}
                </span>
              ))}
            </div>
          </div>
        )}

        <ManifestPanel manifest={manifest} />
        <DetailsPanel file={selectedFile} />
        <GraphCanvas file={selectedFile} onNodeOpen={openResolved} />
      </main>
    </div>
  )
}
