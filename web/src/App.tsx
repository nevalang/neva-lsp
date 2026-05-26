import { useEffect, useMemo, useState } from 'react'
import { GraphCanvas } from './components/GraphCanvas'
import { viewClient } from './lib/viewClient'
import type { FileView, ModuleSummary } from './lib/types'

type Route =
  | { kind: 'modules' }
  | { kind: 'module'; modulePath: string }
  | { kind: 'package'; modulePath: string; packageName: string }
  | { kind: 'file'; fileId: string }
  | { kind: 'component'; fileId: string; componentId: string }

type Breadcrumb = {
  key: string
  label: string
  route: Route
}

function parseHashRoute(): Route {
  const hash = window.location.hash.replace(/^#/, '')
  if (!hash) {
    return { kind: 'modules' }
  }

  const params = new URLSearchParams(hash)
  const kind = params.get('k')

  if (kind === 'module') {
    const modulePath = params.get('m')
    if (modulePath) {
      return { kind: 'module', modulePath }
    }
  }

  if (kind === 'package') {
    const modulePath = params.get('m')
    const packageName = params.get('p')
    if (modulePath && packageName) {
      return { kind: 'package', modulePath, packageName }
    }
  }

  if (kind === 'file') {
    const fileId = params.get('f')
    if (fileId) {
      return { kind: 'file', fileId }
    }
  }

  if (kind === 'component') {
    const fileId = params.get('f')
    const componentId = params.get('c')
    if (fileId && componentId) {
      return { kind: 'component', fileId, componentId }
    }
  }

  return { kind: 'modules' }
}

function routeToHash(route: Route): string {
  const params = new URLSearchParams()
  params.set('k', route.kind)

  if (route.kind === 'module') {
    params.set('m', route.modulePath)
  }

  if (route.kind === 'package') {
    params.set('m', route.modulePath)
    params.set('p', route.packageName)
  }

  if (route.kind === 'file') {
    params.set('f', route.fileId)
  }

  if (route.kind === 'component') {
    params.set('f', route.fileId)
    params.set('c', route.componentId)
  }

  return `#${params.toString()}`
}

function routeKey(route: Route): string {
  return JSON.stringify(route)
}

function routeFileID(route: Route): string | null {
  if (route.kind === 'file' || route.kind === 'component') {
    return route.fileId
  }
  return null
}

function fileDisplayName(fileID: string): string {
  const parts = fileID.split('/')
  const moduleIdx = parts.indexOf('module')
  const packageIdx = parts.indexOf('package')
  const fileIdx = parts.indexOf('file')

  const modulePath = moduleIdx >= 0 ? parts[moduleIdx + 1] : ''
  const packageName = packageIdx >= 0 ? parts[packageIdx + 1] : ''
  const fileName = fileIdx >= 0 ? parts[fileIdx + 1] : ''

  if (modulePath && packageName && fileName) {
    return `${modulePath}/${packageName}/${fileName}.neva`
  }

  return fileID
}

export function App() {
  const [modules, setModules] = useState<ModuleSummary[]>([])
  const [route, setRoute] = useState<Route>(() => parseHashRoute())
  const [fileCache, setFileCache] = useState<Record<string, FileView>>({})
  const [backStack, setBackStack] = useState<Route[]>([])
  const [forwardStack, setForwardStack] = useState<Route[]>([])

  useEffect(() => {
    void reloadProgram()
  }, [])

  useEffect(() => {
    const fileID = routeFileID(route)
    if (!fileID || fileCache[fileID]) {
      return
    }

    void viewClient.getFileView(fileID).then((file) => {
      setFileCache((prev) => ({ ...prev, [fileID]: file }))
    })
  }, [route, fileCache])

  useEffect(() => {
    function onPopState() {
      setRoute(parseHashRoute())
    }

    window.addEventListener('popstate', onPopState)
    return () => window.removeEventListener('popstate', onPopState)
  }, [])

  async function reloadProgram() {
    const program = await viewClient.getProgram({
      includeCurrent: true,
      includeDeps: true,
      includeStd: true,
    })
    setModules(program.modules)
  }

  function navigate(next: Route, trackNav = true) {
    if (trackNav && routeKey(route) !== routeKey(next)) {
      setBackStack((prev) => [...prev, route])
      setForwardStack([])
    }

    setRoute(next)
    window.history.pushState({}, '', routeToHash(next))
  }

  function goBack() {
    const prev = backStack[backStack.length - 1]
    if (!prev) {
      return
    }

    setBackStack((items) => items.slice(0, -1))
    setForwardStack((items) => [...items, route])
    setRoute(prev)
    window.history.pushState({}, '', routeToHash(prev))
  }

  function goForward() {
    const next = forwardStack[forwardStack.length - 1]
    if (!next) {
      return
    }

    setForwardStack((items) => items.slice(0, -1))
    setBackStack((items) => [...items, route])
    setRoute(next)
    window.history.pushState({}, '', routeToHash(next))
  }

  const selectedFile = useMemo(() => {
    const fileID = routeFileID(route)
    if (!fileID) {
      return null
    }
    return fileCache[fileID] ?? null
  }, [route, fileCache])

  const breadcrumbs = useMemo<Breadcrumb[]>(() => {
    if (route.kind === 'modules') {
      return [{ key: 'modules', label: 'modules', route: { kind: 'modules' } }]
    }

    if (route.kind === 'module') {
      return [
        { key: 'modules', label: 'modules', route: { kind: 'modules' } },
        { key: `module:${route.modulePath}`, label: route.modulePath, route },
      ]
    }

    if (route.kind === 'package') {
      return [
        { key: 'modules', label: 'modules', route: { kind: 'modules' } },
        { key: `module:${route.modulePath}`, label: route.modulePath, route: { kind: 'module', modulePath: route.modulePath } },
        { key: `package:${route.modulePath}:${route.packageName}`, label: route.packageName, route },
      ]
    }

    if (route.kind === 'file') {
      const fileID = route.fileId
      const parts = fileID.split('/')
      const modulePath = parts[parts.indexOf('module') + 1] ?? ''
      const packageName = parts[parts.indexOf('package') + 1] ?? ''
      return [
        { key: 'modules', label: 'modules', route: { kind: 'modules' } },
        { key: `module:${modulePath}`, label: modulePath, route: { kind: 'module', modulePath } },
        { key: `package:${modulePath}:${packageName}`, label: packageName, route: { kind: 'package', modulePath, packageName } },
        { key: `file:${fileID}`, label: fileDisplayName(fileID), route },
      ]
    }

    const fileID = route.fileId
    const parts = fileID.split('/')
    const modulePath = parts[parts.indexOf('module') + 1] ?? ''
    const packageName = parts[parts.indexOf('package') + 1] ?? ''
    const componentName = route.componentId.split('/').pop() ?? route.componentId

    return [
      { key: 'modules', label: 'modules', route: { kind: 'modules' } },
      { key: `module:${modulePath}`, label: modulePath, route: { kind: 'module', modulePath } },
      { key: `package:${modulePath}:${packageName}`, label: packageName, route: { kind: 'package', modulePath, packageName } },
      { key: `file:${fileID}`, label: fileDisplayName(fileID), route: { kind: 'file', fileId: fileID } },
      { key: `component:${route.componentId}`, label: componentName, route },
    ]
  }, [route])

  async function resolveAndOpen(target: { fileId: string; entityId: string }) {
    const result = await viewClient.resolveEntityRef(target.fileId, target.entityId)
    const nextRoute: Route = { kind: 'file', fileId: result.targetFileId }
    navigate(nextRoute, true)
  }

  return (
    <main className="single-canvas-layout">
      <GraphCanvas
        modules={modules}
        route={route}
        file={selectedFile}
        breadcrumbs={breadcrumbs}
        canGoBack={backStack.length > 0}
        canGoForward={forwardStack.length > 0}
        onGoBack={goBack}
        onGoForward={goForward}
        onNavigate={navigate}
        onResolveOpen={resolveAndOpen}
      />
    </main>
  )
}
