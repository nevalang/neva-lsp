import { useEffect, useMemo, useState } from 'react'
import {
  Background,
  Controls,
  Handle,
  MiniMap,
  Position,
  ReactFlow,
  type Edge,
  type EdgeMouseHandler,
  type Node,
  type NodeProps,
  type NodeMouseHandler,
} from '@xyflow/react'
import ELK from 'elkjs/lib/elk.bundled.js'
import type { Component, FileView, ModuleSummary, Port } from '../lib/types'

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

type Props = {
  modules: ModuleSummary[]
  route: Route
  file: FileView | null
  breadcrumbs: Breadcrumb[]
  canGoBack: boolean
  canGoForward: boolean
  onGoBack: () => void
  onGoForward: () => void
  onNavigate: (route: Route, trackNav?: boolean) => void
  onResolveOpen: (target: { fileId: string; entityId: string }) => Promise<void>
}

type NodeData = {
  kind: 'entity' | 'port' | 'nav'
  navType?: 'module' | 'package' | 'file' | 'component' | 'interface' | 'type' | 'const'
  portRole?: 'in' | 'out'
  label: string
  subtitle?: string
  showMeta?: boolean
  inPorts?: Port[]
  outPorts?: Port[]
  fileId?: string
  entityId?: string
  modulePath?: string
  packageName?: string
  componentId?: string
}

const elk = new ELK()

function handleOffsets(count: number): string[] {
  if (count <= 0) {
    return []
  }
  const offsets: string[] = []
  for (let i = 0; i < count; i++) {
    offsets.push(`${((i + 0.5) * 100) / count}%`)
  }
  return offsets
}

function handleIDForPort(portName: string): string {
  return `port:${portName}`
}

function EntityNode({ data }: NodeProps<Node<NodeData>>) {
  if (data.kind === 'port') {
    return (
      <div className="rf-port-node">
        {data.portRole === 'out' ? <Handle type="target" position={Position.Top} style={{ left: '50%' }} /> : null}
        <div className="rf-port-name">{data.label}</div>
        {data.showMeta && data.subtitle ? <div className="rf-port-type">{data.subtitle}</div> : null}
        {data.portRole === 'in' ? <Handle type="source" position={Position.Bottom} style={{ left: '50%' }} /> : null}
      </div>
    )
  }

  const inHandles = handleOffsets(data.inPorts?.length ?? 0)
  const outHandles = handleOffsets(data.outPorts?.length ?? 0)
  const showPortBars = Boolean(data.showMeta)
  const hasInPorts = (data.inPorts?.length ?? 0) > 0
  const hasOutPorts = (data.outPorts?.length ?? 0) > 0
  const hiddenHandleStyle = { opacity: 0, pointerEvents: 'none' as const }

  return (
    <div className={`rf-node${showPortBars && hasInPorts ? ' rf-node-has-inbars' : ''}${showPortBars && hasOutPorts ? ' rf-node-has-outbars' : ''}`}>
      {showPortBars && hasInPorts && (
        <div className="rf-node-port-row rf-node-port-row-top">
          {data.inPorts?.map((port, idx) => (
            <span
              key={`in-pill-${port.name}`}
              className="rf-port-pill rf-port-pill-top"
              style={{ left: inHandles[idx], width: `${100 / (data.inPorts?.length || 1)}%` }}
            >
              <span className="rf-port-name">{port.name}</span>
              {port.type ? <span className="rf-port-type">{port.type}</span> : null}
            </span>
          ))}
        </div>
      )}
      {inHandles.map((left, idx) => (
        <Handle
          key={`en-in-${idx}`}
          id={handleIDForPort(data.inPorts?.[idx]?.name ?? `in-${idx}`)}
          type="target"
          position={Position.Top}
          style={showPortBars ? { ...hiddenHandleStyle, left } : { left }}
        />
      ))}
      <div className="rf-node-title">{data.label}</div>
      {data.showMeta && data.subtitle && <div className="rf-node-subtitle">{data.subtitle}</div>}
      {outHandles.map((left, idx) => (
        <Handle
          key={`en-out-${idx}`}
          id={handleIDForPort(data.outPorts?.[idx]?.name ?? `out-${idx}`)}
          type="source"
          position={Position.Bottom}
          style={showPortBars ? { ...hiddenHandleStyle, left } : { left }}
        />
      ))}
      {showPortBars && hasOutPorts && (
        <div className="rf-node-port-row rf-node-port-row-bottom">
          {data.outPorts?.map((port, idx) => (
            <span
              key={`out-pill-${port.name}`}
              className="rf-port-pill rf-port-pill-bottom"
              style={{ left: outHandles[idx], width: `${100 / (data.outPorts?.length || 1)}%` }}
            >
              <span className="rf-port-name">{port.name}</span>
              {data.showMeta && port.type ? <span className="rf-port-type">{port.type}</span> : null}
            </span>
          ))}
        </div>
      )}
    </div>
  )
}

const nodeTypes = { entityNode: EntityNode }

function endpointNodeID(componentID: string, localNodeName: string, portName?: string): string {
  if (localNodeName === 'in') {
    return `${componentID}::in::${portName ?? '_'}`
  }
  if (localNodeName === 'out') {
    return `${componentID}::out::${portName ?? '_'}`
  }
  return `${componentID}::node::${localNodeName}`
}

function formatEndpointLabel(endpoint: { node?: string; port?: string }): string {
  const baseName = endpoint.node === 'in' || endpoint.node === 'out' ? '' : (endpoint.node ?? '')
  if (baseName.length > 0 && endpoint.port && endpoint.port.length > 0) {
    return `${baseName}:${endpoint.port}`
  }
  if (endpoint.port && endpoint.port.length > 0) {
    return endpoint.port
  }
  if (baseName.length > 0) {
    return baseName
  }
  return '?'
}

function normalizeEdgeLabel(label: string): string {
  return label
    .replace(/\bin:/g, '')
    .replace(/\bout:/g, '')
    .replace(/\s+/g, ' ')
    .trim()
}

function parsePortList(raw: string, knownNames: string[]): Map<string, string> {
  const result = new Map<string, string>()
  const known = [...knownNames].sort((a, b) => b.length - a.length)
  for (const chunk of raw.split(',')) {
    const item = chunk.trim()
    if (!item) continue
    let name = ''
    for (const candidate of known) {
      if (item.startsWith(candidate)) {
        name = candidate
        break
      }
    }
    if (!name) {
      const fallback = item.match(/^([A-Za-z_][A-Za-z0-9_]*)(.*)$/)
      if (!fallback) continue
      name = fallback[1]
    }
    const type = item.slice(name.length).trim()
    result.set(name, type)
  }
  return result
}

function parseSignaturePorts(
  signatureText: string | undefined,
  knownInNames: string[],
  knownOutNames: string[],
): { in: Map<string, string>; out: Map<string, string> } {
  if (!signatureText) {
    return { in: new Map<string, string>(), out: new Map<string, string>() }
  }
  const normalized = signatureText.replace(/\s+/g, '')
  const pair = normalized.match(/\(([^()]*)\)\(([^()]*)\)/)
  if (!pair) {
    return { in: new Map<string, string>(), out: new Map<string, string>() }
  }
  return {
    in: parsePortList(pair[1] ?? '', knownInNames),
    out: parsePortList(pair[2] ?? '', knownOutNames),
  }
}

function resolveNodeID(component: Component, endpoint: { node?: string; port?: string }): string | null {
  if (!endpoint.node) {
    return null
  }
  if (endpoint.node === 'in') {
    const port = endpoint.port || component.inPorts[0]?.name
    return endpointNodeID(component.id, 'in', port)
  }
  if (endpoint.node === 'out') {
    const port = endpoint.port || component.outPorts[0]?.name
    return endpointNodeID(component.id, 'out', port)
  }
  return endpointNodeID(component.id, endpoint.node)
}

function moduleNodes(modules: ModuleSummary[]): Node<NodeData>[] {
  return modules.map((mod) => ({
    id: `module:${mod.path}`,
    type: 'entityNode',
    position: { x: 0, y: 0 },
    data: {
      kind: 'nav',
      navType: 'module',
      label: mod.path,
      subtitle: `${mod.packages.length} packages`,
      showMeta: true,
      modulePath: mod.path,
    },
  }))
}

function packageNodes(modules: ModuleSummary[], modulePath: string): Node<NodeData>[] {
  const moduleItem = modules.find((item) => item.path === modulePath)
  if (!moduleItem) return []
  return moduleItem.packages.map((pkg) => ({
    id: `package:${modulePath}:${pkg.name}`,
    type: 'entityNode',
    position: { x: 0, y: 0 },
    data: {
      kind: 'nav',
      navType: 'package',
      label: pkg.name,
      subtitle: `${pkg.fileSummaries.length} files`,
      showMeta: true,
      modulePath,
      packageName: pkg.name,
    },
  }))
}

function fileNodes(modules: ModuleSummary[], modulePath: string, packageName: string): Node<NodeData>[] {
  const moduleItem = modules.find((item) => item.path === modulePath)
  const pkg = moduleItem?.packages.find((item) => item.name === packageName)
  if (!pkg) return []
  return pkg.fileSummaries.map((file) => ({
    id: `file:${file.id}`,
    type: 'entityNode',
    position: { x: 0, y: 0 },
    data: {
      kind: 'nav',
      navType: 'file',
      label: `${file.name}.neva`,
      subtitle: `${modulePath}/${packageName}`,
      showMeta: true,
      fileId: file.id,
    },
  }))
}

function canDrillComponent(component: Component): boolean {
  return component.nodes.length > 0 || component.connections.length > 0
}

function fileEntityNodes(file: FileView): Node<NodeData>[] {
  const components = file.components.map((component) => ({
    id: `entity:${component.id}`,
    type: 'entityNode' as const,
    position: { x: 0, y: 0 },
    data: {
      kind: 'nav' as const,
      navType: 'component' as const,
      label: component.name,
      subtitle: canDrillComponent(component) ? 'component' : 'component (native)',
      showMeta: true,
      fileId: file.id,
      entityId: component.id,
      componentId: component.id,
      inPorts: component.inPorts,
      outPorts: component.outPorts,
    },
  }))

  const interfaces = file.interfaces.map((iface) => ({
    id: `entity:${iface.id}`,
    type: 'entityNode' as const,
    position: { x: 0, y: 0 },
    data: {
      kind: 'nav' as const,
      navType: 'interface' as const,
      label: iface.name,
      subtitle: 'interface',
      showMeta: true,
      fileId: file.id,
      entityId: iface.id,
      inPorts: iface.inPorts,
      outPorts: iface.outPorts,
    },
  }))

  const types = file.types.map((item) => ({
    id: `entity:${item.id}`,
    type: 'entityNode' as const,
    position: { x: 0, y: 0 },
    data: {
      kind: 'nav' as const,
      navType: 'type' as const,
      label: item.name,
      subtitle: 'type',
      showMeta: true,
      fileId: file.id,
      entityId: item.id,
    },
  }))

  const consts = file.consts.map((item) => ({
    id: `entity:${item.id}`,
    type: 'entityNode' as const,
    position: { x: 0, y: 0 },
    data: {
      kind: 'nav' as const,
      navType: 'const' as const,
      label: item.name,
      subtitle: 'const',
      showMeta: true,
      fileId: file.id,
      entityId: item.id,
    },
  }))

  return [...components, ...interfaces, ...types, ...consts]
}

function componentDetailNodes(component: Component, showMeta: boolean): Node<NodeData>[] {
  const result: Node<NodeData>[] = []
  const nodePortKinds = new Map<string, { in: Map<string, string>; out: Map<string, string> }>()

  for (const connection of component.connections) {
    if (connection.sender?.node && connection.sender.node !== 'in' && connection.sender.node !== 'out') {
      const item = nodePortKinds.get(connection.sender.node) ?? { in: new Map<string, string>(), out: new Map<string, string>() }
      const portName = connection.sender.port || 'sig'
      item.out.set(portName, '')
      nodePortKinds.set(connection.sender.node, item)
    }
    if (connection.receiver?.node && connection.receiver.node !== 'in' && connection.receiver.node !== 'out') {
      const item = nodePortKinds.get(connection.receiver.node) ?? { in: new Map<string, string>(), out: new Map<string, string>() }
      const portName = connection.receiver.port || 'sig'
      item.in.set(portName, '')
      nodePortKinds.set(connection.receiver.node, item)
    }
  }

  for (const node of component.nodes) {
    const ports = nodePortKinds.get(node.name)
    if (!ports) continue
    const parsed = parseSignaturePorts(
      node.resolvedRef?.anchor?.text,
      Array.from(ports.in.keys()),
      Array.from(ports.out.keys()),
    )
    for (const [name, t] of parsed.in.entries()) {
      if (ports.in.has(name)) ports.in.set(name, t)
    }
    for (const [name, t] of parsed.out.entries()) {
      if (ports.out.has(name)) ports.out.set(name, t)
    }
  }

  for (const port of component.inPorts) {
    result.push({
      id: endpointNodeID(component.id, 'in', port.name),
      type: 'entityNode',
      position: { x: 0, y: 0 },
      data: {
        kind: 'port',
        portRole: 'in',
        label: port.name,
        subtitle: port.type,
        showMeta,
      },
    })
  }

  for (const node of component.nodes) {
    const ref = node.resolvedRef
    const localRef = (node.entityRef && typeof node.entityRef === 'object') ? (node.entityRef as Record<string, unknown>) : null
    const pkg = typeof localRef?.pkg === 'string' ? localRef.pkg : ''
    const name = typeof localRef?.name === 'string' ? localRef.name : ''
    const sourceLikeRef = pkg && name ? `${pkg}.${name}` : (name || ref?.canonicalRef)
    result.push({
      id: endpointNodeID(component.id, node.name),
      type: 'entityNode',
      position: { x: 0, y: 0 },
      data: {
        kind: 'entity',
        label: node.name,
        subtitle: sourceLikeRef,
        showMeta,
        inPorts: Array.from(nodePortKinds.get(node.name)?.in.entries() ?? []).map(([name, type]) => ({ name, type })),
        outPorts: Array.from(nodePortKinds.get(node.name)?.out.entries() ?? []).map(([name, type]) => ({ name, type })),
        fileId: ref?.fileId,
        entityId: ref?.entityId,
      },
    })
  }

  for (const port of component.outPorts) {
    result.push({
      id: endpointNodeID(component.id, 'out', port.name),
      type: 'entityNode',
      position: { x: 0, y: 0 },
      data: {
        kind: 'port',
        portRole: 'out',
        label: port.name,
        subtitle: port.type,
        showMeta,
      },
    })
  }

  return result
}

function componentDetailEdges(component: Component): Edge[] {
  const result: Edge[] = []
  for (const connection of component.connections) {
    const source = resolveNodeID(component, connection.sender)
    const target = resolveNodeID(component, connection.receiver)
    if (!source || !target) continue
    result.push({
      id: connection.id,
      source,
      target,
      sourceHandle: connection.sender?.node && connection.sender.node !== 'in' && connection.sender.node !== 'out'
        ? handleIDForPort(connection.sender.port || 'sig')
        : undefined,
      targetHandle: connection.receiver?.node && connection.receiver.node !== 'in' && connection.receiver.node !== 'out'
        ? handleIDForPort(connection.receiver.port || 'sig')
        : undefined,
      label: normalizeEdgeLabel(`${formatEndpointLabel(connection.sender)} -> ${formatEndpointLabel(connection.receiver)}`),
    })
  }
  return result
}

async function applyLayout(nodes: Node<NodeData>[], edges: Edge[], direction: 'DOWN' | 'RIGHT' = 'DOWN'): Promise<Node<NodeData>[]> {
  const graph = {
    id: 'root',
    layoutOptions: {
      'elk.algorithm': 'layered',
      'elk.direction': direction,
      'elk.spacing.nodeNode': '40',
      'elk.layered.spacing.nodeNodeBetweenLayers': '58',
    },
    children: nodes.map((node) => ({ id: node.id, width: node.data.kind === 'port' ? 120 : 320, height: node.data.kind === 'port' ? 70 : 120 })),
    edges: edges.map((edge) => ({ id: edge.id, sources: [edge.source], targets: [edge.target] })),
  }

  const layout = await elk.layout(graph)
  const byID = new Map((layout.children ?? []).map((item) => [item.id, item]))

  return nodes.map((node) => {
    const placed = byID.get(node.id)
    return {
      ...node,
      position: {
        x: placed?.x ?? 0,
        y: placed?.y ?? 0,
      },
    }
  })
}

export function GraphCanvas({
  modules,
  route,
  file,
  breadcrumbs,
  canGoBack,
  canGoForward,
  onGoBack,
  onGoForward,
  onNavigate,
  onResolveOpen,
}: Props) {
  const [nodes, setNodes] = useState<Node<NodeData>[]>([])
  const [edges, setEdges] = useState<Edge[]>([])
  const [selectedEdgeID, setSelectedEdgeID] = useState<string | null>(null)
  const [zoom, setZoom] = useState(1)
  const [routeBaseZoom, setRouteBaseZoom] = useState<number | null>(null)
  const [focusedNodeID, setFocusedNodeID] = useState<string | null>(null)
  const [navLock, setNavLock] = useState(false)

  useEffect(() => {
    setSelectedEdgeID(null)
    setRouteBaseZoom(null)
    setNavLock(false)
  }, [route.kind])

  const title = useMemo(() => {
    if (route.kind === 'modules') return 'Modules'
    if (route.kind === 'module') return `Module: ${route.modulePath}`
    if (route.kind === 'package') return `Package: ${route.modulePath}/${route.packageName}`
    if (route.kind === 'file') return file ? `File: ${file.name}.neva` : 'Loading file...'
    const component = file?.components.find((item) => item.id === route.componentId)
    return component ? `Component: ${component.name}` : 'Loading component...'
  }, [route, file])

  useEffect(() => {
    let canceled = false

    async function run() {
      let nextNodes: Node<NodeData>[] = []
      let nextEdges: Edge[] = []
      let direction: 'DOWN' | 'RIGHT' = 'DOWN'

      if (route.kind === 'modules') {
        nextNodes = moduleNodes(modules)
      }

      if (route.kind === 'module') {
        nextNodes = packageNodes(modules, route.modulePath)
      }

      if (route.kind === 'package') {
        nextNodes = fileNodes(modules, route.modulePath, route.packageName)
      }

      if (route.kind === 'file') {
        if (!file) {
          setNodes([])
          setEdges([])
          return
        }
        nextNodes = fileEntityNodes(file)
      }

      if (route.kind === 'component') {
        if (!file) {
          setNodes([])
          setEdges([])
          return
        }

        const component = file.components.find((item) => item.id === route.componentId)
        if (component) {
          nextNodes = componentDetailNodes(component, zoom >= 1.2)
          nextEdges = componentDetailEdges(component).map((edge) => ({
            ...edge,
            label: edge.id === selectedEdgeID ? edge.label : '',
          }))
          direction = 'DOWN'
        }
      }

      const laidOut = await applyLayout(nextNodes, nextEdges, direction)
      if (!canceled) {
        setNodes(laidOut)
        setEdges(nextEdges)
      }
    }

    void run()
    return () => {
      canceled = true
    }
  }, [modules, route, file, selectedEdgeID, zoom])

  const onEdgeClick: EdgeMouseHandler = (_, edge) => {
    setSelectedEdgeID(edge.id)
  }

  const onNodeMouseEnter: NodeMouseHandler<Node<NodeData>> = (_, node) => {
    setFocusedNodeID(node.id)
  }

  function routeUp(current: Route): Route | null {
    if (current.kind === 'component') {
      return { kind: 'file', fileId: current.fileId }
    }
    if (current.kind === 'file') {
      const parts = current.fileId.split('/')
      const modulePath = parts[parts.indexOf('module') + 1] ?? ''
      const packageName = parts[parts.indexOf('package') + 1] ?? ''
      if (modulePath && packageName) {
        return { kind: 'package', modulePath, packageName }
      }
      return { kind: 'modules' }
    }
    if (current.kind === 'package') {
      return { kind: 'module', modulePath: current.modulePath }
    }
    if (current.kind === 'module') {
      return { kind: 'modules' }
    }
    return null
  }

  function routeDown(current: Route, node: Node<NodeData>): Route | null {
    if (current.kind === 'modules' && node.data.modulePath) {
      return { kind: 'module', modulePath: node.data.modulePath }
    }
    if (current.kind === 'module' && node.data.modulePath && node.data.packageName) {
      return { kind: 'package', modulePath: node.data.modulePath, packageName: node.data.packageName }
    }
    if (current.kind === 'package' && node.data.fileId) {
      return { kind: 'file', fileId: node.data.fileId }
    }
    if (current.kind === 'file' && node.data.navType === 'component' && node.data.fileId && node.data.componentId) {
      const component = file?.components.find((item) => item.id === node.data.componentId)
      if (component && canDrillComponent(component)) {
        return { kind: 'component', fileId: node.data.fileId, componentId: node.data.componentId }
      }
    }
    return null
  }

  function withNavCooldown(fn: () => void) {
    setNavLock(true)
    fn()
    window.setTimeout(() => setNavLock(false), 240)
  }

  return (
    <section className="canvas-shell">
      <div className="canvas-overlay">
        <div className="canvas-topline">
          <div className="canvas-nav-buttons">
            <button onClick={onGoBack} disabled={!canGoBack}>←</button>
            <button onClick={onGoForward} disabled={!canGoForward}>→</button>
          </div>
          <h1>Neva View</h1>
        </div>
        <div className="canvas-breadcrumbs">
          {breadcrumbs.map((crumb, index) => (
            <span key={crumb.key}>
              <button className="breadcrumb-link" onClick={() => onNavigate(crumb.route, true)}>{crumb.label}</button>
              {index < breadcrumbs.length - 1 ? <span className="breadcrumb-sep"> / </span> : null}
            </span>
          ))}
        </div>
        <div className="canvas-title">{title}</div>
      </div>

      <ReactFlow
        nodes={nodes}
        edges={edges}
        nodeTypes={nodeTypes}
        nodesDraggable
        fitView
        onEdgeClick={onEdgeClick}
        onPaneClick={() => setSelectedEdgeID(null)}
        onMove={(_, viewport) => {
          const nextZoom = viewport.zoom
          setZoom(nextZoom)

          if (routeBaseZoom === null) {
            setRouteBaseZoom(nextZoom)
            return
          }

          if (navLock) {
            return
          }

          const zoomInThreshold = routeBaseZoom * 1.18
          const zoomOutThreshold = routeBaseZoom * 0.82

          if (nextZoom >= zoomInThreshold && focusedNodeID) {
            const focusedNode = nodes.find((n) => n.id === focusedNodeID)
            if (focusedNode) {
              const nextRoute = routeDown(route, focusedNode)
              if (nextRoute) {
                withNavCooldown(() => onNavigate(nextRoute, true))
                return
              }
            }
          }

          if (nextZoom <= zoomOutThreshold) {
            const upRoute = routeUp(route)
            if (upRoute) {
              withNavCooldown(() => onNavigate(upRoute, true))
            }
          }
        }}
        onNodeMouseEnter={onNodeMouseEnter}
        onNodeClick={(_, node) => {
          setFocusedNodeID(node.id)
          if (route.kind === 'component' && node.data.fileId && node.data.entityId) {
            void onResolveOpen({ fileId: node.data.fileId, entityId: node.data.entityId })
          }
        }}
      >
        <MiniMap />
        <Controls />
        <Background />
      </ReactFlow>
    </section>
  )
}
