import { useEffect, useState } from 'react'
import {
  Background,
  Controls,
  Handle,
  MiniMap,
  Position,
  ReactFlow,
  type ReactFlowInstance,
  type Edge,
  type EdgeMouseHandler,
  type Node,
  type NodeProps,
} from '@xyflow/react'
import ELK from 'elkjs/lib/elk.bundled.js'
import { endpointPortName, inferImplicitPortName, parseSignaturePorts, shouldAddImplicitErrEdge, shouldAddImplicitInputEdge } from '../lib/graphSemantics'
import type { Component, Endpoint, FileView, ModuleSummary, Port } from '../lib/types'

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
  onNativeComponentClick: (target: { fileId: string; entityId: string }) => void
}

type NodeData = {
  kind: 'entity' | 'port' | 'nav' | 'const'
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
  if (data.kind === 'const') {
    return (
      <div className="rf-const-node">
        <Handle type="target" position={Position.Top} style={{ left: '50%' }} />
        <div className="rf-node-title">{data.label}</div>
        {data.subtitle ? <div className="rf-node-subtitle">{data.subtitle}</div> : null}
        <Handle type="source" position={Position.Bottom} style={{ left: '50%' }} />
      </div>
    )
  }

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
  const portHandleStyle = { opacity: 1, pointerEvents: 'none' as const, zIndex: 2 }

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
          style={showPortBars ? { ...portHandleStyle, left } : { left }}
        />
      ))}
      <div className="rf-node-body">
        <div className="rf-node-title">{data.label}</div>
        {data.showMeta && data.subtitle && <div className="rf-node-subtitle">{data.subtitle}</div>}
      </div>
      {outHandles.map((left, idx) => (
        <Handle
          key={`en-out-${idx}`}
          id={handleIDForPort(data.outPorts?.[idx]?.name ?? `out-${idx}`)}
          type="source"
          position={Position.Bottom}
          style={showPortBars ? { ...portHandleStyle, left } : { left }}
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

function constNodeID(componentID: string, endpoint: Endpoint): string {
  return `${componentID}::const::${endpoint.constType ?? ''}::${endpoint.constValue ?? ''}`
}

function minimapNodeFill(node: Node<NodeData>, theme: 'light' | 'dark'): string {
  const navType = node.data?.navType
  if (navType === 'module') return theme === 'dark' ? '#7ea8cf' : '#356287'
  if (navType === 'package') return theme === 'dark' ? '#d8ad7f' : '#8c5a2f'
  if (navType === 'file') return theme === 'dark' ? '#93c8a3' : '#326f56'
  if (navType === 'component') return theme === 'dark' ? '#e3ba8d' : '#9a6734'
  if (navType === 'interface') return theme === 'dark' ? '#94b6dd' : '#44698e'
  if (navType === 'type') return theme === 'dark' ? '#86bfd6' : '#2f6f97'
  if (navType === 'const') return theme === 'dark' ? '#e6c58a' : '#8a5a2a'
  return theme === 'dark' ? '#9ca6b5' : '#566276'
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

function resolveNodeID(component: Component, endpoint: Endpoint): string | null {
  if (endpoint.kind === 'const') {
    return constNodeID(component.id, endpoint)
  }
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
    className: 'rf-node-clickable rf-node-kind-module',
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
    className: 'rf-node-clickable rf-node-kind-package',
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
    className: 'rf-node-clickable rf-node-kind-file',
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
    className: canDrillComponent(component) ? 'rf-node-clickable rf-node-kind-component' : 'rf-node-kind-component',
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
    className: 'rf-node-kind-interface',
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
    className: 'rf-node-kind-type',
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
    className: 'rf-node-kind-const',
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
  const constNodes = new Map<string, Endpoint>()

  for (const connection of component.connections) {
    if (connection.sender?.kind === 'const') {
      constNodes.set(constNodeID(component.id, connection.sender), connection.sender)
    }
    if (connection.receiver?.kind === 'const') {
      constNodes.set(constNodeID(component.id, connection.receiver), connection.receiver)
    }
    if (connection.sender?.node && connection.sender.node !== 'in' && connection.sender.node !== 'out') {
      const item = nodePortKinds.get(connection.sender.node) ?? { in: new Map<string, string>(), out: new Map<string, string>() }
      const portName = endpointPortName(component, connection.sender, 'out')
      item.out.set(portName, '')
      nodePortKinds.set(connection.sender.node, item)
    }
    if (connection.receiver?.node && connection.receiver.node !== 'in' && connection.receiver.node !== 'out') {
      const item = nodePortKinds.get(connection.receiver.node) ?? { in: new Map<string, string>(), out: new Map<string, string>() }
      const portName = endpointPortName(component, connection.receiver, 'in')
      item.in.set(portName, '')
      nodePortKinds.set(connection.receiver.node, item)
    }
  }

  for (const node of component.nodes) {
    const inferredInPort = inferImplicitPortName(component, node.name, 'in')
    if (shouldAddImplicitInputEdge(component, node.name, inferredInPort)) {
      const item = nodePortKinds.get(node.name) ?? { in: new Map<string, string>(), out: new Map<string, string>() }
      const componentInType = component.inPorts.find((port) => port.name === inferredInPort)?.type ?? ''
      item.in.set(inferredInPort, componentInType)
      nodePortKinds.set(node.name, item)
    }

    if (!shouldAddImplicitErrEdge(component, node.name)) {
      continue
    }
    const item = nodePortKinds.get(node.name) ?? { in: new Map<string, string>(), out: new Map<string, string>() }
    item.out.set('err', 'error')
    nodePortKinds.set(node.name, item)
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

  for (const [id, endpoint] of constNodes.entries()) {
    result.push({
      id,
      type: 'entityNode',
      className: 'rf-node-kind-literal',
      position: { x: 0, y: 0 },
      data: {
        kind: 'const',
        label: endpoint.constValue ?? '?',
        subtitle: endpoint.constType,
        showMeta,
      },
    })
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
      className: node.resolvedRef?.fileId && node.resolvedRef?.entityId ? 'rf-node-clickable rf-node-kind-call' : 'rf-node-kind-call',
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
      type: 'step',
      source,
      target,
      sourceHandle: connection.sender?.node && connection.sender.node !== 'in' && connection.sender.node !== 'out'
        ? handleIDForPort(endpointPortName(component, connection.sender, 'out'))
        : undefined,
      targetHandle: connection.receiver?.node && connection.receiver.node !== 'in' && connection.receiver.node !== 'out'
        ? handleIDForPort(endpointPortName(component, connection.receiver, 'in'))
        : undefined,
      label: normalizeEdgeLabel(`${formatEndpointLabel(connection.sender)} -> ${formatEndpointLabel(connection.receiver)}`),
    })
  }

  for (const node of component.nodes) {
    const inferredInPort = inferImplicitPortName(component, node.name, 'in')
    if (shouldAddImplicitInputEdge(component, node.name, inferredInPort)) {
      result.push({
        id: `${component.id}/implicit_in/${inferredInPort}->${node.name}`,
        type: 'step',
        source: endpointNodeID(component.id, 'in', inferredInPort),
        target: endpointNodeID(component.id, node.name),
        targetHandle: handleIDForPort(inferredInPort),
      })
    }

    if (!shouldAddImplicitErrEdge(component, node.name)) {
      continue
    }
    result.push({
      id: `${component.id}/implicit_err/${node.name}`,
      type: 'step',
      source: endpointNodeID(component.id, node.name),
      target: endpointNodeID(component.id, 'out', 'err'),
      sourceHandle: handleIDForPort('err'),
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
      'elk.spacing.nodeNode': '72',
      'elk.layered.spacing.nodeNodeBetweenLayers': '96',
      'elk.layered.spacing.edgeNodeBetweenLayers': '30',
    },
    children: nodes.map((node) => ({
      id: node.id,
      width: node.data.kind === 'port' ? 120 : node.data.kind === 'const' ? 92 : 320,
      height: node.data.kind === 'port' ? 70 : node.data.kind === 'const' ? 72 : 120,
    })),
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
  onNativeComponentClick,
}: Props) {
  const [nodes, setNodes] = useState<Node<NodeData>[]>([])
  const [edges, setEdges] = useState<Edge[]>([])
  const [selectedEdgeID, setSelectedEdgeID] = useState<string | null>(null)
  const [flow, setFlow] = useState<ReactFlowInstance<Node<NodeData>, Edge> | null>(null)
  const [layoutVersion, setLayoutVersion] = useState(0)
  const [copyDone, setCopyDone] = useState(false)
  const [theme, setTheme] = useState<'light' | 'dark'>(() => {
    if (typeof window !== 'undefined' && window.matchMedia && window.matchMedia('(prefers-color-scheme: dark)').matches) {
      return 'dark'
    }
    return 'light'
  })

  useEffect(() => {
    document.documentElement.setAttribute('data-theme', theme)
  }, [theme])

  useEffect(() => {
    setSelectedEdgeID(null)
  }, [route.kind])

  useEffect(() => {
    if (!flow || nodes.length === 0 || layoutVersion === 0) {
      return
    }
    const frame = window.requestAnimationFrame(() => {
      flow.fitView({ duration: 0, padding: 0.2 })
    })
    return () => window.cancelAnimationFrame(frame)
  }, [flow, layoutVersion, nodes.length])

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
          nextNodes = componentDetailNodes(component, true)
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
        setLayoutVersion((v) => v + 1)
      }
    }

    void run()
    return () => {
      canceled = true
    }
  }, [modules, route, file, selectedEdgeID])

  const onEdgeClick: EdgeMouseHandler = (_, edge) => {
    setSelectedEdgeID(edge.id)
  }

  async function copyCurrentURL() {
    try {
      await navigator.clipboard.writeText(window.location.href)
      setCopyDone(true)
      window.setTimeout(() => setCopyDone(false), 1200)
    } catch {
      setCopyDone(false)
    }
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

  return (
    <section className="canvas-shell">
      <div className="canvas-overlay">
        <div className="canvas-nav-buttons">
          <div className="canvas-nav-left">
            <button onClick={onGoBack} disabled={!canGoBack}>←</button>
            <button onClick={onGoForward} disabled={!canGoForward}>→</button>
          </div>
          <div className="canvas-nav-right">
            <button
              className="canvas-theme-toggle"
              onClick={() => setTheme((current) => current === 'light' ? 'dark' : 'light')}
              title={theme === 'light' ? 'Switch to dark theme' : 'Switch to light theme'}
              aria-label={theme === 'light' ? 'Switch to dark theme' : 'Switch to light theme'}
            >
              {theme === 'light' ? '🌙' : '☀️'}
            </button>
          </div>
        </div>
        <div className="canvas-breadcrumbs">
          <div className="canvas-breadcrumbs-links">
            {breadcrumbs.map((crumb, index) => (
              <span key={crumb.key}>
                <button className="breadcrumb-link" onClick={() => onNavigate(crumb.route, true)}>{crumb.label}</button>
                {index < breadcrumbs.length - 1 ? <span className="breadcrumb-sep"> / </span> : null}
              </span>
            ))}
          </div>
          <button
            className="canvas-copy-url"
            onClick={() => void copyCurrentURL()}
            title="Copy URL"
            aria-label="Copy URL"
          >
            {copyDone ? '✅' : '📋'}
          </button>
        </div>
      </div>

      <ReactFlow
        nodes={nodes}
        edges={edges}
        defaultEdgeOptions={{
          type: 'step',
          style: { strokeWidth: 1.5 },
        }}
        nodeTypes={nodeTypes}
        nodesDraggable
        fitView
        onInit={setFlow}
        onEdgeClick={onEdgeClick}
        onPaneClick={() => setSelectedEdgeID(null)}
        onNodeClick={(_, node) => {
          const nextRoute = routeDown(route, node)
          if (nextRoute) {
            onNavigate(nextRoute, true)
            return
          }
          if (route.kind === 'file' && node.data.navType === 'component' && node.data.fileId && node.data.entityId) {
            onNativeComponentClick({ fileId: node.data.fileId, entityId: node.data.entityId })
            return
          }
          if (route.kind === 'component' && node.data.fileId && node.data.entityId) {
            void onResolveOpen({ fileId: node.data.fileId, entityId: node.data.entityId })
          }
        }}
      >
        <MiniMap
          key={`minimap-${theme}`}
          pannable
          zoomable
          nodeClassName="minimap-node"
          nodeColor={(node) => minimapNodeFill(node as Node<NodeData>, theme)}
          nodeStrokeColor={theme === 'dark' ? '#d6dbe3' : '#3f4650'}
          nodeStrokeWidth={2}
          nodeBorderRadius={4}
          bgColor={theme === 'dark' ? '#3b414c' : '#d3d8e0'}
          maskColor={theme === 'dark' ? 'rgba(8, 10, 14, 0.22)' : 'rgba(255, 255, 255, 0.22)'}
          maskStrokeColor={theme === 'dark' ? '#e1e6ef' : '#3f4650'}
          maskStrokeWidth={1.25}
        />
        <Controls />
        <Background />
      </ReactFlow>
    </section>
  )
}
