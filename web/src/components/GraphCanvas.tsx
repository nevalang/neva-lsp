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
} from '@xyflow/react'
import ELK from 'elkjs/lib/elk.bundled.js'
import type { Component, FileView, Interface, Port } from '../lib/types'

type Props = {
  file: FileView | null
  onNodeOpen: (target: { fileId: string; entityId: string }) => Promise<void>
}

type CanvasMode =
  | { kind: 'overview' }
  | { kind: 'component'; componentId: string }

type NodeData = {
  kind: 'entity' | 'port'
  portRole?: 'in' | 'out'
  label: string
  subtitle?: string
  showMeta?: boolean
  inPorts?: Port[]
  outPorts?: Port[]
  fileId?: string
  entityId?: string
}

const elk = new ELK()

function handleOffsets(count: number): string[] {
  if (count <= 0) {
    return []
  }
  if (count === 1) {
    return ['50%']
  }
  const offsets: string[] = []
  for (let i = 0; i < count; i++) {
    offsets.push(`${((i + 1) * 100) / (count + 1)}%`)
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
  const hiddenHandleStyle = { opacity: 0, pointerEvents: 'none' as const }

  return (
    <div className="rf-node">
      {showPortBars && (data.inPorts?.length ?? 0) > 0 && (
        <div className="rf-node-port-row rf-node-port-row-top">
          {data.inPorts?.map((port, idx) => (
            <span key={`in-pill-${port.name}`} className="rf-port-pill rf-port-pill-top" style={{ left: inHandles[idx] }}>
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
      {showPortBars && (data.outPorts?.length ?? 0) > 0 && (
        <div className="rf-node-port-row rf-node-port-row-bottom">
          {data.outPorts?.map((port, idx) => (
            <span key={`out-pill-${port.name}`} className="rf-port-pill rf-port-pill-bottom" style={{ left: outHandles[idx] }}>
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

function componentOverviewNode(component: Component): Node<NodeData> {
  return {
    id: `overview::${component.id}`,
    type: 'entityNode',
    position: { x: 0, y: 0 },
    data: {
      kind: 'entity',
      label: component.name,
      subtitle: 'component',
      showMeta: true,
      inPorts: component.inPorts,
      outPorts: component.outPorts,
      entityId: component.id,
    },
  }
}

function interfaceOverviewNode(iface: Interface): Node<NodeData> {
  return {
    id: `overview::${iface.id}`,
    type: 'entityNode',
    position: { x: 0, y: 0 },
    data: {
      kind: 'entity',
      label: iface.name,
      subtitle: 'interface',
      showMeta: true,
      inPorts: iface.inPorts,
      outPorts: iface.outPorts,
    },
  }
}

function componentDetailNodes(component: Component, showMeta: boolean): Node<NodeData>[] {
  const result: Node<NodeData>[] = []
  const nodePortKinds = new Map<string, { in: Set<string>; out: Set<string> }>()

  for (const connection of component.connections) {
    if (connection.sender?.node && connection.sender.node !== 'in' && connection.sender.node !== 'out') {
      const item = nodePortKinds.get(connection.sender.node) ?? { in: new Set<string>(), out: new Set<string>() }
      item.out.add(connection.sender.port || 'sig')
      nodePortKinds.set(connection.sender.node, item)
    }
    if (connection.receiver?.node && connection.receiver.node !== 'in' && connection.receiver.node !== 'out') {
      const item = nodePortKinds.get(connection.receiver.node) ?? { in: new Set<string>(), out: new Set<string>() }
      item.in.add(connection.receiver.port || 'sig')
      nodePortKinds.set(connection.receiver.node, item)
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
        inPorts: Array.from(nodePortKinds.get(node.name)?.in ?? []).map((name) => ({ name, type: '' })),
        outPorts: Array.from(nodePortKinds.get(node.name)?.out ?? []).map((name) => ({ name, type: '' })),
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

async function applyLayout(nodes: Node<NodeData>[], edges: Edge[]): Promise<Node<NodeData>[]> {
  const graph = {
    id: 'root',
    layoutOptions: {
      'elk.algorithm': 'layered',
      'elk.direction': 'DOWN',
      'elk.spacing.nodeNode': '48',
      'elk.layered.spacing.nodeNodeBetweenLayers': '60',
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

export function GraphCanvas({ file, onNodeOpen }: Props) {
  const [mode, setMode] = useState<CanvasMode>({ kind: 'overview' })
  const [nodes, setNodes] = useState<Node<NodeData>[]>([])
  const [edges, setEdges] = useState<Edge[]>([])
  const [selectedEdgeID, setSelectedEdgeID] = useState<string | null>(null)
  const [zoom, setZoom] = useState(1)

  useEffect(() => {
    setMode({ kind: 'overview' })
    setSelectedEdgeID(null)
  }, [file?.id])

  const title = useMemo(() => {
    if (!file) return 'Canvas'
    if (mode.kind === 'overview') return `File canvas: ${file.name}.neva`
    const component = file.components.find((item) => item.id === mode.componentId)
    return component ? `Component: ${component.name}` : `File canvas: ${file.name}.neva`
  }, [file, mode])

  useEffect(() => {
    let canceled = false

    async function run() {
      if (!file) {
        setNodes([])
        setEdges([])
        return
      }

      let nextNodes: Node<NodeData>[] = []
      let nextEdges: Edge[] = []

      if (mode.kind === 'overview') {
        nextNodes = [
          ...file.components.map(componentOverviewNode),
          ...file.interfaces.map(interfaceOverviewNode),
        ]
      } else {
        const component = file.components.find((item) => item.id === mode.componentId)
        if (component) {
          nextNodes = componentDetailNodes(component, zoom >= 1.2)
          nextEdges = componentDetailEdges(component).map((edge) => ({
            ...edge,
            label: edge.id === selectedEdgeID ? edge.label : '',
          }))
        }
      }

      const laidOut = await applyLayout(nextNodes, nextEdges)
      if (!canceled) {
        setNodes(laidOut)
        setEdges(nextEdges)
      }
    }

    void run()
    return () => {
      canceled = true
    }
  }, [file, mode, selectedEdgeID, zoom])

  const onEdgeClick: EdgeMouseHandler = (_, edge) => {
    setSelectedEdgeID(edge.id)
  }

  if (!file) {
    return <div className="panel">Select file to render canvas.</div>
  }

  return (
    <section className="panel graph-panel">
      <div className="graph-panel-head">
        <h2>{title}</h2>
        {mode.kind === 'component' && <button onClick={() => setMode({ kind: 'overview' })}>Back to file</button>}
      </div>
      <div className="graph-root">
        <ReactFlow
          nodes={nodes}
          edges={edges}
          nodeTypes={nodeTypes}
          nodesDraggable
          fitView
          onEdgeClick={onEdgeClick}
          onPaneClick={() => setSelectedEdgeID(null)}
          onMove={(_, viewport) => setZoom(viewport.zoom)}
          onNodeClick={(_, node) => {
            if (mode.kind === 'overview' && node.data.entityId) {
              setMode({ kind: 'component', componentId: node.data.entityId })
              return
            }

            if (mode.kind === 'component' && node.data.fileId && node.data.entityId) {
              void onNodeOpen({ fileId: node.data.fileId, entityId: node.data.entityId })
            }
          }}
        >
          <MiniMap />
          <Controls />
          <Background />
        </ReactFlow>
      </div>
    </section>
  )
}
