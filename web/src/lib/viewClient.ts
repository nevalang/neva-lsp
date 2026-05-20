import type {
  Component,
  Connection,
  Endpoint,
  FileView,
  Interface,
  ManifestView,
  ModuleSummary,
  NodeItem,
  PackageSummary,
  Port,
  Program,
  ResolveEntityRefResult,
  SearchEntitiesResultItem,
} from './types'

async function getJSON<T>(url: string): Promise<T> {
  const response = await fetch(url)
  if (!response.ok) {
    throw new Error(await response.text())
  }
  return (await response.json()) as T
}

async function postJSON<TReq extends object, TRes>(url: string, payload: TReq): Promise<TRes> {
  const response = await fetch(url, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify(payload),
  })
  if (!response.ok) {
    throw new Error(await response.text())
  }
  return (await response.json()) as TRes
}

function normalizeProgram(program: Program): Program {
  return {
    ...program,
    modules: (program.modules ?? []).map(normalizeModule),
  }
}

function normalizeModule(moduleItem: ModuleSummary): ModuleSummary {
  return {
    ...moduleItem,
    packages: (moduleItem.packages ?? []).map(normalizePackage),
  }
}

function normalizePackage(pkg: PackageSummary): PackageSummary {
  return {
    ...pkg,
    fileSummaries: pkg.fileSummaries ?? [],
  }
}

function normalizePort(port: Partial<Port>): Port {
  return {
    name: port.name ?? '',
    type: port.type,
    array: Boolean(port.array),
  }
}

function normalizeNode(node: Partial<NodeItem>): NodeItem {
  return {
    id: node.id ?? '',
    name: node.name ?? '',
    entityRef: node.entityRef,
    resolvedRef: node.resolvedRef,
    anchor: node.anchor,
  }
}

function normalizeEndpoint(endpoint: Partial<Endpoint> | undefined): Endpoint {
  return {
    node: endpoint?.node,
    port: endpoint?.port,
    idx: endpoint?.idx ?? null,
    kind: endpoint?.kind,
    constType: endpoint?.constType,
    constValue: endpoint?.constValue,
  }
}

function normalizeConnection(raw: any): Connection {
  if (raw && Array.isArray(raw.senders) && Array.isArray(raw.receivers) && raw.senders.length > 0 && raw.receivers.length > 0) {
    return {
      id: raw.id ?? '',
      sender: normalizeEndpoint(raw.senders[0]),
      receiver: normalizeEndpoint(raw.receivers[0]),
      signature: raw.signature,
    }
  }

  return {
    id: raw?.id ?? '',
    sender: normalizeEndpoint(raw?.sender),
    receiver: normalizeEndpoint(raw?.receiver),
    signature: raw?.signature,
  }
}

function normalizeComponent(raw: any): Component {
  return {
    id: raw?.id ?? '',
    name: raw?.name ?? '',
    inPorts: (raw?.inPorts ?? raw?.inports ?? []).map((port: Port) => normalizePort(port)),
    outPorts: (raw?.outPorts ?? raw?.outports ?? []).map((port: Port) => normalizePort(port)),
    nodes: (raw?.nodes ?? []).map((node: NodeItem) => normalizeNode(node)),
    connections: (raw?.connections ?? []).map((connection: any) => normalizeConnection(connection)),
    anchor: raw?.anchor,
  }
}

function normalizeInterface(raw: any): Interface {
  return {
    id: raw?.id ?? '',
    name: raw?.name ?? '',
    inPorts: (raw?.inPorts ?? raw?.inports ?? []).map((port: Port) => normalizePort(port)),
    outPorts: (raw?.outPorts ?? raw?.outports ?? []).map((port: Port) => normalizePort(port)),
    anchor: raw?.anchor,
  }
}

function normalizeFile(file: any): FileView {
  return {
    id: file?.id ?? '',
    name: file?.name ?? '',
    components: (file?.components ?? []).map((component: any) => normalizeComponent(component)),
    interfaces: (file?.interfaces ?? []).map((iface: any) => normalizeInterface(iface)),
    types: file?.types ?? [],
    consts: file?.consts ?? [],
    imports: file?.imports ?? [],
  }
}

function normalizeManifest(manifest: ManifestView): ManifestView {
  return {
    ...manifest,
    deps: manifest.deps ?? {},
    raw: manifest.raw ?? '',
    path: manifest.path ?? '',
  }
}

export type ProgramFilters = {
  includeCurrent: boolean
  includeDeps: boolean
  includeStd: boolean
}

export type SearchFilters = {
  query: string
  kinds: string[]
  packages: string[]
  modules: string[]
}

export const viewClient = {
  async getProgram(filters: ProgramFilters): Promise<Program> {
    const query = new URLSearchParams({
      includeCurrent: String(filters.includeCurrent),
      includeDeps: String(filters.includeDeps),
      includeStd: String(filters.includeStd),
    })
    return normalizeProgram(await getJSON<Program>(`/api/view/program?${query.toString()}`))
  },

  async getFileView(fileId: string): Promise<FileView> {
    return normalizeFile(await getJSON<any>(`/api/view/file?id=${encodeURIComponent(fileId)}`))
  },

  searchEntities(filters: SearchFilters): Promise<SearchEntitiesResultItem[]> {
    const query = new URLSearchParams({ q: filters.query.trim() })
    for (const kind of filters.kinds) query.append('kind', kind)
    for (const pkg of filters.packages) query.append('package', pkg)
    for (const modulePath of filters.modules) query.append('module', modulePath)
    return getJSON<SearchEntitiesResultItem[]>(`/api/view/search?${query.toString()}`)
  },

  resolveEntityRef(targetFileId: string, targetEntityId: string): Promise<ResolveEntityRefResult> {
    return postJSON('/api/view/resolve', { targetFileId, targetEntityId })
  },

  async getManifest(modulePath: string): Promise<ManifestView> {
    return normalizeManifest(await getJSON<ManifestView>(`/api/view/manifest?module=${encodeURIComponent(modulePath)}`))
  },
}
