import type { FileView } from '../lib/types'

type Props = {
  file: FileView | null
}

export function DetailsPanel({ file }: Props) {
  if (!file) {
    return <section className="panel">Select file.</section>
  }

  return (
    <section className="panel">
      <h2>File: {file.name}.neva</h2>
      <div className="summary-row">
        components {file.components.length} · interfaces {file.interfaces.length} · types {file.types.length} · consts {file.consts.length}
      </div>

      {file.imports.length > 0 && (
        <>
          <h3>imports</h3>
          {file.imports.map((item) => {
            const target = item.path && item.path.length > 0 ? item.path : [item.module, item.package].filter(Boolean).join('/')
            return (
              <div key={item.id ?? `${item.alias ?? ''}:${target}`}>
                {item.alias ? `${item.alias} -> ${target}` : target}
              </div>
            )
          })}
        </>
      )}

      <h3>components</h3>
      {file.components.map((component) => (
        <div key={component.id}>{component.name}</div>
      ))}

      <h3>interfaces</h3>
      {file.interfaces.map((iface) => (
        <div key={iface.id}>{iface.name}</div>
      ))}

      <h3>types</h3>
      {file.types.map((item) => (
        <div key={item.id}>{item.name}</div>
      ))}

      <h3>consts</h3>
      {file.consts.map((item) => (
        <div key={item.id}>{item.name}</div>
      ))}
    </section>
  )
}
