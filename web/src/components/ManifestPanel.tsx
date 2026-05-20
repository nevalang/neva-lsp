import type { ManifestView } from '../lib/types'

type Props = {
  manifest: ManifestView | null
}

export function ManifestPanel({ manifest }: Props) {
  if (!manifest) {
    return null
  }

  return (
    <section className="panel">
      <h2>neva.yaml</h2>
      <div>{manifest.path || '(not found)'}</div>
      {!manifest.present && <div>Manifest is not available in this local workspace.</div>}
      <h3>deps</h3>
      {Object.keys(manifest.deps).length === 0 ? (
        <div>No deps</div>
      ) : (
        Object.entries(manifest.deps)
          .sort(([a], [b]) => a.localeCompare(b))
          .map(([name, version]) => <div key={name}>{name} {'->'} {version}</div>)
      )}
      <pre>{manifest.raw || ''}</pre>
    </section>
  )
}
