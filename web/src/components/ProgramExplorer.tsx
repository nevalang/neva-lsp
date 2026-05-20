import type { FileSummary, ModuleSummary } from '../lib/types'

type Props = {
  modules: ModuleSummary[]
  selectedFileId: string | null
  onSelectFile: (file: FileSummary) => void
  onOpenManifest: (modulePath: string) => void
}

export function ProgramExplorer({ modules, selectedFileId, onSelectFile, onOpenManifest }: Props) {
  return (
    <section className="panel">
      <h2>Program Explorer</h2>
      {modules.map((moduleItem) => (
        <div key={moduleItem.path} className="module-block">
          <div className="module-title">
            module {moduleItem.path}
          </div>
          <button onClick={() => onOpenManifest(moduleItem.path)}>neva.yaml</button>
          {moduleItem.packages.map((pkg) => (
            <div key={`${moduleItem.path}/${pkg.name}`} className="package-block">
              <div className="package-title">{pkg.name}</div>
              {pkg.fileSummaries.map((file) => {
                const active = selectedFileId === file.id
                return (
                  <button
                    key={file.id}
                    className={active ? 'active' : ''}
                    onClick={() => onSelectFile(file)}
                  >
                    {file.name}.neva
                  </button>
                )
              })}
            </div>
          ))}
        </div>
      ))}
    </section>
  )
}
