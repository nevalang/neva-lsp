import { useMemo, useState } from 'react'
import type { ModuleSummary, SearchEntitiesResultItem } from '../lib/types'

type Props = {
  modules: ModuleSummary[]
  onSearch: (query: string, kinds: string[], packages: string[], modules: string[]) => Promise<SearchEntitiesResultItem[]>
  onOpenResult: (item: SearchEntitiesResultItem) => Promise<void>
}

const KIND_OPTIONS = ['component', 'interface', 'type', 'const']

export function SearchPanel({ modules, onSearch, onOpenResult }: Props) {
  const [query, setQuery] = useState('')
  const [results, setResults] = useState<SearchEntitiesResultItem[]>([])
  const [kinds, setKinds] = useState<string[]>(KIND_OPTIONS)
  const [selectedModules, setSelectedModules] = useState<string[]>([])
  const [selectedPackages, setSelectedPackages] = useState<string[]>([])

  const packageValues = useMemo(() => {
    const values: string[] = []
    for (const moduleItem of modules) {
      for (const pkg of moduleItem.packages) {
        values.push(`${moduleItem.path}/${pkg.name}`)
      }
    }
    return values
  }, [modules])

  function toggle(setter: (next: string[]) => void, values: string[], value: string) {
    if (values.includes(value)) {
      setter(values.filter((item) => item !== value))
    } else {
      setter([...values, value])
    }
  }

  async function submit() {
    if (!query.trim()) {
      setResults([])
      return
    }
    const found = await onSearch(query, kinds, selectedPackages, selectedModules)
    setResults(found)
  }

  return (
    <details className="panel">
      <summary>Search</summary>
      <div className="search-grid">
        <input value={query} onChange={(e) => setQuery(e.target.value)} placeholder="Search entities..." />
        <div className="check-row">
          {KIND_OPTIONS.map((kind) => (
            <label key={kind}>
              <input
                type="checkbox"
                checked={kinds.includes(kind)}
                onChange={() => toggle(setKinds, kinds, kind)}
              />
              {kind}
            </label>
          ))}
        </div>
        <div className="check-row">
          {modules.map((moduleItem) => (
            <label key={moduleItem.path}>
              <input
                type="checkbox"
                checked={selectedModules.includes(moduleItem.path)}
                onChange={() => toggle(setSelectedModules, selectedModules, moduleItem.path)}
              />
              {moduleItem.path}
            </label>
          ))}
        </div>
        <div className="package-groups">
          {modules.map((moduleItem) => (
            <div key={moduleItem.path} className="package-group">
              <div className="package-group-title">{moduleItem.path}</div>
              <div className="check-row">
                {moduleItem.packages.map((pkg) => {
                  const value = `${moduleItem.path}/${pkg.name}`
                  return (
                    <label key={value}>
                      <input
                        type="checkbox"
                        checked={selectedPackages.includes(value) || (selectedPackages.length === 0 && packageValues.length > 0)}
                        onChange={() => toggle(setSelectedPackages, selectedPackages, value)}
                      />
                      {pkg.name}
                    </label>
                  )
                })}
              </div>
            </div>
          ))}
        </div>
        <button onClick={submit}>Search</button>
        <div className="search-results">
          {results.map((item) => (
            <button key={`${item.fileId}:${item.entityId}`} onClick={() => onOpenResult(item)}>
              {item.label} [{item.kind}] {item.module}/{item.package}
            </button>
          ))}
        </div>
      </div>
    </details>
  )
}
