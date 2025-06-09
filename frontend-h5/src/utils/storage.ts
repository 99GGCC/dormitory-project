// storage.ts

interface StorageInterface {
  local: {
    has: (key: string) => boolean
    get: (key: string) => any
    set: (key: string, value: any) => void
    remove: (key: string) => void
    clear: () => void
  }
  session: {
    has: (key: string) => boolean
    get: (key: string) => any
    set: (key: string, value: any) => void
    remove: (key: string) => void
    clear: () => void
  }
}

const storage: StorageInterface = {
  local: {
    has(key: string): boolean {
      return localStorage.getItem(key) !== null
    },
    get(key: string): any {
      try {
        const item = localStorage.getItem(key)
        return item ? JSON.parse(item) : null
      }
      catch (error) {
        return null
      }
    },
    set(key: string, value: any): void {
      localStorage.setItem(key, JSON.stringify(value))
    },
    remove(key: string): void {
      localStorage.removeItem(key)
    },
    clear(): void {
      localStorage.clear()
    },
  },
  session: {
    has(key: string): boolean {
      return sessionStorage.getItem(key) !== null
    },
    get(key: string): any {
      try {
        const item = sessionStorage.getItem(key)
        return item ? JSON.parse(item) : null
      }
      catch (error) {
        return null
      }
    },
    set(key: string, value: any): void {
      sessionStorage.setItem(key, JSON.stringify(value))
    },
    remove(key: string): void {
      sessionStorage.removeItem(key)
    },
    clear(): void {
      sessionStorage.clear()
    },
  },
}

export default storage
