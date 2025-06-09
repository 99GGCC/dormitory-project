interface Config {
  key: string
  version: string
  plugins?: string[]
  errorHandler?: Function
}

const config: Config = {
  key: '',
  version: '2.0',
  errorHandler: undefined,
  plugins: [],
}

export default config
