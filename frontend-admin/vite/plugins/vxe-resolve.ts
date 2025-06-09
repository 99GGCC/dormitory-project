import { VxeTableResolve, createStyleImportPlugin } from 'vite-plugin-style-import'

export default function createComponents() {
  return createStyleImportPlugin({
    resolves: [
      VxeTableResolve(),
    ],
  })
}
