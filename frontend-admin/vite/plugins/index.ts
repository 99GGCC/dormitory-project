import type { PluginOption } from 'vite'
import vue from '@vitejs/plugin-vue'
import vueJsx from '@vitejs/plugin-vue-jsx'
import vueLegacy from '@vitejs/plugin-legacy'
import createDevtools from './devtools'
import createAutoImport from './auto-import'
import createComponents from './components'
import createVxeTable from './vxe-resolve'
import createUnocss from './unocss'
import createSvgIcon from './svg-icon'
import createMock from './mock'
import createLayouts from './layouts'
import createPages from './pages'
import createCompression from './compression'
import createArchiver from './archiver'

export default function createVitePlugins(viteEnv, isBuild = false) {
  const vitePlugins: (PluginOption | PluginOption[])[] = [
    vue(),
    vueJsx(),
    vueLegacy({
      renderLegacyChunks: false,
      modernPolyfills: [
        'es.array.at',
        'es.array.find-last',
      ],
    }),
  ]
  vitePlugins.push(createDevtools(viteEnv))
  vitePlugins.push(createAutoImport())
  vitePlugins.push(createComponents())
  vitePlugins.push(createVxeTable())
  vitePlugins.push(createUnocss())
  vitePlugins.push(createSvgIcon(isBuild))
  vitePlugins.push(createMock(viteEnv, isBuild))
  vitePlugins.push(createLayouts())
  vitePlugins.push(createPages())
  vitePlugins.push(...createCompression(viteEnv, isBuild))
  vitePlugins.push(createArchiver(viteEnv))
  return vitePlugins
}
