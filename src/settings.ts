import { defaultsDeep } from 'lodash-es'
import type { RecursiveRequired, Settings } from '#/global'
import settingsDefault from '@/settings.default'

const globalSettings: Settings.all = {
  // 请在此处编写或粘贴配置代码
  app: {
    colorScheme: 'light',
    enablePermission: true,
    enableDynamicTitle: true,
  },
  menu: {
    menuMode: 'single',
    enableSubMenuCollapseButton: true,
  },
  toolbar: {
    pageReload: true,
    colorScheme: true,
  },
  topbar: {
    mode: 'fixed',
  },
}

export default defaultsDeep(globalSettings, settingsDefault) as RecursiveRequired<Settings.all>
