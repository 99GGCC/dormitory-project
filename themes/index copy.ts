import { hex2rgba } from '@unocss/preset-mini/utils'

const lightPrimaryColor = '#FF69B4'
// const secondaryPrimaryColor = '#FFC0CB'
const darkPrimaryColor = '#FF69B4'

export const lightTheme = {
  'color-scheme': 'light',
  // 内置 UI
  '--ui-primary': hex2rgba(lightPrimaryColor)!.join(' '),
  '--ui-text': hex2rgba('#fcfcfc')!.join(' '),
  // 主体
  '--g-bg': '#f2f2f2',
  '--g-container-bg': '#fff',
  '--g-border-color': '#f2f2f2',
  // 头部
  '--g-header-bg': '#fff',
  '--g-header-color': lightPrimaryColor,
  '--g-header-menu-color': lightPrimaryColor,
  '--g-header-menu-hover-bg': '#dde1e3',
  '--g-header-menu-hover-color': lightPrimaryColor,
  '--g-header-menu-active-bg': lightPrimaryColor,
  '--g-header-menu-active-color': '#fff',
  // 主导航
  '--g-main-sidebar-bg': '#f2f2f2',
  '--g-main-sidebar-menu-color': lightPrimaryColor,
  '--g-main-sidebar-menu-hover-bg': '#dde1e3',
  '--g-main-sidebar-menu-hover-color': lightPrimaryColor,
  '--g-main-sidebar-menu-active-bg': lightPrimaryColor,
  '--g-main-sidebar-menu-active-color': '#fff',
  // 次导航
  '--g-sub-sidebar-bg': '#fff',
  '--g-sub-sidebar-logo-bg': lightPrimaryColor,
  '--g-sub-sidebar-logo-color': '#fff',
  '--g-sub-sidebar-menu-color': lightPrimaryColor,
  '--g-sub-sidebar-menu-hover-bg': '#dde1e3',
  '--g-sub-sidebar-menu-hover-color': lightPrimaryColor,
  '--g-sub-sidebar-menu-active-bg': lightPrimaryColor,
  '--g-sub-sidebar-menu-active-color': '#fff',
  // 标签栏
  '--g-tabbar-dividers-bg': '#a3a3a3',
  '--g-tabbar-tab-color': '#a3a3a3',
  '--g-tabbar-tab-hover-bg': '#e5e5e5',
  '--g-tabbar-tab-hover-color': lightPrimaryColor,
  '--g-tabbar-tab-active-color': lightPrimaryColor,
}

export const darkTheme = {
  'color-scheme': 'dark',
  // 内置 UI
  '--ui-primary': hex2rgba(darkPrimaryColor)!.join(' '),
  '--ui-text': hex2rgba('#0f0f0f')!.join(' '),
  // 主体
  '--g-bg': '#0a0a0a',
  '--g-container-bg': '#141414',
  '--g-border-color': '#15191e',
  // 头部
  '--g-header-bg': '#141414',
  '--g-header-color': darkPrimaryColor,
  '--g-header-menu-color': '#a8a29e',
  '--g-header-menu-hover-bg': '#141414',
  '--g-header-menu-hover-color': darkPrimaryColor,
  '--g-header-menu-active-bg': darkPrimaryColor,
  '--g-header-menu-active-color': '#0a0a0a',
  // 主导航
  '--g-main-sidebar-bg': '#0a0a0a',
  '--g-main-sidebar-menu-color': '#a8a29e',
  '--g-main-sidebar-menu-hover-bg': '#141414',
  '--g-main-sidebar-menu-hover-color': darkPrimaryColor,
  '--g-main-sidebar-menu-active-bg': darkPrimaryColor,
  '--g-main-sidebar-menu-active-color': '#0a0a0a',
  // 次导航
  '--g-sub-sidebar-bg': '#141414',
  '--g-sub-sidebar-logo-bg': '#0f0f0f',
  '--g-sub-sidebar-logo-color': darkPrimaryColor,
  '--g-sub-sidebar-menu-color': '#a8a29e',
  '--g-sub-sidebar-menu-hover-bg': '#0a0a0a',
  '--g-sub-sidebar-menu-hover-color': darkPrimaryColor,
  '--g-sub-sidebar-menu-active-bg': darkPrimaryColor,
  '--g-sub-sidebar-menu-active-color': '#0a0a0a',
  // 标签栏
  '--g-tabbar-dividers-bg': '#a8a29e',
  '--g-tabbar-tab-color': '#a8a29e',
  '--g-tabbar-tab-hover-bg': '#1b1b1b',
  '--g-tabbar-tab-hover-color': darkPrimaryColor,
  '--g-tabbar-tab-active-color': darkPrimaryColor,
}
