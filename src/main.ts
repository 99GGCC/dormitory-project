import FloatingVue from 'floating-vue'
import 'floating-vue/dist/style.css'

import Message from 'vue-m-message'
import 'vue-m-message/dist/style.css'

import 'overlayscrollbars/overlayscrollbars.css'

import App from './App.vue'
import pinia from './store'
import router from './router'
import ui from './ui-provider'
import useVxeTableComponents from './utils/vxe-tavle-resolve'

// vue-map
import Vue3AMap from './components/Vue3Amap'
import { errorMsg } from './utils/message'

import FormModal from '@/components/FormModal/index.vue'
import ImageUpload from '@/components/ImageUpload/index.vue'

// 自定义指令
import directive from '@/utils/directive'

// 加载 svg 图标
import 'virtual:svg-icons-register'

// 加载 iconify 图标
import { downloadAndInstall } from '@/iconify'
import icons from '@/iconify/index.json'

import 'virtual:uno.css'

// 全局样式
import '@/assets/styles/globals.scss'

const app = createApp(App)
app.use(FloatingVue, {
  distance: 12,
})
app.use(Message)
app.use(pinia)
app.use(router)
app.use(ui)
app.component('FormModal', FormModal)
app.component('ImageUpload', ImageUpload)
// eslint-disable-next-line ts/ban-ts-comment
// @ts-expect-error
window._AMapSecurityConfig = {
  serviceHost: `${window.location.protocol}//${window.location.host}/_AMapService`,
}
app.use(Vue3AMap, {
  key: import.meta.env.VITE_AMAP_JS_KEY,
  version: '2.0',
  errorHandler(error: any) {
    errorMsg(`高德地图: ${error.message}`)
  },
})
useVxeTableComponents(app)
directive(app)
if (icons.isOfflineUse) {
  for (const info of icons.collections) {
    downloadAndInstall(info)
  }
}

app.mount('#app')
