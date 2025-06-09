import type { RouteRecordRaw } from 'vue-router'

function Layout() {
  return import('@/layouts/index.vue')
}

const moduleName = 'repair'
const moduleTitle = '维修管理'

const routes: RouteRecordRaw = {
  path: `/${moduleName}`,
  component: Layout,
  redirect: `/${moduleName}/index`,
  name: `${moduleName}`,
  meta: {
    title: moduleTitle,
    icon: 'wpf:maintenance',
    auth: ['system:repair', 'system:repair:index'],
  },
  children: [
    {
      path: 'index',
      name: `${moduleName}-page`,
      component: () => import(`@/views/repair/page.vue`),
      meta: {
        title: moduleTitle,
        // 是否在菜单中展示
        sidebar: false,
        // 指定高亮的菜单导航，需要设置完整路由地址 (我不高亮谁高亮)
        activeMenu: `/${moduleName}`,
        // 该路由是否在面包屑导航中展示
        breadcrumb: true,
      },
    },
  ],
}

export default routes
