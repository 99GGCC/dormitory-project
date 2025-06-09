import type { RouteRecordRaw } from 'vue-router'

function Layout() {
  return import('@/layouts/index.vue')
}

const routes: RouteRecordRaw = {
  path: '/building',
  component: Layout,
  redirect: '/building/index',
  name: 'building',
  meta: {
    title: '楼栋管理',
    icon: 'mingcute:building-5-fill',
    auth: ['system:building', 'system:building:index'],
  },
  children: [
    {
      path: 'info',
      name: 'building-info-page',
      component: () => import('@/views/building/info/page.vue'),
      meta: {
        title: '楼栋信息',
        auth: ['system:building', 'system:building:info'],
      },
    },
    {
      path: 'view',
      name: 'building-view-page',
      component: () => import('@/views/building/building-view/page.vue'),
      meta: {
        title: '楼栋视图',
        auth: ['system:building', 'system:building:view'],
      },
    },
    {
      path: 'record',
      name: 'building-record-page',
      component: () => import('@/views/building/record/page.vue'),
      meta: {
        title: '动迁记录',
        auth: ['system:building', 'system:building:record'],
      },
    },
  ],
}

export default routes
