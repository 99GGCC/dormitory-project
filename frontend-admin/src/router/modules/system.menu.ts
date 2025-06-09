import type { RouteRecordRaw } from 'vue-router'

function Layout() {
  return import('@/layouts/index.vue')
}

const routes: RouteRecordRaw = {
  path: '/system',
  component: Layout,
  redirect: '/system/user',
  name: 'system',
  meta: {
    title: '系统管理',
    icon: 'tabler:template',
    auth: ['system:system'],
  },
  children: [
    {
      path: 'user',
      name: 'system-user-page',
      component: () => import('@/views/system/user/page.vue'),
      meta: {
        title: '用户管理',
        auth: ['system:user'],
      },
    },
    {
      path: 'role',
      name: 'system-role-page',
      component: () => import('@/views/system/role/page.vue'),
      meta: {
        title: '角色管理',
        auth: ['system:role'],
      },
    },
    {
      path: 'notice',
      name: 'system-notice-page',
      component: () => import('@/views/system/notice/page.vue'),
      meta: {
        title: '公告管理',
        auth: ['system:notice'],
      },
    },
  ],
}

export default routes
