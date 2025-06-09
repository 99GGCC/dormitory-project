import type { RouteRecordRaw } from 'vue-router'

function Layout() {
  return import('@/layouts/index.vue')
}

const routes: RouteRecordRaw = {
  path: '/attendance',
  component: Layout,
  redirect: '/attendance/user',
  name: 'attendance',
  meta: {
    title: '考勤管理',
    icon: 'material-symbols:record-voice-over',
    auth: ['system:attendance', 'system:attendance:index'],
  },
  children: [
    {
      path: 'user',
      name: 'attendance-user-page',
      component: () => import('@/views/attendance/info/page.vue'),
      meta: {
        title: '学生考勤',
        auth: ['system:attendance', 'system:attendance:user'],
      },
    },
    {
      path: 'record',
      name: 'attendance-record-page',
      component: () => import('@/views/attendance/record/page.vue'),
      meta: {
        title: '考勤记录',
        auth: ['system:attendance', 'system:attendance:record'],
      },
    },
  ],
}

export default routes
