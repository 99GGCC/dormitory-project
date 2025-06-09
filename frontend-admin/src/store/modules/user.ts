import useRouteStore from './route'
import useMenuStore from './menu'
import useDictStore from './dict'
import router from '@/router'
import apiUser from '@/api/modules/user'
import { findNodesWithCondition } from '@/utils/dev'

const useUserStore = defineStore(
  // 唯一ID
  'user',
  () => {
    const routeStore = useRouteStore()
    const menuStore = useMenuStore()
    const dictStore = useDictStore()

    const account = ref(localStorage.account ?? '')
    const token = ref(localStorage.token ?? '')
    const avatar = ref(localStorage.avatar ?? '')
    const permissions = ref<string[]>([])
    const isAdmin = computed(() => {
      if (permissions.value.includes('admin')) {
        return true
      }
      return false
    })
    const isLogin = computed(() => {
      if (token.value) {
        return true
      }
      return false
    })

    // 登录
    async function login(data: {
      'code': string
      'adminPhone': string
      'adminPass': string
    }) {
      const res = await apiUser.login(data)
      localStorage.setItem('account', res.data.adminName)
      localStorage.setItem('token', res.data.saToken)
      localStorage.setItem('avatar', res.data.userAvatar ?? '')
      account.value = res.data.adminName
      token.value = res.data.saToken
      avatar.value = res.data.userAvatar ?? ''
      permissions.value = [res.data.roleFlag]
      // 获取字典
      dictStore.init()
    }
    // 登出
    async function logout() {
      localStorage.removeItem('account')
      localStorage.removeItem('token')
      localStorage.removeItem('avatar')
      account.value = ''
      token.value = ''
      avatar.value = ''
      permissions.value = []
      routeStore.removeRoutes()
      menuStore.setActived(0)
      dictStore.clear()
      router.push({
        name: 'login',
      })
    }
    // 获取权限
    async function getPermissions() {
      const res = await apiUser.permission()
      permissions.value = findNodesWithCondition(
        res.data,
        (node) => {
          return node.roleMenuId !== null
        },
        (node) => {
          return node.menuFlag
        },
        'children',
      )
      return permissions.value
    }
    // 修改密码
    // async function editPassword(data: {
    //   password: string
    //   newpassword: string
    // }) {
    //   await apiUser.passwordEdit(data)
    // }

    return {
      account,
      token,
      avatar,
      permissions,
      isAdmin,
      isLogin,
      login,
      logout,
      getPermissions,
      // editPassword,
    }
  },
)

export default useUserStore
