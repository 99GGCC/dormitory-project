import type { AxiosResponse } from 'axios'
import apiUser from '@/api/modules/user'
import router from '@/router'
import storage from '@/utils/storage'

const useUserStore = defineStore(
  // 唯一ID
  'user',
  () => {
    const account = ref(storage.local.get('account') ?? '')
    const token = ref(storage.local.get('token') ?? '')
    const avatar = ref(storage.local.get('avatar') ?? '')
    const userInfo = ref(storage.local.get('userInfo') ?? {})
    const isGetPermissions = ref(false)
    const permissions = ref<string[]>([])

    if (token.value && userInfo.value) {
      apiUser.info().then((res) => {
        if (res.data) {
          userInfo.value = res.data
        }
      }).catch((error) => {
        console.error('Failed to refresh user info on store init:', error)
      })
    }

    const isLogin = computed(() => {
      if (token.value) {
        return true
      }
      return false
    })

    function setUserInfo(res: AxiosResponse<any, any>) {
      storage.local.set('account', res.data.studentNum)
      storage.local.set('token', res.data.saToken)
      storage.local.set('avatar', res.data.avatar || '')
      storage.local.set('userInfo', res.data)
      account.value = res.data.studentNum
      token.value = res.data.saToken
      avatar.value = res.data.avatar
      userInfo.value = res.data
    }

    function login(data: {
      code: string
      studentNum: string
      studentPass: string
    }) {
      return new Promise((resolve, reject) => {
        apiUser.login(data).then((res) => {
          setUserInfo(res)
          resolve(res)
        }).catch((error) => {
          reject(error)
        })
      })
    }
    function logout() {
      storage.local.remove('account')
      storage.local.remove('token')
      storage.local.remove('avatar')
      storage.local.remove('userInfo')
      account.value = ''
      token.value = ''
      avatar.value = ''
      userInfo.value = {}
      router.push({
        name: 'login',
      })
    }
    // 获取权限
    async function getPermissions() {
      // const res = await apiUser.permission()
      permissions.value = []
      isGetPermissions.value = true
    }
    /**
     *
     * 学生号登录
     */
    async function studentIdLogin(studentId: string) {
      return new Promise((resolve, reject) => {
        apiUser.getLoginInfo(studentId).then((res) => {
          setUserInfo(res)
          resolve(res)
        }).catch((error) => {
          reject(error)
        })
      })
    }

    return {
      account,
      token,
      avatar,
      userInfo,
      isLogin,
      isGetPermissions,
      permissions,
      login,
      logout,
      getPermissions, studentIdLogin,
    }
  },
)

export default useUserStore
