import axios from 'axios'

import qs from 'qs'
import Message from 'vue-m-message'
import nProgress from 'nprogress'
import useUserStore from '@/store/modules/user'
import { tansParams } from '@/utils/dev'

const api = axios.create({
  baseURL: (import.meta.env.DEV && import.meta.env.VITE_OPEN_PROXY === 'true') ? '/proxy/' : import.meta.env.VITE_APP_API_BASEURL,
  timeout: 1000 * 60,
  responseType: 'json',

})

api.interceptors.request.use(
  (request) => {
    // 全局拦截请求发送前提交的参数
    const userStore = useUserStore()
    // 设置请求头
    if (request.headers) {
      if (userStore.isLogin) {
        request.headers.saToken = userStore.token
      }
    }
    // 是否将 POST 请求参数进行字符串化处理
    if (request.method === 'post') {
      // request.data = qs.stringify(request.data, {
      //   arrayFormat: 'brackets',
      // })
    }
    nProgress.start()
    return request
  },
)

api.interceptors.response.use(
  (response) => {
    nProgress.done()
    const userStore = useUserStore()
    /**
     * 全局拦截请求发送后返回的数据，如果数据有报错则在这做全局的错误提示
     * 假设返回数据格式为：{ status: 1, error: '', data: '' }
     * 规则是当 status 为 1 时表示请求成功，为 0 时表示接口需要登录或者登录状态失效，需要重新登录
     * 请求出错时 error 会返回错误信息
     */
    if (response.status === 200) {
      if (response.data.state !== true) {
        if (response.data.code === -1) {
          userStore.logout()
        }
        // 错误提示
        Message.error(response.data.message, {
          zIndex: 2000,
        })
        return Promise.reject(response.data)
      }
    }
    else {
      userStore.logout()
    }
    return Promise.resolve(response.data)
  },
  (error) => {
    nProgress.done()
    let message = error.message
    if (message === 'Network Error') {
      message = '后端网络故障'
    }
    else if (message.includes('timeout')) {
      message = '接口请求超时'
    }
    else if (message.includes('Request failed with status code')) {
      message = `接口${message.substr(message.length - 3)}异常`
    }
    Message.error(message, {
      zIndex: 2000,
    })
    return Promise.reject(error)
  },
)

export default api
