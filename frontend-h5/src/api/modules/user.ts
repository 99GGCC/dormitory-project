import api from '../index'

export default {
  /** 修改密码 */
  editPass: (data: {
    newPass: string
    oldPass: string
  }) => {
    return api.post('/student/change/password', data)
  },

  /** 修改个人信息 */
  edit: (data: {
    studentEmail: string
    studentPhone: string
  }) => {
    return api.post('/student/edit', data)
  },

  /** 登录 */
  login: (data: {
    code: string
    studentNum: string
    studentPass: string
  }) => api.post('/student/login', data),

  /** 个人信息 */
  info: () => api.get('/student/mine'),

  /** 根据学生ID获取登录信息 */
  getLoginInfo: (studentId: string) => api.post(`/student/token/${studentId}`),
}
