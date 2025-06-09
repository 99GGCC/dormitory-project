import api from '../index'

export default {
  // 登录
  login: (data: {
    'code'?: string
    'adminPhone': string
    'adminPass': string
  }) => api.post('/admin/login', data, {}),

  /** 获取权限 */
  permission: () => api.get('/admin/role/menu/user'),

  /** 新增用户信息 */
  userAdd: (data: any) => {
    return api.post('/admin/add', data, {})
  },

  /**
   * 删除用户信息
   */
  userDelete: (userId: number, data: any) => {
    return api.delete(`/admin/del/${userId}`, data)
  },

  /**
   * 用户信息详情
   */
  userInfo: (userId: number, data: any) => {
    return api.post(`/admin/detail/${userId}`, data, {})
  },

  /**
   * 编辑用户信息
   */
  userEdit: (userId: number, data: any) => {
    return api.post(`/admin/edit/${userId}`, data, {})
  },

  /**
   * 查询个人信息
   * @param params
   */
  userMine: (params: any) => {
    return api.get('/admin/mine', { params })
  },

  /**
   * 用户分页查询
   */
  userPage: (params: any) => {
    return api.post('/admin/page', params, {})
  },

  /**
   * 修改个人信息
   * @param data
   */
  userMineEdit: (data: any) => {
    return api.post('/user/edit/mine', data, {})
  },

  /**
   * 用户注册
   */
  userRegister: (data: any) => {
    return api.post('/user/register', data, {})
  },

}
