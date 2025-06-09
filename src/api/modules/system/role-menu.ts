import api from '@/api/index'

const URI = '/admin/role/menu'
export default {
  /** 角色授权菜单 */
  empower: (data: any) => {
    return api.post(`${URI}/empower`, data, {})
  },

  /** 根据角色ID获取菜单列表 */
  menuTreeById: (id: number) => {
    return api.get(`${URI}/tree/${id}`, {})
  },

  /** 查询登录用户菜单树 */
  mineRoleMenuTree: () => {
    return api.get(`${URI}/user`, {})
  },

  /** 查询登录用户列表 */
  mineMenuUserList: () => {
    return api.get(`${URI}/user/list`, {})
  },

}
