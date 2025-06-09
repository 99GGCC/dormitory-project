import api from '@/api/index'

const URI = '/admin/param'
export default {
  /** 新增 */
  add: (data: any) => {
    return api.post(`${URI}/add`, data, {})
  },

  /** 删除 */
  delete: (id: number) => {
    return api.delete(`${URI}/del/${id}`)
  },

  /** 编辑 */
  edit: (id: number, data: any) => {
    return api.post(`${URI}/edit/${id}`, data, {})
  },

  /** 分页查询 */
  page: (params: any) => {
    return api.get(`${URI}/page`, { params })
  },

}
