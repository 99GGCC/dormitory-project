import api from '@/api/index'

const URI = '/admin/classes/info'
export default {
  /** 新增 */
  add: (data: any) => {
    return api.post(`${URI}/add`, data, {})
  },

  /** 删除 */
  delete: (id: number) => {
    return api.post(`${URI}/del/${id}`)
  },

  /** 编辑 */
  edit: (id: number, data: any) => {
    return api.post(`${URI}/edit/${id}`, data, {})
  },

  /** 详情 */
  info: (id: number) => {
    return api.get(`${URI}/detail/${id}`, {})
  },

  /** 分页查询 */
  page: (params: any) => {
    return api.get(`${URI}/page`, { params })
  },

  /** 设置状态 */
  updateStatus: (id: number, data: any) => {
    return api.post(`${URI}/status/${id}`, data, {})
  },

}
