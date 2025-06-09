import api from '@/api/index'

const URI = '/admin/change/apply'
export default {
  /** 详情 */
  info: (id: number) => {
    return api.get(`${URI}/detail/${id}`, { })
  },

  /** 新增 */
  initiate: (data: any) => {
    return api.post(`${URI}/initiate`, data, {})
  },

  /** 分页 */
  page: (params: any) => {
    return api.get(`${URI}/page`, { params })
  },

  /** 编辑 */
  status: (id: number, data: any) => {
    return api.post(`${URI}/status/${id}`, data)
  },

}
