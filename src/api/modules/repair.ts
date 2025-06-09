import api from '@/api/index'

const URI = '/admin/repair/apply'
export default {

  /** 详情 */
  info: (id: number) => {
    return api.get(`${URI}/detail/${id}`, {})
  },

  /** 分页查询 */
  page: (params: any) => {
    return api.get(`${URI}/page`, { params })
  },

  /** 编辑 */
  edit: (id: number, data: any) => {
    return api.post(`${URI}/status/${id}`, null, {
      params: data,
    })
  },

}
