import api from '@/api/index'

const URI = '/admin/order'
export default {
  /** 处理 */
  handle: (id: number, data?: any) => {
    return api.post(`${URI}/handle/${id}`, data, {})
  },

  /** 分页查询 */
  page: (data: any) => {
    return api.post(`${URI}/page`, data)
  },

  /** 详情 */
  info: (id: number) => {
    return api.get(`${URI}/detail/${id}`, {})
  },

  /** 查看订单评价 */
  evaluationList: (id: number) => {
    return api.get(`/admin/evaluation/show/${id}`, {})
  },

}
