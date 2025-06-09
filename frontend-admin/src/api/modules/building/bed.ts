import api from '@/api/index'

const URI = '/admin/bed/info'
export default {
  /** 新增 */
  add: (data: {
    'bedName': any
    'bedStatus': any
    'dormitoryId': any
  }) => {
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

  /** 安排床位 */
  arrange: (data: {
    'bedId': number
    'dormitoryId': number
    'isHead': number
    'useStudent': number
  }) => {
    return api.post(`${URI}/arrange`, data, {})
  },

  /** 释放床位 */
  release: (data: {
    'bedId': number
    'relocationType': number
  }) => {
    return api.post(`${URI}/release`, data, {})
  },

  /** 床位列表 */
  list: (params: any) => {
    return api.get(`${URI}/list`, { params })
  },

}
