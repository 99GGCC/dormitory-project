import api from '@/api/index'

const URI = '/admin/dormitory/info'
export default {

  /** 详情 */
  info: (id: number) => {
    return api.get(`${URI}/detail/${id}`, {})
  },

  /** 分页查询 */
  page: (params: any) => {
    return api.get(`${URI}/page`, { params })
  },

  /** 列表查询 */
  list: (params: any) => {
    return api.get(`${URI}/list`, { params })
  },

  /** 楼层列表查询 */
  floorList: (id: number) => {
    return api.get(`${URI}/list/${id}`)
  },

  /** 批量设置床位（适用于不存在床位的宿舍） */
  setBed: (data: {
    /** 床位数量 */
    'bedNum': number
    /** 宿舍IDs */
    'dormitoryIds': number[]
  }) => {
    return api.post(`${URI}/set/bed`, data, {})
  },

  /** 设置宿舍状态 */
  updateStatus: (id: number, data: {
    /** 宿舍ID */
    'dormitoryId': number
    /** 状态 */
    'status': number
  }) => {
    return api.post(`${URI}/status/${id}`, data, {})
  },

}
