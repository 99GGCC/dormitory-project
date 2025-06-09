import api from '@/api/index'

const URI = '/common/dict'
export default {

  /** 分页查询 */
  page: (params: any) => {
    return api.get(`${URI}/page`, { params })
  },

  /** 列表查询 */
  list: (params: any) => {
    return api.get(`${URI}/list`, { params })
  },

}
