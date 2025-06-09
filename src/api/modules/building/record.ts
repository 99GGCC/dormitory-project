import api from '@/api/index'

const URI = '/admin/relocation/record'
export default {

  /** 分页查询 */
  page: (params: any) => {
    return api.get(`${URI}/page`, { params })
  },

}
