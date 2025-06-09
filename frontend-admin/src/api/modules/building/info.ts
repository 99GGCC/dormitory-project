import api from '@/api/index'

const URI = '/admin/building/info'
export default {
  /** 新增 */
  add: (data: any) => {
    return api.post(`${URI}/add`, data, {
      timeout: 1000000,
    })
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

  /** 列表查询 */
  list: (params?: {
    /** 楼层管理员 */
    buildingAdmin?: string
    /** 楼栋名称 */
    buildingName?: string
    /** 楼栋类型1、男生宿舍 0、女生宿舍 */
    buildingType?: number
  }) => {
    return api.get(`${URI}/list`, { params } as any)
  },

}
