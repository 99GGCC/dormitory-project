import api from '../index'
import type { PageParams } from '@/types/global'

export default {
  /** 楼栋信息列表查询 */
  buildingList: (params: PageParams<any>) => api.get<any[]>('/student/building/info/list', { params }),
  /** 宿舍信息列表查询 */
  dormitoryList: (params: PageParams<any>) => api.get<any[]>('/student/dormitory/info/list', { params }),
  /** 床位信息列表查询 */
  bedList: (params: PageParams<any>) => api.get<any[]>('/student/bed/info/list', { params }),
}
