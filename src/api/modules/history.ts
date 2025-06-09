import api from '../index'
import type { PageParams } from '@/types/global'

export default {
  /** 维修 */
  getRepairList(params: PageParams<{ repairStatus?: number }>) {
    return api.get('/student/repair/apply/page', { params })
  },
  /** 调换 */
  getExchangeList(params: PageParams<{ applyStatus?: number }>) {
    return api.get('/student/change/apply/page', { params })
  },
}
