import api from '../index'

export default {
  /** 维修 */
  repair: (data: any) => {
    return api.post<any>('/student/repair/apply/initiate', data)
  },
  /** 调换 */
  change: (data: any) => {
    return api.post<any>('/student/change/apply/initiate', data)
  },
  /** 取消维修 */
  cancelRepair: (repairId: string | number) => {
    return api.post<any>(`/student/repair/apply/cancel/${repairId}`)
  },
  /** 取消调换 */
  cancelChange: (changeId: string | number) => {
    return api.post<any>(`/student/change/apply/cancel/${changeId}`)
  },

}
