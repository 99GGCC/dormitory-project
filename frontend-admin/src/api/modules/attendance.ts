import api from '@/api/index'

const URI = '/admin/sign/in'
export default {
  /** 考勤信息发布 */
  add: (data: any) => {
    return api.post(`${URI}/issue/add`, data, {})
  },
  /** 考勤信息详情 */
  info: (id: number) => {
    return api.get(`${URI}/issue/detail/${id}`, { })
  },
  /** 考勤信息分页查询 */
  page: (data: any) => {
    return api.post(`${URI}/issue/page`, data)
  },
  /** 考勤信息作废 */
  voided: (id: number) => {
    return api.post(`${URI}/issue/status/${id}`)
  },
  /** 考勤记录分页查询 */
  recordPage: (data: any) => {
    return api.post(`${URI}/record/page`, data)
  },
  /** 给学生签到 */
  sign: (id: number) => {
    return api.post(`${URI}/record/sign/${id}`, null, {})
  },

}
