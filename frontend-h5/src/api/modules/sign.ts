import api from '../index'
import type { PageParams } from '@/types/global'

export default {
  /** 考勤记录详情 */
  getSignDetail(recordId: number) {
    return api.get(`/student/sign/in/record/detail/${recordId}`)
  },
  /** 考勤记录分页查询 */
  getSignList(data: PageParams<{ recordStatus?: number }>) {
    return api.post('/student/sign/in/record/page', data)
  },
  /** 学生签到 */
  studentSign(recordId: number) {
    return api.post(`/student/sign/in/record/sign/${recordId}`)
  },
}
