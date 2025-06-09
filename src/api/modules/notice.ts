import api from '../index'
import type { IdType, PageParams } from '@/types/global'

export default {
  /** 详情 */
  detail: (id: IdType) => api.get<any>(`/student/notice/info/detail/${id}`),
  /** 列表 */
  list: (params: PageParams<{
    noticeContent?: string
    noticeTitle?: string
  }>) => api.get<any[]>('/student/notice/info/list', { params }),
  /** 分页 */
  page: (params: PageParams<{
    noticeContent?: string
    noticeTitle?: string
  }>) => api.get<any[]>('/student/notice/info/page', { params }),
}
