import api from '@/api/index'

const URI = '/common'
export default {

  /** 获取图片验证码 */
  getImageCode: () => {
    return api.get(`${URI}/getImageCode`)
  },

  /** 获取keyId */
  getKeyId: (number: number) => {
    return api.get(`${URI}/getKeyId/${number}`)
  },

  /** 图片删除 */
  delImages: (url: string) => {
    const data = new FormData()
    data.append('url', url)
    return api.post(`${URI}/images/delete`, data)
  },

  /** 图片文件批量上传 */
  batchUploadImage: (params: { url: string }) => {
    return api.post(`${URI}/upload/batch/image`, params)
  },

  /** 图片文件上传 */
  uploadImage: (params: { url: string }) => {
    return api.post(`${URI}/upload/image`, params)
  },

}
