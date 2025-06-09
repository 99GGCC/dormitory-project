import api from '../index'

export default {
  /** 获取图片验证码 */
  imgCode: () => api.get('/common/getImageCode'),
}
