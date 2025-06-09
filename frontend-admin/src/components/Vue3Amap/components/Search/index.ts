import { withInstall } from '../../utils'
import AMapSearch from './index.vue'

export default withInstall(AMapSearch, [
  'AMap.AutoComplete',
  'AMap.PlaceSearch',
])
