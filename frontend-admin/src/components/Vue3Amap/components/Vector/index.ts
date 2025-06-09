import type { App } from 'vue'
import { withInstall } from '../../utils'
import AMapVector from './index.vue'

AMapVector.install = function install(app: App) {
  app.component(AMapVector.name, AMapVector)
}

AMapVector.plugins = [
  'AMap.MouseTool',
  'AMap.PolygonEditor',
  'AMap.BezierCurveEditor',
  'AMap.RectangleEditor',
  'AMap.CircleEditor',
]

export default withInstall(AMapVector, [
  'AMap.MouseTool',
  'AMap.PolygonEditor',
  'AMap.BezierCurveEditor',
  'AMap.RectangleEditor',
  'AMap.CircleEditor',
])
