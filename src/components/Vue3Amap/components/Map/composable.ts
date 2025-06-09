import { inject, provide } from 'vue'
import { uuid } from '../../utils'

export interface AMapProvider {
  AMap: typeof AMap
  map: AMap.Map
}

interface AMapInjection extends AMapProvider {}

const key = Symbol(uuid())

export const useProvideMap = (val: AMapProvider) => provide(key, val)
// eslint-disable-next-line ts/ban-ts-comment
// @ts-expect-error
export const useInjectMap = (): AMapInjection => inject(key)
