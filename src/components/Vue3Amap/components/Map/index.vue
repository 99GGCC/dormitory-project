<script setup lang="ts">
import {
  nextTick, onBeforeUnmount, onMounted, ref, shallowReactive, watch,
} from 'vue'
import type { PropType } from 'vue'
import AMapLoader from '@amap/amap-jsapi-loader'
import config from '../../core/config'
import { handleError } from '../../core/error'
import type { AMapProvider } from './composable'
import { useProvideMap } from './composable'

defineOptions({ name: 'AMapMap' })
const props = defineProps({
  mapStyle: {
    type: String as PropType<
      'normal' |
      'macaron' |
      'graffiti' |
      'whitesmoke' |
      'dark' |
      'fresh' |
      'darkblue' |
      'blue' |
      'light' |
      'grey'
    >,
    default: 'normal',
    validator: (v: string) => [
      'normal',
      'macaron',
      'graffiti',
      'whitesmoke',
      'dark',
      'fresh',
      'darkblue',
      'blue',
      'light',
      'grey',
    ].includes(v),
  },
})

const emit = defineEmits(['complete', 'destroy'])
// eslint-disable-next-line ts/ban-ts-comment
// @ts-expect-error
const mapState = shallowReactive<AMapProvider>({ AMap: null, map: null })
const initialized = ref<boolean>(false)
const containerRef = ref<HTMLElement>()

useProvideMap(mapState)

onMounted(async () => {
  try {
    const AMap = await AMapLoader.load(config)
    const map = new AMap.Map(containerRef.value, {
      resizeEnable: true,
      zoom: 12,
      mapStyle: `amap://styles/${props.mapStyle}`,
    })

    map.on('complete', () => {
      initialized.value = true
      emit('complete')
    })

    Object.assign(mapState, { AMap, map })
  }
  catch (info) {
    handleError({ info, target: '地图加载' })
  }
})

watch(
  () => props.mapStyle,
  () => {
    mapState.map?.setMapStyle(`amap://styles/${props.mapStyle}`)
  },
)

onBeforeUnmount(() => {
  // 确保子组件及地图内元素销毁完毕
  nextTick(() => {
    mapState.map?.destroy()
    emit('destroy')
  })
})
</script>

<template>
  <div ref="containerRef" class="a-map-map__wrapper">
    <slot v-if="initialized" />
  </div>
</template>

<style lang="scss">
.a-map-map__wrapper {
  width: max(100%, 100px);
  height: max(100%, 100px);
  overflow: hidden;
}
</style>
