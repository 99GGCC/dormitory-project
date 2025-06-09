<script lang="ts">
import {
  defineComponent, onBeforeUnmount, onMounted,
} from 'vue'
import type { PropType } from 'vue'
import { uuid } from '../../utils'
import { handleError } from '../../core/error'
import { useInjectMap } from '../../components/Map/composable'

export default defineComponent({
  name: 'AMapSearch',
  props: {
    position: {
      type: Object as PropType<AMap.ControlConfig['position']>,
      default: () => ({ top: '40px', left: '90px' }),
    },
  },
  setup(props) {
    const id = `a-map__search${uuid()}`
    const { AMap, map } = useInjectMap()
    let autoComplete: any
    let placeSearch: any

    onMounted(() => {
      autoComplete = new AMap.Autocomplete({ input: id })
      placeSearch = new AMap.PlaceSearch({ map })

      autoComplete.on('select', (e: any) => {
        placeSearch.setCity(e.poi.adcode)
        placeSearch.search(e.poi.name)
      })
      // @ts-expect-error
      autoComplete.on('error', ({ info }) => handleError({ info, target: '输入提示' }))
      // @ts-expect-error
      placeSearch.on('error', ({ info }) => handleError({ info, target: '搜索服务' }))
    })

    onBeforeUnmount(() => {
      map.remove(autoComplete)
      map.remove(placeSearch)
    })

    return { props, id }
  },
})
</script>

<template>
  <div class="a-map__search" :style="props.position">
    <input
      :id="id"
      placeholder="输入地名进行搜索"
    >
  </div>
</template>

<style lang="scss">
.a-map__search {
  position: absolute;
  z-index: 1;
}
</style>
~/utils~/core/error
