<!-- eslint-disable ts/ban-ts-comment -->
<script lang="ts">
import {
  Comment, computed, defineComponent, h, onBeforeUnmount, watch,
} from 'vue'
import type { ComputedRef, PropType } from 'vue'
import { useInjectMap } from '../../components/Map/composable'

export default defineComponent({
  name: 'AMapMarker',
  props: {
    position: {
      type: Array as unknown as PropType<AMap.Vector2>,
      default: null,
    },
    title: {
      type: String,
      default: '',
    },
    zIndex: {
      type: Number,
      default: 100,
    },
    autoFitView: {
      type: Boolean,
      default: false,
    },
  },

  setup(props) {
    const { AMap, map } = useInjectMap()

    const position: ComputedRef<AMap.LngLat> = computed(() => {
      const { lng, lat } = map.getCenter()
      return new AMap.LngLat(...(
        props.position?.length ? props.position : [lng, lat]
      ) as AMap.Vector2)
    })
    const optionsRef = computed(() => {
      // eslint-disable-next-line unused-imports/no-unused-vars
      const { autoFitView, ...rest } = props
      return {
      // @ts-expect-error
        position: position.value,
        ...rest,
      }
    })

    const control = new AMap.Marker({
      map,
      ...optionsRef.value,
    })

    map.add(control)
    map.setFitView()

    watch(
      () => props.position,
      () => {
        control.setPosition(optionsRef.value.position)
        // eslint-disable-next-line style/max-statements-per-line
        if (props.autoFitView) { map.setFitView() }
      },
      { deep: true },
    )

    watch(
      () => props.title,
      () => control.setTitle(optionsRef.value.title),
    )

    watch(
      () => props.zIndex,
      () => control.setzIndex(optionsRef.value.zIndex),
    )

    onBeforeUnmount(() => {
      map.remove(control)
    })

    return () => h(Comment)
  },
})
</script>
