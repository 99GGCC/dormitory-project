<script lang="ts">
import {
  Comment, computed, defineComponent, h, onBeforeUnmount,
} from 'vue'
import type { PropType } from 'vue'
import { immediateInterval } from '../../utils'
import { useInjectMap } from '../../components/Map/composable'

export default defineComponent({
  name: 'AMapGeolocation',
  props: {
    watchPosition: {
      type: Boolean,
      default: false,
    },
    showButton: {
      type: Boolean,
      default: true,
    },
    showCircle: {
      type: Boolean,
      default: true,
    },
    showMarker: {
      type: Boolean,
      default: true,
    },
    position: {
      type: Object as PropType<AMap.ControlConfig['position']>,
      default: () => ({ bottom: '90px', right: '40px' }),
    },
    panToLocation: {
      type: Boolean,
      default: true,
    },
  },
  emits: ['update:position'],
  setup(props, { emit }) {
    const { AMap, map } = useInjectMap()
    const optionsRef = computed((): AMap.GeolocationOptions => {
      // eslint-disable-next-line unused-imports/no-unused-vars
      const { watchPosition, ...rest } = props
      return { ...rest }
    })

    const geolocation = new AMap.Geolocation(optionsRef.value)
    map.addControl(geolocation)

    let timer: number | null | undefined

    const getCurrentPosition = () => new Promise((resolve, reject) => {
      geolocation.getCurrentPosition((status, result) => {
        if (status === 'complete') {
          resolve([
            result.position.getLng(),
            result.position.getLat(),
          ])
        }
        else {
          reject(result)
        }
      })
    })

    const unWatch = () => {
      if (timer) {
        clearInterval(timer)
        timer = null
      }
    }

    const watch = () => {
      unWatch()
      timer = immediateInterval(() => {
        getCurrentPosition().then((pos) => {
          emit('update:position', pos)
        })
      }, 1000 * 3)
    }

    if (props.watchPosition) {
      watch()
    }

    onBeforeUnmount(() => {
      map.remove(geolocation)
      unWatch()
    })

    return () => h(Comment)
  },
})
</script>
~/utils
