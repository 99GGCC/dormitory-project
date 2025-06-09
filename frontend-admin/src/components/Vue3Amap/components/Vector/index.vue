<!-- eslint-disable ts/ban-ts-comment -->
<script setup lang="ts">
import {
  Comment, computed, watch,
} from 'vue'
import type { PropType } from 'vue'
import use from './composable'

defineOptions({
  name: 'AMapVector',
})

const props = defineProps({
  config: {
    type: Object as PropType<{
      type: string // 矢量图形类型
      shape: any // 矢量图形构造函数选项
      [key: string]: any
    }>,
    default: () => ({}),
  },
})

const {
  drawerRef, vectorRef, editorRef,
  factory,
  start,
  stop,
  clear,
  mountVector,
} = use(props.config.type)
const reaOnlyRef = computed(() => !(drawerRef.value || editorRef.value))

watch(
  () => props.config.shape,
  (shape) => {
    if (shape) {
      mountVector(props.config.shape)
    }
  },
  { immediate: true },
)

function generateConfig() {
  return {
  // @ts-expect-error
    ...factory.serializeVector(vectorRef.value),
    ...props.config,
  }
}

defineExpose({
  start, stop, clear, vectorRef, reaOnlyRef, generateConfig,
})
</script>

<template>
  <Comment />
</template>
