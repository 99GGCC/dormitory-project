<script setup lang="ts">
import { type PropType, ref } from 'vue'
import type { ComponentExposed } from 'vue-component-type-helpers'
import { AMapVector } from '@/components/Vue3Amap/index'
import { errorMsg } from '@/utils/message'

export interface Fence {
  /** 围栏业务 id */
  gfid?: number
  /** 围栏名称 */
  name?: string
  /** 围栏描述 */
  desc?: string
  /** 围栏地图值 */
  shape: any
  /** 围栏类型 */
  type: 'circle' | 'polygon'
  /** 围栏创建时间 */
  createtime?: number
  /** 围栏更新时间 */
  modifytime?: number
}

defineOptions({
  name: 'FenceMap',
})

const props = defineProps({
  type: {
    type: String as PropType<'circle' | 'polygon'>,
    default: 'polygon',
  },
  disabled: {
    type: Boolean,
    default: false,
  },
})

const modelValue = defineModel<string>({
  default: '',
})

const hasSaved = ref(false)
const config = reactive<Fence>({
  type: props.type,
  shape: {
    points: modelValue.value,
    type: props.type,
  },
})

watchEffect(() => {
  if (modelValue.value) {
    config.shape = { points: modelValue.value, type: props.type }
  }
  else {
    config.shape = null
  }
})

const childRef = ref<ComponentExposed<typeof AMapVector> | null>(null)
function setChildRef(v: any) {
  childRef.value = v
}
function handleSubmit() {
  if (!childRef.value) {
    return
  }
  if (!childRef.value.vectorRef) {
    return errorMsg('请绘制围栏')
  }
  const payload = childRef.value.generateConfig()
  hasSaved.value = true
  modelValue.value = payload.points || ''
}

function clear() {
  if (!childRef.value) {
    return
  }
  childRef.value.clear()
}

defineExpose({
  clear,
})
</script>

<template>
  <div>
    <div v-if="childRef && !disabled" class="mb-3 flex flex-row gap-2">
      <vxe-button @click="childRef.start">
        {{ childRef.vectorRef ? "继续" : "开始" }}绘制
      </vxe-button>

      <vxe-button :disabled="!childRef.vectorRef" @click="childRef.stop">
        结束绘制
      </vxe-button>
      <vxe-button :disabled="!childRef.vectorRef" @click="clear">
        清空绘制
      </vxe-button>

      <vxe-button type="primary" content="保存" @click="handleSubmit" />
    </div>
    <div class="h-[500px] w-full">
      <a-map-map>
        <a-map-fit-view />
        <!-- JS SKD 调用，日限额较低 -->
        <!-- <a-map-search /> -->

        <!-- Web API 调用，日限额较高 -->
        <!-- <auto-complete /> -->
        <AMapVector :ref="setChildRef" :config="config" />
        <a-map-toolbar />
        <a-map-scale />
        <a-map-control-bar />
        <!-- <a-map-map-type /> -->
      </a-map-map>
    </div>
  </div>
</template>
