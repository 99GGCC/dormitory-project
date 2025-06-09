<script setup lang="ts">
import { serverAssetsURI } from '@/utils/dev'

const props = defineProps({
  src: {
    type: String,
    default: '',
  },
})
const url = computed(() => {
  return serverAssetsURI(props.src)
})
</script>

<template>
  <span>
    <img :src="url" v-bind="$attrs">
  </span>
</template>
