<script lang="ts" setup>
import { useConfig } from './config'
import API from '@/api/modules/building/record'
import type { BtnOptType, ModalType } from '@/types/global'
import { confirmMsg, successMsg, warningMsg } from '@/utils/message'

defineOptions({
  name: 'BuildRecord',
})

const xGrid = ref()

// 业务主键
const { gridOptions } = useConfig(API)
</script>

<template>
  <PageMainFull>
    <vxe-grid ref="xGrid" v-bind="gridOptions">
      <template #toolbar_buttons />
    </vxe-grid>
  </PageMainFull>
</template>
