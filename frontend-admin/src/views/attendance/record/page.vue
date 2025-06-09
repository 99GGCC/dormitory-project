<script lang="ts" setup>
import { useConfig } from './config'
import API from '@/api/modules/attendance'
import type { BtnOptType, ModalType } from '@/types/global'
import { confirmMsg, successMsg, warningMsg } from '@/utils/message'

defineOptions({
  name: 'AttendanceRecord',
})

const xGrid = ref()

// 业务主键
// const keyId = 'recordId'
const { gridOptions } = useConfig(API)

// function handleOperate(type: BtnOptType, data: any = {}) {
//   API.sign(data[keyId]).then((res) => {
//     if (res.data) {
//       xGrid.value.commitProxy('reload')
//       successMsg('修改成功')
//     }
//   }).catch()
// }
</script>

<template>
  <PageMainFull>
    <vxe-grid ref="xGrid" v-bind="gridOptions">
      <template #toolbar_buttons />
      <!-- <template #operate="{ row }">
        <vxe-button content="代签" @click="handleOperate('edit', row)" />
      </template> -->
    </vxe-grid>
  </PageMainFull>
</template>
