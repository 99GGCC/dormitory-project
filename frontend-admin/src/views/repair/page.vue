<script lang="ts" setup>
import {useConfig} from './config'
import API from '@/api/modules/repair'
import type {BtnOptType, ModalType} from '@/types/global'
import {confirmMsg, successMsg, warningMsg} from '@/utils/message'
import useDictStore from '@/store/modules/dict'

defineOptions({
  name: 'Repair',
})

const formModalRef = ref()
const xGrid = ref()

const modalFormData = ref({applayUserId: '',repairStatus:null})

const modalType = ref<ModalType>('add')
const dictStore = useDictStore()
// 业务主键
const keyId = 'repairId'
const {gridOptions, formItems, formRules, setDisable} = useConfig(API)

function onSubmit(modalType: ModalType, modalValue: any, close: (next?: boolean) => void) {
  if (modalType === 'edit') {
    API.edit(modalValue[keyId], {
      status: modalValue.repairStatus,
    }).then((res) => {
      if (res.data) {
        xGrid.value.commitProxy('reload')
        close()
        successMsg('修改成功')
      } else {
        close(false)
      }
    }).catch(() => close(false))
  }
}

function handleOperate(type: BtnOptType, data: any = {}) {
  if (type === 'view') {
    API.info(data[keyId]).then((res) => {
      formModalRef.value.open('view', {})
      modalFormData.value = {...res.data, repairStatus: res.data.repairStatus}
      setDisable(true)
    }).catch(() => warningMsg('获取详情失败'))
  } else if (type === 'edit') {
    API.info(data[keyId]).then((res) => {
      formModalRef.value.open('edit', {})
      modalFormData.value = {...res.data, repairStatus: res.data.repairStatus}
      setDisable(false)
    }).catch(() => warningMsg('获取详情失败'))
  }
}

const repairStatusList = computed(() => {
  return dictStore.getDict('REPAIR_STATUS')
})

// 获取处理状态下拉框禁用
// 0 已申请 1 处理中 2 已处理 3 无需处理 4 取消处理
// 维修状态为已申请 ---> 只能改为 已处理 / 无需处理 / 处理中
// 维修状态为处理中 ---> 只能改为 已处理
// 维修状态为已处理 / 无需处理 / 已取消  ---> 不能修改为其他状态了
function getRepairStatusOptionDisabled(itemValue: number) {
  const modalStatus = modalFormData.value.repairStatus
  if (modalStatus === 0) {
    return ![1, 2, 3].includes(itemValue)
  } else if (modalStatus === 1) {
    return itemValue !== 2
  } else if (modalStatus === 2 || modalStatus === 3 || modalStatus === 4) {
    return true
  } else {
    return false
  }
}

</script>

<template>
  <PageMainFull>
    <vxe-grid ref="xGrid" v-bind="gridOptions">
      <template #toolbar_buttons/>
      <template #operate="{ row }">
        <vxe-button content="详情" @click="handleOperate('view', row)"/>
        <vxe-button content="处理" @click="handleOperate('edit', row)"/>
      </template>
    </vxe-grid>
    <FormModal
      ref="formModalRef"
      v-model="modalFormData"
      v-model:type="modalType"
      :form-items="formItems"
      :form-rules="formRules"
      :on-submit="onSubmit"
      :show-ok="modalType !== 'view'"
    >
      <!-- slot -->
      <template #repairStatusSlots="{  }">
        <vxe-select
          v-model="modalFormData.repairStatus" :disabled="modalType === 'view'" filterable transfer
        >
          <vxe-option
            v-for="item in repairStatusList" :key="item.value" :value="item.value" :label="item.label" clearable
            :disabled="getRepairStatusOptionDisabled(item.value) && modalType === 'edit'"
          />
        </vxe-select>
      </template>
    </FormModal>
  </PageMainFull>
</template>
