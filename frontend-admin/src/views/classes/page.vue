<script lang="ts" setup>
import { useConfig } from './config'
import API from '@/api/modules/classes'
import type { BtnOptType, ModalType } from '@/types/global'
import { confirmMsg, successMsg, warningMsg } from '@/utils/message'
import MAJOR_API from '@/api/modules/major'

defineOptions({
  name: 'OwnerMessage',
})

const formModalRef = ref()
const xGrid = ref()

const modalFormData = ref({ majorId: '' })

const modalType = ref<ModalType>('add')

const majorOptions = ref<any[]>([])
MAJOR_API.page({ limit: 999, page: 1 }).then((res) => {
  majorOptions.value = res.data.records
})

// 业务主键
const keyId = 'classesId'
const { gridOptions, formItems, formRules, setDisable } = useConfig(API)

function onSubmit(modalType: ModalType, modalValue: any, close: (next?: boolean) => void) {
  if (modalType === 'add') {
    API.add(modalValue).then((res) => {
      if (res.data) {
        xGrid.value.commitProxy('reload')
        close()
        successMsg('新增成功')
      }
      else {
        close(false)
      }
    }).catch(() => close(false))
  }
  else if (modalType === 'edit') {
    API.edit(modalValue[keyId], modalValue).then((res) => {
      if (res.data) {
        xGrid.value.commitProxy('reload')
        close()
        successMsg('修改成功')
      }
      else {
        close(false)
      }
    }).catch(() => close(false))
  }
}

function handleOperate(type: BtnOptType, data: any = {}) {
  if (type === 'add') {
    formModalRef.value.open('add', data)
    setDisable(false)
  }
  if (type === 'view') {
    API.info(data[keyId]).then((res) => {
      formModalRef.value.open('view', {})
      modalFormData.value = { ...res.data }
      setDisable(true)
    }).catch(() => warningMsg('获取详情失败'))
  }
  else if (type === 'edit') {
    API.info(data[keyId]).then((res) => {
      formModalRef.value.open('edit', {})
      modalFormData.value = { ...res.data }
      setDisable(false)
    }).catch(() => warningMsg('获取详情失败'))
  }
  else if (type === 'delete') {
    confirmMsg('确定要删除吗？').then(() => {
      API.delete(data[keyId]).then(() => {
        xGrid.value.commitProxy('reload')
      })
    }).catch(res => console.log(res))
  }
}
</script>

<template>
  <PageMainFull>
    <vxe-grid ref="xGrid" v-bind="gridOptions">
      <template #toolbar_buttons>
        <vxe-button
          status="primary"
          icon="vxe-icon-add"
          @click="handleOperate('add')"
        >
          新增
        </vxe-button>
      </template>
      <template #operate="{ row }">
        <vxe-button content="详情" @click="handleOperate('view', row)" />
        <vxe-button content="编辑" @click="handleOperate('edit', row)" />
        <vxe-button
          status="danger" content="删除"
          @click="handleOperate('delete', row)"
        />
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
      <template #majorIdSlots>
        <vxe-select
          v-model="modalFormData.majorId" :disabled="modalType === 'view'" filterable transfer
        >
          <vxe-option v-for="item in majorOptions" :key="item.majorId" :value="item.majorId" :label="item.majorName" clearable />
        </vxe-select>
      </template>
    </FormModal>
  </PageMainFull>
</template>
