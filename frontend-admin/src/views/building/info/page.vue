<script lang="ts" setup>
import dayjs from 'dayjs'
import { useConfig } from './config'
import API from '@/api/modules/building/info'
import type { BtnOptType, ModalType } from '@/types/global'
import { confirmMsg, successMsg } from '@/utils/message'

defineOptions({
  name: 'SystemUser',
})

const formModalRef = ref()
const xGrid = ref()

const modalFormData = ref({
  buildingId: '',
})

const modalType = ref<ModalType>('add')

const keyId = 'buildingId'
const { gridOptions, formItems, formRules, setDisable } = useConfig()

function onSubmit(modalType: ModalType, modalValue: any, close: (next?: boolean) => void) {
  if (modalType === 'add') {
    API.add({ ...modalValue }).then((res) => {
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
    API.edit(modalValue[keyId], { ...modalValue }).then((res) => {
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
    formModalRef.value.open('add', {})
    setDisable(false)
  }
  if (type === 'view') {
    formModalRef.value.open('view', data)
    setDisable(true)
  }
  else if (type === 'edit') {
    formModalRef.value.open('edit', data)
    setDisable(false)
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
        <vxe-button status="primary" icon="vxe-icon-add" @click="handleOperate('add')">
          新增
        </vxe-button>
      </template>
      <template #operate="{ row }">
        <vxe-button content="详情" @click="handleOperate('view', row)" />
        <vxe-button content="编辑" @click="handleOperate('edit', row)" />
        <vxe-button
          v-if="row.userAccount !== 'admin'" status="danger" content="删除"
          @click="handleOperate('delete', row)"
        />
      </template>
    </vxe-grid>
    <FormModal
      ref="formModalRef" v-model="modalFormData" v-model:type="modalType" :form-items="formItems"
      :form-rules="formRules" :on-submit="onSubmit" :show-ok="modalType !== 'view'"
    >
      <!--  -->
    </FormModal>
  </PageMainFull>
</template>
