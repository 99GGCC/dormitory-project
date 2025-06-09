<script lang="ts" setup>
import PulldownTable from '../components/pulldown-table.vue'
import { useConfig } from './config'
import API from '@/api/modules/commodity/commodity'
import type { BtnOptType, ModalType } from '@/types/global'
import { confirmMsg, successMsg, warningMsg } from '@/utils/message'
import TYPE_API from '@/api/modules/commodity/lsType'

defineOptions({
  name: 'Commodity',
})

const formModalRef = ref()
const xGrid = ref()

const modalFormData = ref({ typeId: '', specsList: [] })

const modalType = ref<ModalType>('add')

// 业务主键
const keyId = 'commodityId'
const { gridOptions, formItems, formRules, setDisable } = useConfig(API)

const typeOptions = ref<any[]>([])
TYPE_API.list({}).then((res) => {
  typeOptions.value = res.data || []
})

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
      <template #commodityImgSlots="{ data }">
        <ImageUpload v-model="data.commodityImg" :disabled="modalType === 'view'" />
      </template>
      <template #typeIdSlots>
        <vxe-select
          v-model="modalFormData.typeId" :disabled="modalType === 'view'" filterable transfer
        >
          <vxe-option v-for="item in typeOptions" :key="item.typeId" :value="item.typeId" :label="item.typeName" clearable />
        </vxe-select>
      </template>
      <template #specsListSlots>
        <PulldownTable v-model="modalFormData.specsList" :disabled="modalType === 'view'" />
      </template>
    </FormModal>
  </PageMainFull>
</template>
