<script lang="ts" setup>
import { useAddConfig, useConfig } from './config'
import API from '@/api/modules/attendance'
import type { BtnOptType, ModalType } from '@/types/global'
import { confirmMsg, successMsg, warningMsg } from '@/utils/message'
import { VxePulldownInstance } from 'vxe-table'

defineOptions({
  name: 'AttendanceInfo',
})

const formModalRef = ref()
const xGrid = ref()

const modalFormData = ref({ applayUserId: '', buildingVOList: []   })

const modalType = ref<ModalType>('add')

// 业务主键
const keyId = 'signInId'
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
}

// #region
const formAddModalRef = ref()
const modalFormAddData = ref({})
const { formItems: formAddItems, formRules: formAddRules } = useAddConfig()

function onAddSubmit(modalType: ModalType, modalValue: any, close: (next?: boolean) => void) {
  console.log('🚀 ~ modalValue:', modalValue)
  API.add({ ...modalValue, endTime: `${modalValue.endTime} 23:59:59` }).then((res) => {
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
// #endregion

function handleOperate(type: BtnOptType, data: any = {}) {
  if (type === 'add') {
    formAddModalRef.value.open('add', data)
  }
  if (type === 'view') {
    API.info(data[keyId]).then((res) => {
      formModalRef.value.open('view', {})
      modalFormData.value = { ...res.data, signInStatus: String(res.data.signInStatus) }
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
    confirmMsg('确定要作废吗？').then(() => {
      API.voided(data[keyId]).then(() => {
        xGrid.value.commitProxy('reload')
      })
    }).catch(res => console.log(res))
  }
}

const pulldownRef = ref<VxePulldownInstance>()

const clickEvent3 = () => {
  const $pulldown = pulldownRef.value
  if ($pulldown) {
    $pulldown.togglePanel()
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
        <vxe-button v-if="new Date() < new Date(row.endTime) && row.signInStatus === 1" status="danger" content="作废"
                    @click="handleOperate('delete', row)" />
      </template>
    </vxe-grid>
    <FormModal ref="formModalRef" v-model="modalFormData" v-model:type="modalType" :form-items="formItems"
               :form-rules="formRules" :on-submit="onSubmit" :show-ok="modalType !== 'view'">
      <!-- slot -->
      <template #buildingVOListSlots>
        <vxe-pulldown ref="pulldownRef" popup-class-name="my-dropdown4" transfer>
          <template #default>
            <vxe-button icon="vxe-icon-arrow-down" @click="clickEvent3">展开详情
            </vxe-button>
          </template>
          <template #dropdown>
            <div class="w-[400px] h-[300px]">
              <vxe-grid border auto-resize height="auto" :row-config="{ isHover: true }"
                        :data="modalFormData.buildingVOList" :columns="[
                  { field: 'buildingName', title: '楼栋名称' },
                  { field: 'buildingFloor', title: '楼层' },
                  {
                    field: 'buildingType', title: '楼栋类型', formatter:
                    ({ cellValue }:any) => cellValue === 1 ? '男生宿舍' : '女生宿舍'
                  },
                ]">
              </vxe-grid>
            </div>
          </template>
        </vxe-pulldown>
      </template>
    </FormModal>
    <FormModal ref="formAddModalRef" v-model="modalFormAddData" type="edit" :form-items="formAddItems"
               :form-rules="formAddRules" :on-submit="onAddSubmit" :show-ok="true">
      <!-- slot -->
    </FormModal>
  </PageMainFull>
</template>
