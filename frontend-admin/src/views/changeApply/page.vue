<script lang="ts" setup>
import { useConfig } from './config'
import StatusDialog from './statusDialog.vue'
import API from '@/api/modules/changeApply'
import type { BtnOptType, ModalType } from '@/types/global'
import { confirmMsg, successMsg, warningMsg } from '@/utils/message'
import BUILDING_API from '@/api/modules/building/info'
import DORMITORY_API from '@/api/modules/building/dormitory'
import BED_API from '@/api/modules/building/bed'
import STUDENT_API from '@/api/modules/student'

defineOptions({
  name: 'ChangeApply',
})

const formModalRef = ref()
const xGrid = ref()
const statusDialogRef = ref()

const modalFormData = ref({
  studentId: '',
  buildingId: '', dormitoryId: '', bedId: '',
  inBuildingId: '', inDormitoryId: '', inBedId: '',
  status: '', applyResult: '',
})

const modalType = ref<ModalType>('add')

// 业务主键
const keyId = 'changeId'
const { gridOptions, formItems, formRules, setDisable } = useConfig(API)

function onSubmit(modalType: ModalType, modalValue: any, close: (next?: boolean) => void) {
  if (modalType === 'add') {
    API.initiate(modalValue).then((res) => {
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
    API.status(modalValue[keyId], modalValue).then((res) => {
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
      console.log('🚀 ~ formItems.value:', formItems.value)
      if (modalFormData.value.buildingId) {
        refreshDormitoryOptions(+modalFormData.value.buildingId)
      }
      if (modalFormData.value.dormitoryId) {
        refreshBedOptions(+modalFormData.value.dormitoryId)
      }
      if (modalFormData.value.inBuildingId) {
        refreshInDormitoryOptions(+modalFormData.value.inBuildingId)
      }
      if (modalFormData.value.inDormitoryId) {
        refreshInBedOptions(+modalFormData.value.inDormitoryId)
      }
      setDisable(true)
    }).catch(() => warningMsg('获取详情失败'))
  }
  else if (type === 'handle') {
    API.info(data[keyId]).then((res) => {
      statusDialogRef.value.open(res.data)
      setDisable(true)
    }).catch(() => warningMsg('获取详情失败'))
  }
}

// #region 学生
const studentOptions = ref<any[]>([])
STUDENT_API.page({ limit: 999, page: 1 }).then((res) => {
  studentOptions.value = res.data.records
})
function handleStudentChange({ value }: { value: number }) {
  const studentInfo = studentOptions.value.find(item => item.studentId === value)
  console.log('🚀 ~ studentInfo:', studentInfo)
  modalFormData.value.buildingId = studentInfo.buildingId
  if (studentInfo.buildingId) {
    refreshBuildingOptions()
  }
  modalFormData.value.dormitoryId = studentInfo.dormitoryId
  if (studentInfo.dormitoryId) {
    refreshDormitoryOptions()
  }
  modalFormData.value.bedId = studentInfo.bedId
  if (studentInfo.bedId) {
    refreshBedOptions()
  }
}
// #endregion

// #region 床位
const buildingOptions = ref<any[]>([])
function refreshBuildingOptions() {
  BUILDING_API.page({ limit: 999, page: 1 }).then((res) => {
    buildingOptions.value = res.data.records
  })
}
refreshBuildingOptions()

function handleBuildingChange({ value }: { value: number }) {
  modalFormData.value.dormitoryId = ''
  modalFormData.value.bedId = ''
  refreshDormitoryOptions(value)
}

const dormitoryOptions = ref<any[]>([])
function refreshDormitoryOptions(buildingId?: number) {
  DORMITORY_API.list({ buildingId: buildingId || '' }).then((res) => {
    dormitoryOptions.value = res.data
  })
}
function handleDormitoryChange({ value }: { value: number }) {
  console.log('🚀 ~ args:', value)
  modalFormData.value.bedId = ''
  refreshBedOptions(value)
}

const bedOptions = ref<any[]>([])
function refreshBedOptions(dormitoryId?: number) {
  BED_API.list({ limit: 999, page: 1, dormitoryId: dormitoryId || '' }).then((res) => {
    bedOptions.value = res.data
  })
}
// #endregion

// #region 床位 old
const inBuildingOptions = ref<any[]>([])
function refreshInBuildingOptions() {
  BUILDING_API.page({ limit: 999, page: 1 }).then((res) => {
    inBuildingOptions.value = res.data.records
  })
}
refreshInBuildingOptions()

function handleInBuildingChange({ value }: { value: number }) {
  modalFormData.value.inDormitoryId = ''
  modalFormData.value.inBedId = ''
  refreshInDormitoryOptions(value)
}

const inDormitoryOptions = ref<any[]>([])
function refreshInDormitoryOptions(buildingId?: number) {
  DORMITORY_API.list({ buildingId: buildingId || '' }).then((res) => {
    inDormitoryOptions.value = res.data
  })
}
function handleInDormitoryChange({ value }: { value: number }) {
  console.log('🚀 ~ args:', value)
  modalFormData.value.inBedId = ''
  refreshInBedOptions(value)
}

const inBedOptions = ref<any[]>([])
function refreshInBedOptions(dormitoryId?: number) {
  BED_API.list({ limit: 999, page: 1, dormitoryId: dormitoryId || '' }).then((res) => {
    inBedOptions.value = res.data
  })
}
// #endregion
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
        <vxe-button v-if="row.applyStatus === 0" content="处理" @click="handleOperate('handle', row)" />
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
      title="申请"
    >
      <!-- slot -->

      <template #studentIdSlots>
        <vxe-select
          v-model="modalFormData.studentId" :disabled="modalType !== 'add'" filterable transfer
          @change="handleStudentChange"
        >
          <vxe-option
            v-for="item in studentOptions" :key="item.studentId" :value="item.studentId"
            :label="item.studentName" clearable
          />
        </vxe-select>
      </template>

      <template #buildingIdSlots>
        <vxe-select
          v-model="modalFormData.buildingId" :disabled="modalType !== 'add'" filterable transfer
          @change="handleBuildingChange"
        >
          <vxe-option
            v-for="item in buildingOptions" :key="item.buildingId" :value="item.buildingId"
            :label="item.buildingName" clearable
          />
        </vxe-select>
      </template>
      <template #dormitoryIdSlots>
        <vxe-select
          v-model="modalFormData.dormitoryId" :disabled="modalType !== 'add'" filterable transfer
          @change="handleDormitoryChange"
        >
          <vxe-option
            v-for="item in dormitoryOptions" :key="item.dormitoryId" :value="item.dormitoryId"
            :label="item.dormitoryName" clearable
          />
        </vxe-select>
      </template>
      <template #bedIdSlots>
        <vxe-select v-model="modalFormData.bedId" :disabled="modalType !== 'add'" filterable transfer>
          <vxe-option
            v-for="item in bedOptions" :key="item.bedId" :value="item.bedId" :label="item.bedName"
            clearable
          />
        </vxe-select>
      </template>

      <template #inBuildingIdSlots>
        <vxe-select
          v-model="modalFormData.inBuildingId" :disabled="modalType !== 'add'" filterable transfer
          @change="handleInBuildingChange"
        >
          <vxe-option
            v-for="item in inBuildingOptions" :key="item.buildingId" :value="item.buildingId"
            :label="item.buildingName" clearable
          />
        </vxe-select>
      </template>
      <template #inDormitoryIdSlots>
        <vxe-select
          v-model="modalFormData.inDormitoryId" :disabled="modalType !== 'add'" filterable transfer
          @change="handleInDormitoryChange"
        >
          <vxe-option
            v-for="item in inDormitoryOptions" :key="item.dormitoryId" :value="item.dormitoryId"
            :label="item.dormitoryName" clearable
          />
        </vxe-select>
      </template>
      <template #inBedIdSlots>
        <vxe-select v-model="modalFormData.inBedId" :disabled="modalType !== 'add'" filterable transfer>
          <vxe-option
            v-for="item in inBedOptions" :key="item.bedId" :value="item.bedId" :label="item.bedName"
            clearable
          />
        </vxe-select>
      </template>
    </FormModal>
    <StatusDialog ref="statusDialogRef" />
  </PageMainFull>
</template>
