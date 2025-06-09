<script lang="ts" setup>
import { useConfig } from './config'
import SetStatusDialog from './setStatus.dialog.vue'
import API from '@/api/modules/student'
import type { BtnOptType, ModalType } from '@/types/global'
import { confirmMsg, successMsg, warningMsg } from '@/utils/message'
import COLLEGE_API from '@/api/modules/college'
import MAJORR_API from '@/api/modules/major'
import CLASSES_API from '@/api/modules/classes'
import BUILDING_API from '@/api/modules/building/info'
import DORMITORY_API from '@/api/modules/building/dormitory'
import BED_API from '@/api/modules/building/bed'

defineOptions({
  name: 'Student',
})

const formModalRef = ref()
const setStatusDialogRef = ref<InstanceType<typeof SetStatusDialog> | null>()
const xGrid = ref()

const modalFormData = ref({
  studentId: '', collegeId: '', majorId: '', classesId: '', buildingId: '',
  dormitoryId: '',
  bedId: '',
})

const modalType = ref<ModalType>('add')

// 业务主键
const keyId = 'studentId'
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
      if (modalFormData.value.collegeId) {
        refreshMajorOptions(+modalFormData.value.collegeId)
      }
      if (modalFormData.value.majorId) {
        refreshClassesOptions(+modalFormData.value.majorId)
      }
      if (modalFormData.value.buildingId) {
        refreshDormitoryOptions(+modalFormData.value.buildingId)
      }
      if (modalFormData.value.dormitoryId) {
        refreshBedOptions(+modalFormData.value.dormitoryId)
      }
      setDisable(true)
    }).catch(() => warningMsg('获取详情失败'))
  }
  else if (type === 'edit') {
    API.info(data[keyId]).then((res) => {
      formModalRef.value.open('edit', {})

      modalFormData.value = { ...res.data }
      if (modalFormData.value.collegeId) {
        refreshMajorOptions(+modalFormData.value.collegeId)
      }
      if (modalFormData.value.majorId) {
        refreshClassesOptions(+modalFormData.value.majorId)
      }
      if (modalFormData.value.buildingId) {
        refreshDormitoryOptions(+modalFormData.value.buildingId)
      }
      if (modalFormData.value.dormitoryId) {
        refreshBedOptions(+modalFormData.value.dormitoryId)
      }
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
  else if (type === 'handle') {
    if (setStatusDialogRef.value) {
      setStatusDialogRef.value.open('edit', data)
    }
  }
}

const collegeOptions = ref<any[]>([])
COLLEGE_API.page({ limit: 999, pageSize: 1 }).then((res) => {
  collegeOptions.value = res.data.records
})

function handleColleggeChange({ value }: { value: number }) {
  modalFormData.value.majorId = ''
  modalFormData.value.classesId = ''
  refreshMajorOptions(value)
}

const majorOptions = ref<any[]>([])
function refreshMajorOptions(collegeId?: number) {
  MAJORR_API.page({ limit: 999, pageSize: 1, collegeId: collegeId || '' }).then((res) => {
    majorOptions.value = res.data.records
  })
}
function handleMajorChange({ value }: { value: number }) {
  console.log('🚀 ~ args:', value)
  modalFormData.value.classesId = ''
  refreshClassesOptions(value)
}

const classesOptions = ref<any[]>([])
function refreshClassesOptions(majorId?: number) {
  CLASSES_API.page({ limit: 999, pageSize: 1, majorId: majorId || '' }).then((res) => {
    classesOptions.value = res.data.records
  })
}
// #region 床位
const buildingOptions = ref<any[]>([])
BUILDING_API.page({ limit: 999, pageSize: 1 }).then((res) => {
  buildingOptions.value = res.data.records
})

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
  BED_API.list({ limit: 999, pageSize: 1, dormitoryId: dormitoryId || '' }).then((res) => {
    bedOptions.value = res.data
  })
}
// #endregion
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
        <vxe-button content="状态" @click="handleOperate('handle', row)" />
        <vxe-button status="danger" content="删除" @click="handleOperate('delete', row)" />
      </template>
    </vxe-grid>
    <FormModal
      ref="formModalRef" v-model="modalFormData" v-model:type="modalType" :form-items="formItems"
      :form-rules="formRules" :on-submit="onSubmit" :show-ok="modalType !== 'view'"
    >
      <!-- slot -->
      <template #collegeIdSlots>
        <vxe-select
          v-model="modalFormData.collegeId" :disabled="modalType === 'view'" filterable transfer
          @change="handleColleggeChange"
        >
          <vxe-option
            v-for="item in collegeOptions" :key="item.collegeId" :value="item.collegeId"
            :label="item.collegeName" clearable
          />
        </vxe-select>
      </template>
      <template #majorIdSlots>
        <vxe-select
          v-model="modalFormData.majorId" :disabled="modalType === 'view'" filterable transfer
          @change="handleMajorChange"
        >
          <vxe-option
            v-for="item in majorOptions" :key="item.majorId" :value="item.majorId" :label="item.majorName"
            clearable
          />
        </vxe-select>
      </template>
      <template #classesIdSlots>
        <vxe-select v-model="modalFormData.classesId" :disabled="modalType === 'view'" filterable transfer>
          <vxe-option
            v-for="item in classesOptions" :key="item.classesId" :value="item.classesId"
            :label="item.classesName" clearable
          />
        </vxe-select>
      </template>

      <template #buildingIdSlots>
        <vxe-select
          v-model="modalFormData.buildingId" :disabled="modalType === 'view'" filterable transfer clearable
          @change="handleBuildingChange"
        >
          <vxe-option
            v-for="item in buildingOptions" :key="item.buildingId" :value="item.buildingId"
            :label="item.buildingName"
          />
        </vxe-select>
      </template>
      <template #dormitoryIdSlots>
        <vxe-select
          v-model="modalFormData.dormitoryId" :disabled="modalType === 'view'" filterable transfer clearable
          @change="handleDormitoryChange"
        >
          <vxe-option
            v-for="item in dormitoryOptions" :key="item.dormitoryId" :value="item.dormitoryId"
            :label="item.dormitoryName"
          />
        </vxe-select>
      </template>
      <template #bedIdSlots>
        <vxe-select v-model="modalFormData.bedId" :disabled="modalType === 'view'" filterable transfer clearable>
          <vxe-option
            v-for="item in bedOptions" :key="item.bedId" :value="item.bedId" :label="item.bedName"
          />
        </vxe-select>
      </template>
    </FormModal>

    <!-- 设置状态 -->
    <SetStatusDialog
      ref="setStatusDialogRef" @on-submit="() => {
        xGrid.commitProxy('reload')
      }"
    />
  </PageMainFull>
</template>
