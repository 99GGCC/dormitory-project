<script lang="ts" setup>
import { computed, ref } from 'vue'
import useDictStore from '@/store/modules/dict'
import { createRequiredValidateRule, createVxeFormItem } from '@/utils/vxe-utils'
import type { ModalType } from '@/types/global'
import API from '@/api/modules/changeApply'
import { successMsg } from '@/utils/message'
import BUILDING_API from '@/api/modules/building/info'
import DORMITORY_API from '@/api/modules/building/dormitory'
import BED_API from '@/api/modules/building/bed'
import STUDENT_API from '@/api/modules/student'

const keyId = 'changeId'
const formModalRef = ref()
const modalType = ref<ModalType>('edit')
const modalFormData = ref({
  studentId: '',
  buildingId: '', dormitoryId: '', bedId: '',
  inBuildingId: '', inDormitoryId: '', inBedId: '',
  status: null, applyResult: '', applyStatus: null
})
const formItems = ref([
  {
    field: 'studentId', title: '申请学生', span: 24,
    titleWidth: 150,
    slots: {
      default: 'studentIdSlots',
    },
    itemRender: {
      props: {
        disabled: true,
      },
    },
  },
  {
    field: 'buildingId', title: '原楼栋', span: 8,
    titleWidth: 150,
    slots: {
      default: 'buildingIdSlots',
    },
    itemRender: {
      props: {
        disabled: true,
      },
    },
  },
  {
    field: 'dormitoryId', title: '原宿舍', span: 8,
    titleWidth: 80,
    slots: {
      default: 'dormitoryIdSlots',
    },
    itemRender: {
      props: {
        disabled: true,
      },
    },
  },
  {
    field: 'bedId', title: '原床位', span: 8,
    titleWidth: 80,
    slots: {
      default: 'bedIdSlots',
    },
    itemRender: {
      props: {
        disabled: true,
      },
    },
  },
  createVxeFormItem('textarea', {
    field: 'applyReason', title: '申请原因',
  }),
  {
    field: 'inBuildingId', title: '新楼栋', span: 8,
    titleWidth: 150,
    slots: {
      default: 'inBuildingIdSlots',
    },
    itemRender: {
      props: {
        disabled: true,
      },
    },
  },
  {
    field: 'inDormitoryId', title: '新宿舍', span: 8,
    titleWidth: 80,
    slots: {
      default: 'inDormitoryIdSlots',
    },
    itemRender: {
      props: {
        disabled: true,
      },
    },
  },
  {
    field: 'inBedId', title: '新床位', span: 8,
    titleWidth: 80,
    slots: {
      default: 'inBedIdSlots',
    },
    itemRender: {
      props: {
        disabled: true,
      },
    },
  },
  {
    field: 'status', title: '状态', span: 24,
    titleWidth: 150,
    slots: {
      default: 'statusSlots',
    },
    itemRender: {
      props: {
        disabled: true,
      },
    },
  },
  {
    field: 'applyResult', title: '处理意见', span: 24,
    titleWidth: 150,
    slots: {
      default: 'applyResultSlots',
    },
    itemRender: {
      props: {
        disabled: true,
      },
    },
  },
])

const formRules = ref({
  studentId: [createRequiredValidateRule()],
  buildingId: [createRequiredValidateRule()],
  dormitoryId: [createRequiredValidateRule()],
  bedId: [createRequiredValidateRule()],
  applyReason: [createRequiredValidateRule()],
  inBuildingId: [createRequiredValidateRule()],
  inDormitoryId: [createRequiredValidateRule()],
  inBedId: [createRequiredValidateRule()],
  status: [createRequiredValidateRule()],
  applyResult: [createRequiredValidateRule()],
})

function onSubmit(modalType: ModalType, modalValue: any, close: (next?: boolean) => void) {
  if (modalType === 'edit') {
    API.status(modalValue[keyId], {
      status: modalValue.status,
      applyResult: modalValue.applyResult,
    }).then((res) => {
      if (res.data) {
        close()
        successMsg('修改成功')
      }
      else {
        close(false)
      }
    }).catch(() => close(false))
  }
}
function open(data: any) {
  formModalRef.value.open(data)
  modalFormData.value = { ...data }
  modalType.value = 'edit'
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
  if(typeof modalFormData.value?.applyStatus !== 'undefined'){
    modalFormData.value.status = modalFormData.value.applyStatus
  }
}
const dictStore = useDictStore()
const statusOptions = computed(() => dictStore.getDict('APPLY_STATUS'))

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

// 0 已申请 1 通过申请 2 拒绝申请 3 取消申请
// 已申请 -> 只能变为其他三种状态
// 通过申请/拒绝申请/取消申请 -> 不能选择状态了
function getStatusOptionDisabled({ value }: { value: number }) {
  if (modalFormData.value.status === 0) {
    return value === 0
  } else if (modalFormData.value.status === 1 || modalFormData.value.status === 2 || modalFormData.value.status === 3) {
    return true
  }
}

defineExpose({
  open,
})
</script>

<template>
  <FormModal ref="formModalRef" v-model="modalFormData" v-model:type="modalType" :form-items="formItems"
    :form-rules="formRules" :on-submit="onSubmit" :show-ok="true" title="表单">
    <!-- slot -->

    <template #studentIdSlots>
      <vxe-select v-model="modalFormData.studentId" :disabled="true" filterable transfer @change="handleStudentChange">
        <vxe-option v-for="item in studentOptions" :key="item.studentId" :value="item.studentId"
          :label="item.studentName" clearable />
      </vxe-select>
    </template>

    <template #buildingIdSlots>
      <vxe-select v-model="modalFormData.buildingId" :disabled="true" filterable transfer
        @change="handleBuildingChange">
        <vxe-option v-for="item in buildingOptions" :key="item.buildingId" :value="item.buildingId"
          :label="item.buildingName" clearable />
      </vxe-select>
    </template>
    <template #dormitoryIdSlots>
      <vxe-select v-model="modalFormData.dormitoryId" :disabled="true" filterable transfer
        @change="handleDormitoryChange">
        <vxe-option v-for="item in dormitoryOptions" :key="item.dormitoryId" :value="item.dormitoryId"
          :label="item.dormitoryName" clearable />
      </vxe-select>
    </template>
    <template #bedIdSlots>
      <vxe-select v-model="modalFormData.bedId" :disabled="true" filterable transfer>
        <vxe-option v-for="item in bedOptions" :key="item.bedId" :value="item.bedId" :label="item.bedName" clearable />
      </vxe-select>
    </template>

    <template #inBuildingIdSlots>
      <vxe-select v-model="modalFormData.inBuildingId" :disabled="true" filterable transfer
        @change="handleInBuildingChange">
        <vxe-option v-for="item in inBuildingOptions" :key="item.buildingId" :value="item.buildingId"
          :label="item.buildingName" clearable />
      </vxe-select>
    </template>
    <template #inDormitoryIdSlots>
      <vxe-select v-model="modalFormData.inDormitoryId" :disabled="true" filterable transfer
        @change="handleInDormitoryChange">
        <vxe-option v-for="item in inDormitoryOptions" :key="item.dormitoryId" :value="item.dormitoryId"
          :label="item.dormitoryName" clearable />
      </vxe-select>
    </template>
    <template #inBedIdSlots>
      <vxe-select v-model="modalFormData.inBedId" :disabled="true" filterable transfer>
        <vxe-option v-for="item in inBedOptions" :key="item.bedId" :value="item.bedId" :label="item.bedName"
          clearable />
      </vxe-select>
    </template>
    <template #statusSlots>
      <vxe-select v-model="modalFormData.status" filterable transfer>
        <vxe-option v-for="item in statusOptions" :key="item.value" :value="item.value" :label="item.label" clearable
          :disabled="getStatusOptionDisabled(item)" />
      </vxe-select>
    </template>
    <template #applyResultSlots>
      <vxe-textarea v-model="modalFormData.applyResult" placeholder="请输入" :maxlength="100" />
    </template>
  </FormModal>
</template>

<style lang="scss" scoped></style>
