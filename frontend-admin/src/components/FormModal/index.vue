<script lang="ts" setup>
import type { PropType } from 'vue'
import {
  ref } from 'vue'
import { useVModel } from '@vueuse/core'
import type { VxeFormPropTypes } from 'vxe-table'
import { cloneDeep } from 'lodash-es'
import { warningMsg } from '@/utils/message'
import type { ModalType } from '@/types/global'

const props = defineProps({
  modelValue: {
    type: Object,
    default: () => ({}),
  },
  title: {
    type: String,
    default: '表单',
  },
  width: {
    type: String,
    default: '750px',
  },
  type: {
    type: String as PropType<ModalType>,
    default: 'add',
  },
  okText: {
    type: String,
    default: '确定',
  },
  cancelText: {
    type: String,
    default: '关闭',
  },
  formData: {
    type: Object,
    default: () => ({}),
  },
  formItems: {
    type: Array as PropType<VxeFormPropTypes.Items>,
    default: () => ([]),
  },
  formRules: {
    type: Object as PropType<VxeFormPropTypes.Rules>,
    default: () => ({}),
  },
  onSubmit: {
    type: Function,
    default: () => ({}),
  },
  onClose: {
    type: Function,
    default: () => ({}),
  },
  showFooter: {
    type: Boolean,
    default: true,
  },
  showCancel: {
    type: Boolean,
    default: true,
  },
  showOk: {
    type: Boolean,
    default: true,
  },
})
const emit = defineEmits(['update:modelValue', 'update:type'])
const modalLoading = ref(false)
const show = ref(false)
const formRef = ref()
const modalFormData = useVModel(props, 'modelValue', emit)
const modalType = useVModel(props, 'type', emit)
const modalFormItems = ref<VxeFormPropTypes.Items>(props.formItems)
const modalFormRules = ref<VxeFormPropTypes.Rules>(props.formRules)
const modalTitle = computed(() => {
  const map = {
    add: '新增',
    edit: '编辑',
    view: '查看',
    review: '审核',
  }
  return map[modalType.value] + props.title
})
function open(type: ModalType, data: any) {
  modalType.value = type
  modalFormData.value = cloneDeep(data)
  show.value = true
}
function cancelEvent() {
  modalLoading.value = false
  show.value = false
  formRef.value.reset()
  props.onClose()
}
function close(beClose = true) {
  if (beClose) {
    cancelEvent()
  }
  else {
    modalLoading.value = false
  }
}
function confirmEvent() {
  formRef.value.validate().then((res: any) => {
    if (res) {
      return warningMsg('表单校验失败')
    }
    else {
      modalLoading.value = true
      props.onSubmit(modalType.value, modalFormData.value, close)
    }
  })
}

const slots = useSlots()

defineExpose({
  formRef,
  open,
})
</script>

<template>
  <vxe-modal v-model="show" :loading="modalLoading" :title="modalTitle" :width="width" show-footer>
    <template #default>
      <VxeForm
        ref="formRef"
        :data="modalFormData"
        :items="modalFormItems"
        :rules="modalFormRules"
        title-width="150px"
        title-align="right"
        title-overflow
      >
        <template v-for="slot in Object.keys(slots)" #[slot]="{ data }">
          <slot :name="slot" :data="data" />
        </template>
      </VxeForm>
    </template>
    <template v-if="showFooter" #footer>
      <vxe-button v-if="showCancel" :content="cancelText" @click="cancelEvent" />
      <vxe-button v-if="showOk" status="primary" :content="okText" @click="confirmEvent" />
    </template>
  </vxe-modal>
</template>
