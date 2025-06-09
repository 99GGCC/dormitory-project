<script lang="ts" setup>
import { cloneDeep } from 'lodash-es'
import API from '@/api/modules/param-manage'
import type { BtnOptType, ModalType } from '@/types/global'
import { confirmMsg, successMsg, warningMsg } from '@/utils/message'
import { createRequiredValidateRule, createVxeFormItem } from '@/utils/vxe-utils'

defineOptions({
  name: 'ParamManage',
})
// 业务主键
const keyId = 'paramId'

const formModalRef = ref()
const modalFormData = ref({
  url: '',
  content: '',
  sort: 0,
})
const paramId = ref(0)

const list = ref<any[]>([])
function refresh() {
  API.page({
    paramClass: 'PARAM_CAROUSE',
    page: 1,
    limit: 9999,
  }).then((res) => {
    const [data = {
      paramClass: 'PARAM_CAROUSE',
      paramId: 0,
      paramName: '',
      paramValue: '',
    }] = res.data.records
    paramId.value = data[keyId]
    list.value = JSON.parse(data.paramValue)
  })
}
const formItems = ref([
  createVxeFormItem('input', {
    field: 'content', title: '文字',
  }),
  {
    field: 'url', title: '图片', span: 24,
    slots: {
      default: 'urlSlots',
    },
    itemRender: {},
  },
  createVxeFormItem('input', {
    field: 'sort', title: '排序',
    props: {
      // 整数类型
      type: 'integer',
    },
  }),
])

const formRules = ref({
  url: [createRequiredValidateRule('sc')],
  sort: [createRequiredValidateRule()],
})

onMounted(() => {
  refresh()
})

const editIndex = ref<any>()

function onSubmit(modalType: ModalType, modalValue: any, close: (next?: boolean) => void) {
  const newData = cloneDeep(list.value)
  if (modalType === 'add') {
    newData.push(modalValue)
  }
  if (modalType === 'edit') {
    newData[editIndex.value] = modalValue
  }

  API.edit(paramId.value, {
    paramClass: 'PARAM_CAROUSE',
    paramName: '轮播图',
    paramValue: JSON.stringify(newData),
  }).then((res) => {
    if (res.data) {
      list.value = newData
      close()
      successMsg('修改成功')
    }
    else {
      close(false)
    }
  }).catch(() => close(false))
}

function deleteImg(index: number) {
  const newData = cloneDeep(list.value)
  newData.splice(index, 1)

  API.edit(paramId.value, {
    paramClass: 'PARAM_CAROUSE',
    paramName: '轮播图',
    paramValue: JSON.stringify(newData),
  }).then((res) => {
    if (res.data) {
      list.value = newData
      successMsg('修改成功')
    }
  }).catch()
}

function handleOperate(type: BtnOptType, data: any = {}, index?: number) {
  if (type === 'add') {
    formModalRef.value.open('add', data)
  }
  else if (type === 'edit' && index !== undefined) {
    formModalRef.value.open('edit', list.value[index])
    editIndex.value = index
  }
  else if (type === 'delete' && index !== undefined) {
    confirmMsg('确定要删除吗？').then(() => {
      editIndex.value = index
      deleteImg(index)
    }).catch(res => console.log(res))
  }
}
</script>

<template>
  <PageMainFull>
    <div class="my-3 flex flex-col gap-3">
      <div v-for="item, index in list" :key="item.id" class="flex flex-row items-center gap-4">
        <ImagePreview :src="item.url" :width="500" />
        <VxeButton status="danger" icon="vxe-icon-delete" circle @click="handleOperate('delete', item, index)" />
      </div>
    </div>

    <VxeButton content="添加" @click="handleOperate('add')" />

    <FormModal
      ref="formModalRef"
      v-model="modalFormData"
      :form-items="formItems"
      :form-rules="formRules"
      :on-submit="onSubmit"
    >
      <!-- slot -->
      <template #urlSlots="{ data }">
        <ImageUpload v-model="data.url" />
      </template>
    </FormModal>
  </PageMainFull>
</template>
