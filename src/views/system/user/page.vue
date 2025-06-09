<script lang="ts" setup>
import dayjs from 'dayjs'
import { useConfig } from './config'
import userApi from '@/api/modules/user'
import roleApi from '@/api/modules/system/role'
import type { BtnOptType, ModalType } from '@/types/global'
import { confirmMsg, successMsg } from '@/utils/message'

defineOptions({
  name: 'SystemUser',
})

const formModalRef = ref()
const xGrid = ref()

const modalFormData = ref({
  roleId: '',
})

const modalType = ref<ModalType>('add')

const roleList = ref<any[]>([])
roleApi.list().then((res) => {
  roleList.value = res.data.map((item: any) => ({
    label: item.roleName,
    value: item.roleId,
  }))
})

const keyId = 'adminId'
const { gridOptions, formItems, formRules, setDisable } = useConfig()

function onSubmit(modalType: ModalType, modalValue: any, close: (next?: boolean) => void) {
  if (modalType === 'add') {
    userApi.userAdd({ ...modalValue, userBirthday: dayjs(modalValue.userBirthday).valueOf() }).then((res) => {
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
    userApi.userEdit(modalValue[keyId], { ...modalValue, userBirthday: dayjs(modalValue.userBirthday).valueOf() }).then((res) => {
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
  console.log(data)
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
      userApi.userDelete(data[keyId], data).then(() => {
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
      <template #roleIdSlots>
        <vxe-select v-model="modalFormData.roleId" :disabled="modalType === 'view'" filterable transfer>
          <vxe-option v-for="item in roleList" :key="item.value" :value="item.value" :label="item.label" clearable />
        </vxe-select>
      </template>
    </FormModal>
  </PageMainFull>
</template>
