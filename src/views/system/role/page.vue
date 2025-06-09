<script lang="ts" setup>
import { useConfig } from './config'
import RoleTreeModal from './components/role-tree.vue'
import API from '@/api/modules/system/role'
import type { BtnOptType, ModalType } from '@/types/global'
import { confirmMsg, successMsg, warningMsg } from '@/utils/message'

defineOptions({
  name: 'SystemRole',
})

const formModalRef = ref()
const xGrid = ref()

const modalFormData = ref({ alarmVehicleId: '' })

const modalType = ref<ModalType>('add')

// 业务主键
const keyId = 'roleId'
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
    formModalRef.value.open('add', {})
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

const RoleTreeModalRef = ref()

function handelOptAuth(row: any) {
  RoleTreeModalRef.value.open(row)
}
</script>

<template>
  <PageMainFull>
    <vxe-grid ref="xGrid" v-bind="gridOptions">
      <template #toolbar_buttons>
        <vxe-button
          v-auth="'role:add'"
          status="primary"
          icon="vxe-icon-add"
          @click="handleOperate('add')"
        >
          新增
        </vxe-button>
      </template>
      <template #operate="{ row }">
        <vxe-button v-auth="'role:detail'" content="详情" @click="handleOperate('view', row)" />
        <vxe-button v-auth="'role:edit'" content="编辑" @click="handleOperate('edit', row)" />
        <vxe-button v-auth-all="['role:tree', 'role:empower']" content="授权" @click="handelOptAuth(row)" />
        <vxe-button
          v-if="row.roleFlag !== 'admin'"
          v-auth="'role:del'"
          status="danger"
          content="删除"
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
    </FormModal>
    <RoleTreeModal ref="RoleTreeModalRef" />
  </PageMainFull>
</template>
