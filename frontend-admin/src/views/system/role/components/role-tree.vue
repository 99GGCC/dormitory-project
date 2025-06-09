<script lang="ts" setup>
import { ElTreeV2 } from 'element-plus'
import { ref } from 'vue'
import RoleMenuAPI from '@/api/modules/system/role-menu'
import { findNodesWithCondition } from '@/utils/dev'
import { successMsg, warningMsg } from '@/utils/message'

const emit = defineEmits(['close', 'submit'])

interface Tree {
  id: number
  label: string
  children?: Tree[]
}
const data: Ref<Tree[]> = ref([])

const treeRef = ref<InstanceType<typeof ElTreeV2>>()

const dialogValue = ref()

// label    指定节点标签为节点对象的某个属性值
// children 指定子树为节点对象的某个属性值
// disabled 指定节点选择框是否禁用为节点对象的某个属性值
// isLeaf   指定节点是否为叶子节点，仅在指定了 lazy 属性的情况下生效
// class    自定义节点类名
const defaultProps = {
  value: 'menuId',
  label: 'menuName',
  children: 'children',
}

const visible = ref(false)
const loading = ref(false)

function open(values: any) {
  visible.value = true
  dialogValue.value = values
  if (values.roleId) {
    loading.value = true
    RoleMenuAPI.menuTreeById(values.roleId).then((res) => {
      if (res.data) {
        data.value = res.data
        const selectedKeys = findNodesWithCondition(
          res.data,
          (node) => {
            return node.roleMenuId !== null
          },
          (node) => {
            return node.menuId
          },
          'children',
        )
        const expandedKeys = findNodesWithCondition(
          res.data,
          (node) => {
            return node.children.length > 0
          },
          (node) => {
            return node.menuId
          },
          'children',
        )
        nextTick(() => {
          treeRef.value!.setCheckedKeys(selectedKeys)
          treeRef.value!.setExpandedKeys(expandedKeys)
        })
      }
    }).finally(() => {
      loading.value = false
    })
  }
}

function close() {
  visible.value = false
  emit('close')
}

function save() {
  loading.value = true
  const checkedKeys = treeRef.value!.getCheckedKeys() || []
  RoleMenuAPI.empower({
    menuIds: checkedKeys,
    roleId: dialogValue.value.roleId,
  }).then((res) => {
    if (res.data) {
      successMsg('授权成功')
      close()
    }
  }).finally(() => {
    loading.value = false
  })
}

defineExpose({
  open,
})
</script>

<template>
  <vxe-modal v-model="visible" :loading="loading" title="角色授权" show-footer>
    <template #default>
      <ElTreeV2
        ref="treeRef"
        :data="data"
        show-checkbox
        default-expand-all
        node-key="menuId"
        highlight-current
        :props="defaultProps"
        :height="300"
      />
    </template>
    <template #footer>
      <vxe-button content="取消" @click="close" />
      <vxe-button status="primary" content="保存" @click="save" />
    </template>
  </vxe-modal>
</template>

<!-- <style lang="scss" scoped></style> -->
