<script lang="ts" setup>
import { computed, reactive, ref, watch } from 'vue'
import { successMsg, warningMsg } from '@/utils/message'
import BED_API from '@/api/modules/building/bed'

defineProps({
  type: {
    type: String,
    default: 'save',
  },
})

const emits = defineEmits(['onSubmit'])

const elFormRef = ref<any>(null)

const formData = reactive({
  bedName: '',
  bedStatus: '',
  dormitoryId: 0,
})

const rules = ref({
  bedName: [{
    required: true,
    message: '请输入',
  }],
  bedStatus: [{
    required: true,
    message: '请选择',
  }],
  dormitoryId: [],
})
const bedStatusOptions = ref([{
  label: '启用',
  value: 1,
}, {
  label: '禁用',
  value: 0,
}])

const visible = ref(false)
const loading = ref(false)
const dialogType = ref<'add' | 'view' | 'edit'>('add')
const dialogData = ref<any>({})

const title = computed(() => {
  return {
    add: '新增床位',
    view: '查看床位',
    edit: '编辑床位',
  }[dialogType.value || 'add']
})
function open(type: 'add' | 'view' | 'edit', data?: any) {
  console.log('🚀 ~ data:', data)
  dialogType.value = type
  visible.value = true
  dialogData.value = data
  formData.dormitoryId = data?.dormitoryId
  if (type !== 'add') {
    Object.assign(formData, dialogData.value)
  }
}
function onClose() {
  elFormRef.value && elFormRef.value?.resetFields()
  loading.value = false
}

watch(() => visible.value, () => {
  onClose()
})
function close() {
  visible.value = false
}
function handleConfirm() {
  elFormRef.value && elFormRef.value?.validate((valid: any) => {
    console.log('🚀 ~ valid:', valid)
    if (valid) {
      loading.value = true

      if (dialogType.value === 'edit' && dialogData.value?.bedId) {
        BED_API.edit(dialogData.value.bedId, formData).then((res) => {
          if (res.data) {
            successMsg('编辑成功')
            emits('onSubmit', formData)
            close()
          }
          else {
            warningMsg('编辑失败')
          }
        }).finally(() => {
          loading.value = false
        })
      }
      else if (dialogType.value === 'add') {
        BED_API.add(formData).then((res) => {
          if (res.data) {
            successMsg('新增成功')
            emits('onSubmit', formData)
            close()
          }
          else {
            warningMsg('新增失败')
          }
        }).finally(() => {
          loading.value = false
        })
      }
    }
    else {
      return warningMsg('请填写完整信息')
    }
  })
}

defineExpose({
  open,
  close,
})
</script>

<template>
  <div>
    <el-dialog v-model="visible" :title="title">
      <el-form ref="elFormRef" :model="formData" :rules="rules" label-width="auto">
        <el-form-item label="床位名称" prop="bedName">
          <el-input
            v-model="formData.bedName" placeholder="请输入床位名称" :maxlength="10" clearable
            :style="{ width: '100%' }"
          />
        </el-form-item>
        <el-form-item label="床位状态" prop="bedStatus">
          <el-select
            v-model="formData.bedStatus" placeholder="请选择床位状态" clearable filterable
            :style="{ width: '100%' }"
          >
            <el-option
              v-for="(item, index) in bedStatusOptions" :key="index" :label="item.label"
              :value="item.value"
            />
          </el-select>
        </el-form-item>
      </el-form>
      <template #footer>
        <el-button @click="close">
          取消
        </el-button>
        <el-button v-loading="loading" type="primary" @click="handleConfirm">
          确定
        </el-button>
      </template>
    </el-dialog>
  </div>
</template>
