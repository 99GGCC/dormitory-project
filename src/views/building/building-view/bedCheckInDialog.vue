<script lang="ts" setup>
import { computed, reactive, ref, watch } from 'vue'
import { t } from 'vxe-table'
import { successMsg, warningMsg } from '@/utils/message'
import BED_API from '@/api/modules/building/bed'
import STUDENT_API from '@/api/modules/student'

defineProps({
  type: {
    type: String,
    default: 'save',
  },
})

const emits = defineEmits(['onSubmit'])

const elFormRef = ref<any>(null)

const formData = reactive({
  bedId: 0,
  dormitoryId: 0,
  isHead: 0,
  useStudent: undefined,
})

const rules = ref({
  isHead: [{
    required: false,
    message: '请输入',
  }],
  useStudent: [{
    required: true,
    message: '请选择',
  }],
  dormitoryId: [],
})

const studentOptions = ref<any[]>([])
function refreshStudentOptions() {
  STUDENT_API.page({
    page: 1,
    pageSize: 1000,
  }).then((res) => {
    if (res.data) {
      studentOptions.value = res.data.records
    }
  })
}

const visible = ref(false)
const loading = ref(false)
const dialogType = ref<'add' | 'view' | 'edit'>('add')
const dialogData = ref({})

const title = computed(() => {
  return '安排床位'
})
function open(type: 'add' | 'view' | 'edit', data?: any) {
  console.log('🚀 ~ data:', data)
  dialogType.value = type
  visible.value = true
  dialogData.value = data
  formData.dormitoryId = data?.dormitoryId
  formData.bedId = data?.bedId
  refreshStudentOptions()
}
function onClose() {
  elFormRef.value && elFormRef.value?.resetFields()
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
      BED_API.arrange({
        ...formData,
        useStudent: Number(formData.useStudent),
      }).then((res) => {
        if (res.data) {
          successMsg('操作成功')
          emits('onSubmit', formData)
          close()
        }
        else {
          warningMsg('操作失败')
        }
      }).finally(() => {
        loading.value = false
      })
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
        <el-form-item label="使用学生" prop="useStudent">
          <el-select
            v-model="formData.useStudent" placeholder="请选择" clearable filterable
            :style="{ width: '100%' }"
          >
            <el-option
              v-for="(item, index) in studentOptions" :key="index" :label="item.studentName"
              :value="item.studentId"
            />
          </el-select>
        </el-form-item>
        <el-form-item label="是否宿舍长" prop="isHead">
          <el-switch v-model="formData.isHead" active-text="是" inactive-text="否" :active-value="1" :inactive-value="0" />
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
