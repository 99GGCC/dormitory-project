<script lang="ts" setup>
import { computed, reactive, ref, watch } from 'vue'
import { warningMsg } from '@/utils/message'

import STUDENT_API from '@/api/modules/student'
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
  bedName: undefined,
  studentId: undefined,
  classesName: undefined,
  studentNum: undefined,
  collegeName: undefined,
  majorName: undefined,
  studentPhone: undefined,
  studentEmail: undefined,
  isHead: false,
  bedStatus: undefined,
})

const rules = ref({
  bedName: [{
    required: true,
    message: '请输入床位名称',
  }],
  studentId: [{
    required: false,
    message: '请选择使用学生',
  }],
  classesName: [],
  studentNum: [],
  collegeName: [],
  majorName: [],
  studentPhone: [],
  bedStatus: [{
    required: true,
    message: '请选择床位状态',
  }],
})
const bedStatusOptions = ref([{
  label: '启用',
  value: 1,
}, {
  label: '禁用',
  value: 0,
}])

const visible = ref(false)
const dialogType = ref<'add' | 'view' | 'edit'>('add')
const dialogData = ref({})

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
      // BED_API.add(formData).then((res) => {
      //   console.log('🚀 ~ res:', res)
      // })
      emits('onSubmit', formData)
      close()
    }
    else {
      return warningMsg('请填写完整信息')
    }
  })
}

const studentOptions = ref<any[]>([])
function refreshStudentOptions() {
  STUDENT_API.page({ limit: 9999, pageNum: 1 }).then((res) => {
    studentOptions.value = res.data.records
  })
}

function handleStudentChange(value: number) {
  console.log('🚀 ~ value:', value)
  const item = studentOptions.value.find(item => item.studentId === value)
  if (item) {
    console.log('🚀 ~ item:', item)
    Object.assign(formData, item)
  }
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
        <el-form-item label="使用学生" prop="studentId">
          <el-select
            v-model="formData.studentId" placeholder="请选择使用学生" filterable clearable
            :style="{ width: '100%' }"
            @change="handleStudentChange"
          >
            <el-option
              v-for="(item, index) in studentOptions" :key="index" :label="item.studentName"
              :value="item.studentId"
            />
          </el-select>
        </el-form-item>
        <el-row :gutter="15">
          <el-col :span="8">
            <el-form-item label="班级" prop="classesName">
              <el-input
                v-model="formData.classesName" placeholder="" :disabled="true"
                :style="{ width: '100%' }"
              />
            </el-form-item>
          </el-col>
          <el-col :span="8">
            <el-form-item label="学号" prop="studentNum">
              <el-input
                v-model="formData.studentNum" placeholder="" :disabled="true"
                :style="{ width: '100%' }"
              />
            </el-form-item>
          </el-col>
          <el-col :span="8">
            <el-form-item label="学院" prop="collegeName">
              <el-input
                v-model="formData.collegeName" placeholder="" :disabled="true"
                :style="{ width: '100%' }"
              />
            </el-form-item>
          </el-col>
          <el-col :span="8">
            <el-form-item label="专业" prop="majorName">
              <el-input
                v-model="formData.majorName" placeholder="" :disabled="true"
                :style="{ width: '100%' }"
              />
            </el-form-item>
          </el-col>
          <el-col :span="8">
            <el-form-item label="手机" prop="studentPhone">
              <el-input
                v-model="formData.studentPhone" placeholder="" :disabled="true"
                :style="{ width: '100%' }"
              />
            </el-form-item>
          </el-col>
          <el-col :span="8">
            <el-form-item label="邮箱" prop="studentEmail">
              <el-input
                v-model="formData.studentEmail" placeholder="" :disabled="true"
                :style="{ width: '100%' }"
              />
            </el-form-item>
          </el-col>
        </el-row>
        <el-form-item label="是否宿舍长" prop="isHead" required>
          <el-switch v-model="formData.isHead" />
        </el-form-item>
        <el-form-item label="床位状态" prop="bedStatus">
          <el-select
            v-model="formData.bedStatus" placeholder="请选择床位状态" filterable clearable
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
        <el-button type="primary" @click="handleConfirm">
          确定
        </el-button>
      </template>
    </el-dialog>
  </div>
</template>
