<script lang="ts" setup>
import { computed, reactive, ref, watch } from 'vue'
import { successMsg, warningMsg } from '@/utils/message'
import useDictStore from '@/store/modules/dict'
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
  studentId: '',
  status: '',
  studentName: '',
  studentNum: '',
  studentPhone: '',
  studentEmail: '',
  studentSex: '',
  collegeName: '',
  classesName: '',
  majorName: '',
  dormitoryName: '',
})

const rules = ref({
  status: [{
    required: true,
    message: '请选择',
  }],
})
const dictStore = useDictStore()
const statusOptions = computed<any[]>(() => {
  return dictStore.getDict('STUDENT_STATUS')
})

const visible = ref(false)
const loading = ref(false)
const dialogType = ref<'add' | 'view' | 'edit'>('add')
const dialogData = ref<any>({})

const title = computed(() => {
  return '设置状态'
})
function open(type: 'add' | 'view' | 'edit', data?: any) {
  console.log('🚀 ~ data:', data)
  dialogType.value = type
  visible.value = true
  dialogData.value = data
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
      if (dialogType.value === 'edit' && dialogData.value?.studentId) {
        STUDENT_API.status(dialogData.value.studentId, {
          status: +formData.status,
        }).then((res: any) => {
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
        <el-row :gutter="12">
          <el-col :span="8">
            <el-form-item prop="studentName" label="学生姓名">
              <el-input v-model="formData.studentName" disabled />
            </el-form-item>
          </el-col>
          <el-col :span="8">
            <el-form-item prop="studentNum" label="学生学号">
              <el-input v-model="formData.studentNum" disabled />
            </el-form-item>
          </el-col>
          <el-col :span="8">
            <el-form-item prop="studentPhone" label="手机号码">
              <el-input v-model="formData.studentPhone" disabled />
            </el-form-item>
          </el-col>
          <el-col :span="8">
            <el-form-item prop="studentEmail" label="学生邮箱">
              <el-input v-model="formData.studentEmail" disabled />
            </el-form-item>
          </el-col>
          <el-col :span="8">
            <el-form-item prop="studentSex" label="学生性别">
              <el-input :value="Number(formData.studentSex) === 0 ? '女' : '男'" disabled />
            </el-form-item>
          </el-col>
          <el-col :span="8">
            <el-form-item prop="collegeName" label="学院名称">
              <el-input v-model="formData.collegeName" disabled />
            </el-form-item>
          </el-col>
          <el-col :span="8">
            <el-form-item prop="majorName" label="专业名称">
              <el-input v-model="formData.majorName" disabled />
            </el-form-item>
          </el-col>
          <el-col :span="8">
            <el-form-item prop="classesName" label="班级名称">
              <el-input v-model="formData.classesName" disabled />
            </el-form-item>
          </el-col>
          <el-col :span="8">
            <el-form-item prop="dormitoryName" label="宿舍">
              <el-input v-model="formData.dormitoryName" disabled />
            </el-form-item>
          </el-col>
        </el-row>
        <el-form-item label="学生状态" prop="status">
          <el-select
            v-model="formData.status" placeholder="请选择" clearable filterable
            :style="{ width: '100%' }"
          >
            <el-option
              v-for="(item, index) in statusOptions" :key="index" :label="item.label"
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
