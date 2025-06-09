<script setup>
import { reactive, ref } from 'vue'
import { closeToast, showFailToast, showLoadingToast, showSuccessToast } from 'vant'
import useUserStore from '@/store/modules/user'
import ApplyApi from '@/api/modules/apply'

const formData = reactive({
  dormitoryId: 0,
  faultDescription: '',
  repairFacilities: '',
  studentId: 0,
})
const disabled = ref(false)
const userInfo = computed(() => useUserStore().userInfo)
function handleSubmit() {
  if (!formData.faultDescription) {
    showFailToast('请输入故障描述')
  }
  else if (!formData.repairFacilities) {
    showFailToast('请输入维修设施')
  }
  else {
    showLoadingToast({
      message: '加载中...',
      forbidClick: true,
      loadingType: 'spinner',
    })
    ApplyApi.repair(formData).then((res) => {
      if (res.data) {
        showSuccessToast('提交成功')
        setTimeout(() => {
          closeToast()
        }, 2000)
      }
    }).catch(() => {
      closeToast()
    })
  }
}
onMounted(() => {
  formData.dormitoryId = userInfo.value.dormitoryId
  formData.studentId = userInfo.value.studentId
  if (!userInfo.value.dormitoryId) {
    showFailToast('请先完善宿舍信息')
    disabled.value = true
  }
  if (!userInfo.value.studentId) {
    showFailToast('请先完善个人信息')
    disabled.value = true
  }
})
</script>

<template>
  <div class="w-full flex flex-col items-center p-4 text-gray-800">
    <!-- Form for Repair and Adjustment -->
    <div class="max-w-md w-full space-y-4">
      <div class="max-w-md min-h-64 w-full flex flex-col space-y-4">
        <!-- Fault Description -->
        <div>
          <label for="faultDescription" class="mb-1 block text-sm font-medium">故障描述</label>
          <textarea id="faultDescription" v-model="formData.faultDescription" placeholder="请详细描述故障情况(100字以内)" maxlength="100" class="h-20 w-full border-gray-300 rounded border-none bg-[#EBEDED] p-2" :disabled="disabled" />
        </div>

        <!-- Repair Device -->
        <div>
          <label for="repairFacilities" class="mb-1 block text-sm font-medium">维修设施</label>
          <input id="repairFacilities" v-model="formData.repairFacilities" type="text" placeholder="请详细描述维修设施" maxlength="20" class="w-full border-gray-300 rounded border-none bg-[#EBEDED] p-2" :disabled="disabled">
        </div>
      </div>
      <!-- Submit Button -->
      <div class="mt-4">
        <button v-if="disabled === false" type="submit" class="btn w-full" @click="handleSubmit">
          提交申请
        </button>
      </div>
    </div>
  </div>
</template>

<style lang="scss" scoped>
.btn {
  width: 100%;
  padding: 12px;
  font-size: 1.2rem;
  line-height: 1.8rem;
  color: #fff;
  text-align: center;
  // 线性渐变
  background: linear-gradient(90deg, #6e9bff 0%, #3071ff 100%);
  border: none;
  border-radius: 1.5rem;
}
</style>
