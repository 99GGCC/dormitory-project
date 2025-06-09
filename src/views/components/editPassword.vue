<script lang="ts" setup>
import { ref } from 'vue'
import { closeToast, showFailToast, showLoadingToast, showSuccessToast } from 'vant'
import useUserStore from '@/store/modules/user'
import userApi from '@/api/modules/user'
import { isEmail, isPassword, isPhoneNumber } from '@/utils/is'

const emits = defineEmits(['onSuccess', 'onError'])
const disabled = ref(false)
const formData = reactive({
  oldPass: '',
  newPass: '',
})
function handleSubmit() {
  disabled.value = true
  if (formData.oldPass === '' || formData.newPass === '') {
    showFailToast('请填写完整信息')
    disabled.value = false
    emits('onError', '请填写完整信息')
    return
  }
  if (formData.newPass.length < 6) {
    showFailToast('密码长度不能小于6位')
    disabled.value = false
    emits('onError', '密码长度不能小于6位')
    return
  }
  showLoadingToast({
    message: '加载中...',
    forbidClick: true,
    loadingType: 'spinner',
    duration: 0,
  })
  userApi.editPass(formData).then((res) => {
    if (res.data) {
      disabled.value = false
      emits('onSuccess')
      showSuccessToast('提交成功')
      useUserStore().studentIdLogin(useUserStore().userInfo.studentId)
      setTimeout(() => {
        closeToast()
      }, 2000)
    }
  }).catch(() => {
    closeToast()
  })
}
defineExpose({
  submit: handleSubmit,
})
</script>

<template>
  <div class="w-full flex flex-col items-center p-4 text-gray-800">
    <div class="max-w-md w-full space-y-4">
      <div class="max-w-md min-h-64 w-full flex flex-col space-y-4">
        <!-- Fault Description -->
        <div>
          <label for="studentEmail" class="mb-1 block text-sm font-medium">旧密码</label>
          <input id="studentEmail" v-model="formData.oldPass" type="password" placeholder="请输入旧密码" maxlength="20" class="w-full border-gray-300 rounded border-none bg-[#EBEDED] p-2" :disabled="disabled">
        </div>
        <div>
          <label for="studentPhone" class="mb-1 block text-sm font-medium">新密码</label>
          <input id="studentPhone" v-model="formData.newPass" type="password" placeholder="请输入新密码" maxlength="20" class="w-full border-gray-300 rounded border-none bg-[#EBEDED] p-2" :disabled="disabled">
        </div>

        <!-- Submit Button -->
        <div class="mt-4">
          <button :class="disabled ? 'bg-gray-500 pointer-events-none' : null" type="submit" class="btn w-full" @click="handleSubmit">
            提交修改
          </button>
        </div>
      </div>
    </div>
  </div>
</template>

<style lang="scss" scoped>
.btn {
  width: 100%;
  padding: 8px 12px;
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
