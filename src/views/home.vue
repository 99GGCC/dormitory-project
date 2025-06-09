<script setup lang="ts">
import { useRoute } from 'vue-router'
import { closeToast, showLoadingToast, showSuccessToast } from 'vant'
import Attendance from './components/attendance.vue'
import Change from './components/change.vue'
import Home from './components/home.vue'
import Repair from './components/repair.vue'
import signApi from '@/api/modules/sign'

const active = ref(0)

/**
 * 手动切换tab
 * @param index
 */
function changeTab(index: number) {
  active.value = index
}

provide('changeTab', changeTab)

function handleRefresh() {
  window.location.reload()
}

// signInId=240829235405520&studentId=240827223129951
const route = useRoute()

function autoSign() {
  if (!route.query.recordId) {
    return
  }
  showLoadingToast({
    message: '加载中...',
    forbidClick: true,
    loadingType: 'spinner',
  })
  signApi.studentSign(+route.query.recordId).then((res) => {
    if (res.data) {
      showSuccessToast('签到成功')
    }
    else {
      showSuccessToast('签到失败')
    }
    setTimeout(() => {
      closeToast()
    }, 2000)
  }).catch(() => {
    closeToast()
  })
}

onMounted(() => {
  if (route.query && route.query.recordId) {
    autoSign()
  }
})
</script>

<template>
  <PageLayout :navbar="false" tabbar>
    <div class="relative flex justify-center py-4">
      <div class="w-full flex flex-1 justify-center text-center text-lg text-[#192838] font-bold dark-text-white">
        宿舍通办
      </div>
      <div class="absolute right-0 flex items-center">
        <div class="flex px-2">
          <Icon icon="material-symbols:refresh-rounded" class="btn" @click="handleRefresh" />
        </div>
      </div>
    </div>
    <van-tabs v-model:active="active" animated>
      <van-tab title="首页">
        <div class="flex px-4 pt-2">
          <Home />
        </div>
      </van-tab>
      <van-tab title="考勤">
        <div class="flex px-2 pt-2">
          <Attendance />
        </div>
      </van-tab>
      <van-tab title="维修申请">
        <div class="flex px-2 pt-2">
          <Repair />
        </div>
      </van-tab>
      <van-tab title="床位调换">
        <div class="flex px-2 pt-2">
          <Change />
        </div>
      </van-tab>
    </van-tabs>
  </PageLayout>
</template>
