<script setup>
import { ref } from 'vue'
import { closeToast, showLoadingToast, showSuccessToast, showToast } from 'vant'
import useUserStore from '@/store/modules/user'
import NoticeApi from '@/api/modules/notice'
import signApi from '@/api/modules/sign'

defineOptions({
  name: 'Home',
})

const userStore = useUserStore()
const changeTab = inject('changeTab')

// #region 公告
const noticeList = ref([])
const showNotice = ref(false)
const notice = ref({})
function handleViewNotice(item) {
  showNotice.value = true
  notice.value = item
}
function refreshList() {
  NoticeApi.list({
    page: 1,
    limit: 9999,
  }).then((res) => {
    noticeList.value = res.data.reverse()
  })
}
onMounted(() => {
  refreshList()
})
// #endregion

// #region 申请
const showActions = ref(false)
const actions = ref([
  { name: '维修' },
  { name: '调换床位' },
])
function handleApply() {
  showActions.value = true
}
function onActionSelect(item) {
  showActions.value = false
  if (item.name === '维修') {
    changeTab(2)
  }
  if (item.name === '调换床位') {
    changeTab(3)
  }
}
// #endregion

// #region 签到
const signList = ref([])
async function setSignList() {
  signApi.getSignList({
    page: 1,
    limit: 3,
  }).then((res) => {
    signList.value = res.data.records
  })
}

function getSignStatusText(sign) {
  // 当前时间大于截止时间，状态是未签到的话，就是已过期
  if (sign.signInStatus === 0 && new Date() > new Date(sign.endTime)) {
    return '已过期'
  }
  // 如果是1，并且当前时间大于截止日期 endTime，就是已截止
  if (sign.signInStatus === 1 && new Date() > new Date(sign.endTime)) {
    return '已截止'
  }
  return sign.recordStatus === 1 ? '已签到' : '未签到'
}

function handleSign(signItem) {
  if (signItem.recordStatus === 1) {
    return showToast('已签到,无需重复签到')
  }
  if (signItem.signInStatus === 0 && new Date() > new Date(signItem.endTime)) {
    return showToast('签到已过期')
  }
  if (signItem.signInStatus === 1 && new Date() > new Date(signItem.endTime)) {
    return showToast('签到已截止')
  }
  showLoadingToast({
    message: '加载中...',
    forbidClick: true,
    loadingType: 'spinner',
  })
  signApi.studentSign(signItem.recordId).then((res) => {
    if (res.data) {
      showSuccessToast('签到成功')
      setSignList()
    }
    else {
      showSuccessToast('签到失败')
    }
  }).finally(() => {
    closeToast()
  })
}
onMounted(() => {
  setSignList()
})
// #endregion
</script>

<template>
  <div class="home w-full">
    <van-notice-bar color="#1989fa" background="#ecf9ff" left-icon="volume-o" :scrollable="true">
      <van-swipe vertical class="notice-swipe" :autoplay="10000" :touchable="false" :show-indicators="false">
        <van-swipe-item v-for="item in noticeList" :key="item.noticeId" @click="handleViewNotice(item)">
          {{ item.noticeContent }}
        </van-swipe-item>
      </van-swipe>
    </van-notice-bar>

    <div class="my-4 w-full flex items-center rounded-lg bg-white px-4 py-3 shadow-lg">
      <div class="flex">
        <img src="@/assets/images/Frame.jpg" alt="" class="h-16 w-16">
      </div>
      <div class="ml-3 flex-1">
        <p class="m-0 mb-1 text-lg font-bold">
          Hi！{{ userStore.userInfo.studentName }} - {{ userStore.userInfo.studentNum }}
        </p>
        <p class="m-0 flex flex-row items-center text-sm text-gray-500">
          联系方式：{{ userStore.userInfo.studentPhone }}
        </p>
      </div>
    </div>
    <div class="mt-7 flex flex-col gap-3">
      <div class="flex flex-row items-center justify-between">
        <p class="my-0 text-xl font-bold">
          考勤签到
        </p>
        <div class="flex flex-row text-gray-500" @click="changeTab(1)">
          查看更多记录
          <Icon icon="mingcute:right-line" class="text-lg" />
        </div>
      </div>
      <div class="my-0 flex flex-row">
        <div class="flex flex-row gap-2">
          <template v-if="signList.length === 0">
            <p class="my-0 text-sm text-gray-500">
              您没有需要签到的记录
            </p>
          </template>
          <div v-for="item in signList" v-else :key="item.signId" class="flex rounded-2 bg-white p-3 shadow shadow-md" @click="handleSign(item)">
            <div class="flex flex-col">
              <div class="text-[12px] text-gray-700">
                {{ item.issueTime }}
              </div>
              <div class="mt-2 text-[12] text-gray-500">
                {{ getSignStatusText(item) }}
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>
    <div class="mt-7 flex flex-col gap-3">
      <p class="my-0 text-xl font-bold">
        提交申请
      </p>
      <p class="my-0 text-sm text-gray-500">
        如果您的宿舍需要维修或您想申请床位调换，请点击下方按钮提交申请。我们会尽快处理您的请求。
      </p>
      <div class="btn mt-3 shadow-black shadow-md" @click="handleApply()">
        提交申请
      </div>
    </div>
    <Teleport to="body">
      <van-action-sheet
        v-model:show="showActions" description="请选择申请类型" :actions="actions" @select="onActionSelect"
      />
      <van-action-sheet v-model:show="showNotice" :title="notice.noticeTitle">
        <div class="min-h-60 px-3">
          <div class="flex flex-row items-center justify-between text-sm text-gray-500">
            <span>
              发布时间：{{ notice.noticeTime }}
            </span>
            <span>
              发布人：{{ notice.createName }}
            </span>
          </div>
          <div class="mt-2 text-base text-gray-900">
            {{ notice.noticeContent }}
          </div>
        </div>
      </van-action-sheet>
    </Teleport>
  </div>
</template>

<style lang="scss" scoped>
.notice-swipe {
  height: 40px;
  line-height: 40px;
}

.btn {
  width: 100%;
  padding: 12px;
  font-size: 1.2rem;
  line-height: 1.8rem;
  color: #fff;
  text-align: center;
  // 线性渐变
  background: linear-gradient(90deg, #6e9bff 0%, #3071ff 100%);
  border-radius: 1.5rem;
}
</style>
