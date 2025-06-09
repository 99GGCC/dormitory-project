<script setup lang="ts">
import { showConfirmDialog, showFailToast, showSuccessToast } from 'vant'
import EditEmailPhone from '../components/editEmailPhone.vue'
import EditPassword from '../components/editPassword.vue'
import useUserStore from '@/store/modules/user'
import historyApi from '@/api/modules/history'
import applyApi from '@/api/modules/apply'
import { isEmail, isPhoneNumber } from '@/utils/is'

definePage({
  meta: {
    title: '个人中心',
    auth: true,
  },
})

const userStore = useUserStore()

const userInfo = computed(() => userStore.userInfo)

const repairList = ref<any[]>([])

function refreshRepair() {
  historyApi.getRepairList({ page: 1, limit: 99 }).then((res) => {
    repairList.value = res.data.records
  })
}

const changeList = ref<any[]>([])

function refreshChange() {
  historyApi.getExchangeList({ page: 1, limit: 99 }).then((res) => {
    changeList.value = res.data.records
  })
}

function handleLogout() {
  userStore.logout()
}

const showPopup = ref(false)
const editType = ref('phoneEmail')

function handleEditUser(type: 'phoneEmail' | 'pass') {
  showPopup.value = true
  editType.value = type
}

function handleCancel(item: any) {
  if (item.changeId || item.repairId) {
    const id = item.changeId || item.repairId
    const isChange = !!item.changeId
    const title = isChange ? '取消调换' : '取消维修'
    const message = isChange ? '确定要取消该调换申请吗？' : '确定要取消该维修申请吗？'
    const api = isChange ? applyApi.cancelChange : applyApi.cancelRepair
    showConfirmDialog({
      theme: 'round-button',
      title,
      message,
      confirmButtonText: '确认',
      cancelButtonText: '再想想',
      beforeClose: action =>
        new Promise((resolve) => {
          if (action === 'confirm') {
            api(id).then(() => {
              refreshRepair()
              refreshChange()
              resolve(true)
              setTimeout(() => {
                showSuccessToast('取消成功')
              }, 1000)
            }).catch(() => {
              showFailToast('取消失败')
              resolve(true)
            })
          }
          else {
            resolve(true)
          }
        }),
    })
  }
}

onMounted(() => {
  refreshRepair()
  refreshChange()
})
</script>

<template>
  <PageLayout :navbar="false" tabbar>
    <div class="flex flex-col items-center p-4 text-gray-800">
      <!-- Profile Section -->
      <div class="mb-6 flex flex-col items-center">
        <img src="@/assets/images/Frame.jpg" alt="Profile" class="mb-2 h-24 w-24 rounded-full">
        <div class="text-lg font-bold">
          {{ userInfo.studentName }}
        </div>
        <div class="text-sm text-gray-500">
          学号：{{ userInfo.studentNum }}
        </div>
      </div>

      <!-- Detailed Information -->
      <div class="mb-5 w-full">
        <h2 class="mb-2 text-base font-semibold">
          详细信息
        </h2>
        <ul class="text-sm space-y-1">
          <!-- <li><span class="font-medium">姓名：</span>{{ userInfo.studentName }}</li>
          <li><span class="font-medium">学号：</span>{{ userInfo.studentNum }}</li> -->
          <li><span class="font-medium">学院：</span>{{ userInfo.collegeName }}</li>
          <li><span class="font-medium">专业：</span>{{ userInfo.majorName }}</li>
          <li><span class="font-medium">班级：</span>{{ userInfo.classesName }}</li>
          <li class="flex flex-row items-center gap-1" @click="handleEditUser('phoneEmail')">
            <span class="font-medium">手机：</span>{{ userInfo.studentPhone }}
            <Icon icon="material-symbols:edit-outline" />
          </li>
          <li class="flex flex-row items-center gap-1" @click="handleEditUser('phoneEmail')">
            <span class="font-medium">邮箱：</span>{{ userInfo.studentEmail }}
            <Icon icon="material-symbols:edit-outline" />
          </li>
        </ul>
      </div>

      <!-- Repair and Adjustment History -->
      <div class="w-full">
        <van-tabs>
          <van-tab title="维修历史">
            <div class="max-h-100 min-h-30 overflow-y-auto space-y-4">
              <template v-if="!repairList.length">
                <van-empty description="暂无记录" image-size="100" />
              </template>
              <template v-else>
                <div
                  v-for="item in repairList" :key="item.repairId"
                  class="flex cursor-pointer items-center border rounded p-3 shadow-sm"
                >
                  <div class="mr-3 flex rounded bg-[#EBEDED] p-3">
                    <Icon icon="wpf:maintenance" />
                  </div>
                  <div class="flex-1">
                    <div class="flex flex-row justify-between text-sm font-medium">
                      <span> 维修 </span>
                      <span class="text-xs text-gray-500">
                        <!-- 0 已申请 1 处理中 2 已处理 3 无需处理 4 取消处理 -->
                        <template v-if="item.repairStatus === 0">
                          <div class="flex flex-row gap-1">
                            <van-tag type="warning">已申请</van-tag>
                            <van-tag color="var(--van-tag-danger-color)" @click.stop="handleCancel(item)">取消</van-tag>
                          </div>
                        </template>
                        <template v-if="item.repairStatus === 1">
                          <van-tag type="primary">处理中</van-tag>
                        </template>
                        <template v-if="item.repairStatus === 2">
                          <van-tag type="success">已处理</van-tag>
                        </template>
                        <template v-if="item.repairStatus === 3">
                          <van-tag type="warning">无需处理</van-tag>
                        </template>
                        <template v-if="item.repairStatus === 4">
                          <van-tag type="warning">已取消</van-tag>
                        </template>
                      </span>
                    </div>
                    <div class="text-xs text-gray-500">
                      {{ item.applyTime }} - {{ item.repairFacilities }}-{{ item.faultDescription }}
                    </div>
                  </div>
                </div>
              </template>
            </div>
          </van-tab>
          <van-tab title="调换历史">
            <div class="max-h-100 min-h-30 overflow-y-auto space-y-4">
              <template v-if="!changeList.length">
                <van-empty description="暂无记录" image-size="100" />
              </template>
              <template v-else>
                <div
                  v-for="item in changeList" :key="item.changeId"
                  class="flex cursor-pointer items-center border rounded p-3 shadow-sm"
                >
                  <div class="mr-3 flex rounded bg-[#EBEDED] p-3">
                    <Icon icon="tabler:exchange" />
                  </div>
                  <div class="flex-1">
                    <div class="flex flex-row justify-between text-sm font-medium">
                      <span> 调换床位 </span>
                      <span class="text-xs text-gray-500">
                        <template v-if="item.applyStatus === 0">
                          <div class="flex flex-row gap-1">
                            <van-tag type="warning">已申请</van-tag>
                            <van-tag color="var(--van-tag-danger-color)" @click.stop="handleCancel(item)">取消</van-tag>
                          </div>
                        </template>
                        <template v-if="item.applyStatus === 1">
                          <van-tag type="success">通过申请</van-tag>
                        </template>
                        <template v-if="item.applyStatus === 2">
                          <van-tag type="danger">拒绝申请</van-tag>
                        </template>
                        <template v-if="item.applyStatus === 3">
                          <van-tag type="warning">取消申请</van-tag>
                        </template>
                      </span>
                    </div>
                    <div class="text-xs text-gray-500">
                      {{ item.applyTime }} - 更换至{{ item.inBuildingName }}-{{ item.inDormitoryName }}-{{
                        item.inBedName
                      }}
                    </div>
                  </div>
                </div>
              </template>
            </div>
          </van-tab>
        </van-tabs>
        <h2 class="mb-2 text-base font-semibold">
          维修和调换历史
        </h2>
      </div>
      <div class="mt-4 w-full flex flex-row gap-2">
        <HButton block class="flex-1" @click="handleLogout">
          退出登录
        </HButton>
        <HButton @click="handleEditUser('pass')">
          修改密码
        </HButton>
      </div>
      <van-popup v-model:show="showPopup" round position="bottom" :style="{ minHeight: '40vh' }">
        <EditEmailPhone v-if="editType === 'phoneEmail'" @on-success="showPopup = false" />
        <EditPassword v-if="editType === 'pass'" @on-success="showPopup = false" />
      </van-popup>
    </div>
  </PageLayout>
</template>
