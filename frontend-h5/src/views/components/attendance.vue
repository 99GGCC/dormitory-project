<script lang="ts" setup>
import { ref } from 'vue'
import signApi from '@/api/modules/sign'

const activeNames = ref([])
const list = ref<any[]>([])
const loading = ref(false)
const finished = ref(false)
const page = ref(0)
function refresh() {
  page.value += 1
  signApi.getSignList({ limit: 10, page: page.value }).then((res) => {
    list.value.push(...res.data.records)
    loading.value = false
    if (res.data.records.length < 10) {
      finished.value = true
    }
    console.log('🚀 ~ list.value:', list.value)
  }).finally(() => {
    loading.value = false
  })
}
const signMine = computed(() => {
  return {
    0: list.value.filter(item => +item.recordStatus === 0).length,
    1: list.value.filter(item => +item.recordStatus === 1).length,
  }
})
function getSignStatusText(sign: { signInStatus: number, endTime: string | number | Date, recordStatus: number }) {
  // 当前时间大于截止时间，状态是未签到的话，就是已过期
  if (sign.signInStatus === 0 && new Date() > new Date(sign.endTime)) {
    return '已过期'
  }
  // 如果是1，并且当前时间大于截止日期 endTime，就是已截止
  if (sign.signInStatus === 1 && new Date() > new Date(sign.endTime)) {
    return '已截止'
  }
  return sign.recordStatus === 1 ? '已签到' : '待签到'
}
</script>

<template>
  <div class="mt-4 w-full flex flex-col gap-3">
    <!-- 考勤统计 -->
    <div class="flex flex-row gap-2">
      <div class="flex flex-1 rounded-2 bg-white p-3 shadow shadow-md">
        <div class="flex flex-col">
          <div class="text-[12px] text-gray-700">
            累计签到
          </div>
          <div class="mt-2 text-[12] text-gray-500">
            {{ signMine[0] }}次
          </div>
        </div>
      </div>
      <div class="flex flex-1 rounded-2 bg-white p-3 shadow shadow-md">
        <div class="flex flex-col">
          <div class="text-[12px] text-gray-700">
            累计缺勤
          </div>
          <div class="mt-2 text-[12] text-gray-500">
            {{ signMine[1] }}次
          </div>
        </div>
      </div>
    </div>

    <van-list
      v-model:loading="loading"
      :finished="finished"
      finished-text="没有更多了"
      class="w-full"
      @load="refresh"
    >
      <template v-if="!list.length">
        <van-empty description="暂无数据" />
      </template>
      <van-collapse v-else v-model="activeNames" class="w-full">
        <van-collapse-item v-for="item in list" :key="item.recordId" :title="item.issueTime" :name="item.recordId">
          <div class="flex flex-row items-start gap-4">
            <div class="mr-5 flex flex-col items-center">
              <div class="text-[12px] text-gray-700">
                签到率
              </div>
              <div class="mt-2 text-[12] text-gray-500">
                <van-circle
                  :current-rate="
                    Number(((item.realityStudent / item.totalStudent) * 100).toFixed(0))"
                  :rate="100"
                  :speed="100"
                  size="80px"
                  :text="`${((item.realityStudent / item.totalStudent) * 100).toFixed(0)}%`"
                />
              </div>
            </div>
            <div class="flex flex-col">
              <div class="text-[12px] text-gray-700">
                总人数
              </div>
              <div class="mt-2 text-[12] text-gray-500">
                {{ item.totalStudent }}人
              </div>
            </div>
            <div class="flex flex-col">
              <div class="text-[12px] text-gray-700">
                已签到
              </div>
              <div class="mt-2 text-[12] text-gray-500">
                {{ item.realityStudent }}人
              </div>
            </div>
            <div class="flex flex-col">
              <div class="text-[12px] text-gray-700">
                未签到
              </div>
              <div class="mt-2 text-[12] text-gray-500">
                {{ item.totalStudent - item.realityStudent }}人
              </div>
            </div>
            <div class="flex flex-col">
              <div class="text-[12px] text-gray-700">
                签到状态
              </div>
              <div class="mt-2 text-[12] text-gray-500">
                {{ getSignStatusText(item) }}
              </div>
            </div>
          </div>
        </van-collapse-item>
      </van-collapse>
    </van-list>
  </div>
</template>
<!--
<style lang="scss" scoped>

</style> -->
