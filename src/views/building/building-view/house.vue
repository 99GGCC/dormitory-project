<script lang="ts" setup>
import type { PropType } from 'vue'
import { ref } from 'vue'
import { el } from 'element-plus/es/locale/index.mjs'
import { ElTable } from 'element-plus'
import type { Dormitory } from './types'
import BedEditDialog from './bedEditDialog.vue'
import BedAddDialog from './bedAddDialog.vue'
import BedCheckInDialog from './bedCheckInDialog.vue'
import DormitoreApi from '@/api/modules/building/dormitory'
import BED_API from '@/api/modules/building/bed'
import { successMsg, warningMsg } from '@/utils/message'

const props = defineProps({
  value: {
    type: Object as PropType<Record<number, any[]>>,
    default: () => null,
  },
  buildLoading: {
    type: Boolean,
    default: false,
  },
})

const dormitory = ref<Dormitory>({})
watch(() => props.value, () => {
  dormitory.value = {}
})
const dormitoryLoading = ref(false)
function handleView(room: { dormitoryId: number }) {
  dormitoryLoading.value = true
  console.log('🚀 ~ room:', room)
  if (room.dormitoryId) {
    DormitoreApi.info(room.dormitoryId).then((res) => {
      console.log('🚀 ~ res:', res)
      dormitory.value = { ...res.data, bedInfoList: [] }
      refreshBedList(room.dormitoryId)
    }).finally(() => {
      dormitoryLoading.value = false
    })
  }
}

function refreshBedList(dormitoryId?: number) {
  if (!dormitoryId) {
    return
  }
  BED_API.list({ dormitoryId }).then((res) => {
    if (res.data) {
      dormitory.value.bedInfoList = res.data
    }
    else {
      dormitory.value.bedInfoList = []
    }
  })
}

const bedInfoTableRef = ref<InstanceType<typeof ElTable>>()
// const bedEditDialogRef = ref<InstanceType<typeof BedEditDialog> | null>(null)
const bedAddDialogRef = ref<InstanceType<typeof BedAddDialog> | null>(null)
const bedCheckInDialogRef = ref<InstanceType<typeof BedCheckInDialog> | null>(null)
// const tableLoading = ref(false)
function handleOpt(type: string, row?: any) {
  console.log('🚀 ~ row:', row)
  console.log('🚀 ~ type:', type)
  if (type === 'add') {
    // 添加床位
    if (bedAddDialogRef.value) {
      bedAddDialogRef.value.open('add', dormitory.value)
    }
  }
  else if (type === 'edit') {
    // 编辑
    if (bedAddDialogRef.value) {
      bedAddDialogRef.value.open('edit', row)
    }
  }
  else if (type === 'checkIn') {
    // 入住
    if (bedCheckInDialogRef.value) {
      bedCheckInDialogRef.value.open('add', row)
    }
  }
  else if (type === 'release') {
    // 释放
    dormitoryLoading.value = true
    BED_API.release({
      bedId: row.bedId,
      relocationType: 4,
    }).then((res) => {
      if (res.data) {
        successMsg('操作成功')
        handleView({ dormitoryId: dormitory.value.dormitoryId! })
      }
    }).finally(() => {
      dormitoryLoading.value = false
    })
  }
  else if (type === 'delete') {
    // 删除
    const selection = bedInfoTableRef.value?.getSelectionRows()
    console.log('🚀 ~ selection:', selection)
    if (selection.length === 0) {
      return
    }
    dormitoryLoading.value = true
    const promiseList = []
    for (let i = 0; i < selection.length; i++) {
      if (!selection[i].studentId) {
        promiseList.push(BED_API.delete(selection[i].bedId))
      }
    }
    Promise.all(promiseList).then(() => {
      successMsg('操作成功')
      refreshBedList(dormitory.value.dormitoryId)
      if (promiseList.length !== selection.length) {
        warningMsg('部分床位已入住,请重新选择')
      }
    }).finally(() => {
      dormitoryLoading.value = false
    })
  }
}

onMounted(() => {
})
</script>

<template>
  <div class="w-full flex flex-row">
    <div v-loading="buildLoading" class="house">
      <template v-if="props.value && Object.keys(props.value).length > 0">
        <div v-for="(floor, key) in props.value" :key="`floor${key}`" class="house-floor">
          <div class="flex flex-row items-center">
            <div class="floor-lable">
              <el-tag>
                {{ key }} 楼
              </el-tag>
            </div>
            <div class="rooms flex flex-row">
              <div
                v-for="room in floor" :key="`room${room.dormitoryId}`" class="room"
                :class="{ active: dormitory.dormitoryId === room.dormitoryId }" @click="handleView(room)"
              >
                {{ room.dormitoryName }}
              </div>
            </div>
          </div>
        </div>
      </template>
      <div v-else class="h-full">
        <el-empty description="请选择楼栋" />
      </div>
    </div>
    <div v-loading="dormitoryLoading" class="house-detail">
      <template v-if="dormitory.dormitoryId">
        <el-form :model="dormitory" label-width="auto">
          <el-form-item label="楼层">
            {{ dormitory.buildingFloor }}
          </el-form-item>
          <el-form-item label="房间名">
            <el-input v-model="dormitory.dormitoryName" />
          </el-form-item>
          <el-form-item label="宿舍状态">
            <el-select v-model="dormitory.dormitoryStatus" placeholder="请选择">
              <el-option label="启用" :value="1" />
              <el-option label="禁用" :value="0" />
            </el-select>
          </el-form-item>
          <el-form-item label="使用状态">
            <el-select v-model="dormitory.useStatus" placeholder="请选择">
              <el-option label="已使用" :value="1" />
              <el-option label="未使用" :value="0" />
            </el-select>
          </el-form-item>
          <el-form-item label="床位信息">
            <ElTable ref="bedInfoTableRef" :data="dormitory.bedInfoList" style="width: 100%;" empty-text="暂无床位信息,请添加">
              <el-table-column type="selection" width="55" />
              <el-table-column prop="bedName" label="床位" />
              <el-table-column prop="studentName" label="使用人" width="200">
                <template #default="{ row }">
                  <div v-if="row.studentName && row.studentId" class="flex flex-row gap-1">
                    <el-tag type="success">
                      {{ row.studentName }}
                    </el-tag>
                    <el-tag v-if="row.isHead" type="info">
                      宿舍长
                    </el-tag>
                    <el-button type="primary" size="small" @click="handleOpt('release', row)">
                      释放
                    </el-button>
                  </div>
                  <div v-else class="flex flex-row gap-1">
                    <el-tag type="info">
                      空床位
                    </el-tag>
                    <el-button
                      v-if="row.bedStatus === 1" type="primary" size="small" @click="handleOpt('checkIn', row)"
                    >
                      入住
                    </el-button>
                  </div>
                </template>
              </el-table-column>
              <el-table-column prop="bedStatus" label="状态" width="80">
                <template #default="{ row }">
                  <el-tag v-if="row.bedStatus !== undefined" :type="row.bedStatus === 1 ? 'success' : 'danger'">
                    {{ row.bedStatus === 1 ? '启用 ' : '禁用' }}
                  </el-tag>
                </template>
              </el-table-column>
              <el-table-column prop="operation" label="" width="90">
                <template #default="{ row }">
                  <el-button type="default" size="small" @click="handleOpt('edit', row)">
                    编辑
                  </el-button>
                </template>
              </el-table-column>
            </ElTable>
            <div class="flex flex-row gap-1">
              <el-button type="primary" size="small" @click="handleOpt('add')">
                添加床位
              </el-button>
              <el-button type="danger" size="small" @click="handleOpt('delete')">
                删除
              </el-button>
            </div>
          </el-form-item>
        </el-form>
      </template>
      <el-empty v-else description="请选择房间" />
    </div>
    <!-- <BedEditDialog ref="bedEditDialogRef" /> -->
    <BedAddDialog ref="bedAddDialogRef" @on-submit="() => handleView({ dormitoryId: dormitory.dormitoryId! })" />
    <BedCheckInDialog ref="bedCheckInDialogRef" @on-submit="() => handleView({ dormitoryId: dormitory.dormitoryId! })" />
  </div>
</template>

<style lang="scss" src="./house.scss" scoped></style>
