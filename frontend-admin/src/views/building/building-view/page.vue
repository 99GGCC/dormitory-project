<script lang="ts" setup>
import { onMounted } from 'vue'
import { ElMessage } from 'element-plus'
import House from './house.vue'
import { confirmMsg, successMsg, warningMsg } from '@/utils/message'
import BUILD_API from '@/api/modules/building/info'
import DORMITORY_API from '@/api/modules/building/dormitory'
import BED_API from '@/api/modules/building/bed'

defineOptions({
  name: 'BuildingView',
})

const value = ref('')
const buildingOptions = ref<any[]>([])
const buildFloorValues = ref<Record<number, any[]>>()
function refreshBuildingOptions() {
  BUILD_API.list().then((res) => {
    buildingOptions.value = res.data
  })
}

function handleClear() {
    buildFloorValues.value = {}
}

const buildLoading = ref(false)
function handleSelect(value: number) {
  buildLoading.value = true
  buildFloorValues.value = {}
  DORMITORY_API.floorList(value).then((res) => {
    buildFloorValues.value = res.data
  }).finally(() => {
    buildLoading.value = false
  })
}

const dialogVisible = ref(false)
const formData = reactive<{
  bedNum: number
  dormitoryIds: number[]
}>({
  bedNum: 0,
  dormitoryIds: [],
})
function handleInit() {
  buildLoading.value = true
  const dormitoryIds: number[] = []
  if (!buildFloorValues.value) {
    return
  }
  Object.keys(buildFloorValues.value).forEach((key) => {
    if (buildFloorValues.value && buildFloorValues.value[+key].length) {
      buildFloorValues.value[+key].forEach((item) => {
        dormitoryIds.push(item.dormitoryId)
      })
    }
  })
  DORMITORY_API.setBed({
    ...formData,
    dormitoryIds,
  }).then((res) => {
    if (res.data) {
      successMsg('初始化成功')
    }
    else {
      warningMsg('初始化失败')
    }
  }).finally(() => {
    dialogVisible.value = false
    buildLoading.value = false
  })
}

onMounted(() => {
  refreshBuildingOptions()
})
</script>

<template>
  <PageMainFull>
    <div class="mx-auto flex flex-row items-center gap-4">
      <div class="flex">
        楼栋选择
      </div>
      <div class="w-60 flex">
        <el-select
          v-model="value"
          clearable
          placeholder="请选择"
          @clear="handleClear"
          @change="handleSelect"
        >
          <el-option
            v-for="item in buildingOptions"
            :key="item.buildingId"
            :label="item.buildingName"
            :value="item.buildingId"
          />
        </el-select>
      </div>
      <div v-if="value" class="w-40">
        <el-button type="primary" size="small" @click="dialogVisible = true">
          初始化
        </el-button>
      </div>
    </div>
    <div class="mt-6 flex flex-row px-6">
      <!-- loudong -->
      <House :build-loading="buildLoading" :value="buildFloorValues" />
      <!-- loudong detail -->
    </div>
    <el-dialog v-model="dialogVisible" title="初始化床位" width="500" draggable>
      <el-form :model="formData" label-width="auto">
        <el-form-item label="床位数">
          <el-input-number v-model="formData.bedNum" :step="1" />
        </el-form-item>
      </el-form>
      <template #footer>
        <div class="dialog-footer">
          <el-button @click="dialogVisible = false">
            取消
          </el-button>
          <el-button v-loading="buildLoading" type="primary" @click="handleInit">
            确认
          </el-button>
        </div>
      </template>
    </el-dialog>
  </PageMainFull>
</template>
