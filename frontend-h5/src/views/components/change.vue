<script lang="ts" setup>
import { reactive, ref } from 'vue'
import type { CascaderOption, PickerInstance } from 'vant'
import { closeToast, showFailToast, showLoadingToast, showSuccessToast } from 'vant'
import useUserStore from '@/store/modules/user'
import ApplyApi from '@/api/modules/apply'
import infoListApi from '@/api/modules/info'

const formData = reactive({
  applyReason: '',
  buildingId: 0, buildingName: '',
  dormitoryId: 0, dormitoryName: '',
  bedId: 0, bedName: '',
  inBuildingId: 0, inBuildingName: '',
  inDormitoryId: 0, inDormitoryName: '',
  inBedId: 0, inBedName: '',
  studentId: 0,
})
const disabled = ref(false)
const userInfo = computed(() => useUserStore().userInfo)
function handleSubmit() {
  if (!formData.buildingName || !formData.dormitoryName || !formData.bedName || !formData.inBuildingName || !formData.inDormitoryName || !formData.inBedName) {
    showFailToast('请填写完整信息')
  }
  else if (!formData.applyReason) {
    showFailToast('请输入申请原因')
  }
  else {
    showLoadingToast({
      message: '加载中...',
      forbidClick: true,
      loadingType: 'spinner',
    })
    ApplyApi.change(formData).then((res) => {
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

// #region 选择器
const columns = ref<{
  name: string
  value: number
  children?: {
    name: string
    value: number
    children?: {
      name: string
      value: number
    }[]
  }[]
}[]>([
  // {
  //   buildingName: '浙江',
  //   buildingId: 123,
  //   children: [
  //     {
  //       cityName: '杭州',
  //       cities: [{ cityName: '西湖区' }, { cityName: '余杭区' }],
  //     },
  //     {
  //       cityName: '温州',
  //       cities: [{ cityName: '鹿城区' }, { cityName: '瓯海区' }],
  //     },
  //   ],
  // },
])

const showPicker = ref(false)
const selectType = ref<'old' | 'in'>('old')
const cascaderValue = ref('')

function showSelect(name: 'old' | 'in') {
  showPicker.value = true
  selectType.value = name
  cascaderValue.value = ''
}

async function initColumns() {
  const buildingList = await infoListApi.buildingList({})
  columns.value = buildingList.data.map((item) => {
    return {
      ...item,
      name: item.buildingName,
      value: item.buildingId,
      children: [],
    }
  })
  if (buildingList.data.length === 0) {
    return
  }
  const dormitoryList = await infoListApi.dormitoryList({ buildingId: buildingList.data[0].buildingId })
  columns.value[0].children = dormitoryList.data.map((item) => {
    return {
      name: item.dormitoryName,
      value: item.dormitoryId,
      children: [],
    }
  })
  if (dormitoryList.data.length === 0) {
    return
  }
  const bedList = await infoListApi.bedList({ dormitoryId: dormitoryList.data[0].dormitoryId })
  columns.value[0].children[0].children = bedList.data.map((item) => {
    return {
      ...item,
      name: item.bedName,
      value: item.bedId,
    }
  })
  console.log('🚀 ~ columns.value:', columns.value)
}

function onChange({ value, selectedOptions, tabIndex }: { value: string | number, tabIndex: number, selectedOptions: CascaderOption[] }) {
  if (tabIndex === 0) {
    const selectBuild = columns.value.find(item => item.value === value)
    if (selectBuild && selectBuild.children?.length === 0) {
      showLoadingToast('加载中...')
      infoListApi.dormitoryList({ buildingId: value }).then((res) => {
        const buildIndex = columns.value.findIndex(item => item.value === value)
        columns.value[buildIndex].children = res.data.map((item) => {
          return {
            name: item.dormitoryName,
            value: item.dormitoryId,
            children: [],
          }
        })
      }).finally(() => {
        closeToast()
      })
    }
  }
  else if (tabIndex === 1) {
    if (selectedOptions[1].children?.length === 0) {
      showLoadingToast('加载中...')
      infoListApi.bedList({ dormitoryId: value }).then((res) => {
        const buildIndex = columns.value.findIndex(item => item.value === selectedOptions[0].value)
        const dormIndex = columns.value[buildIndex].children!.findIndex(item => item.value === value)
        columns.value[buildIndex].children![dormIndex].children = res.data.map((item) => {
          return {
            ...item,
            name: item.bedName,
            value: item.bedId,
          }
        })
      }).finally(() => {
        closeToast()
      })
    }
  }
}

function onFinish({ selectedOptions }: { selectedOptions: CascaderOption[] }) {
  showPicker.value = false
  if (selectType.value === 'old') {
    formData.buildingName = selectedOptions[0].name
    formData.buildingId = Number(selectedOptions[0].value) || 0
    formData.dormitoryName = selectedOptions[1].name
    formData.dormitoryId = Number(selectedOptions[1].value) || 0
    formData.bedName = selectedOptions[2].name
    formData.bedId = Number(selectedOptions[2].value) || 0
  }
  else {
    formData.inBuildingName = selectedOptions[0].name
    formData.inBuildingId = Number(selectedOptions[0].value) || 0
    formData.inDormitoryName = selectedOptions[1].name
    formData.inDormitoryId = Number(selectedOptions[1].value) || 0
    formData.inBedName = selectedOptions[2].name
    formData.inBedId = Number(selectedOptions[2].value) || 0
  }
}

onMounted(() => {
  initColumns()
})
// #endregion

onMounted(() => {
  console.log('🚀 ~ userInfo:', userInfo)
  formData.buildingId = userInfo.value.buildingId
  formData.buildingName = userInfo.value.buildingName
  formData.dormitoryId = userInfo.value.dormitoryId
  formData.dormitoryName = userInfo.value.dormitoryName
  formData.bedId = userInfo.value.bedId
  formData.bedName = userInfo.value.bedName
  formData.studentId = userInfo.value.studentId
  // if (!userInfo.value.buildingId) {
  //   showFailToast('请先完善宿舍信息')
  //   disabled.value = true
  // }
  // if (!userInfo.value.dormitoryId) {
  //   showFailToast('请先完善宿舍信息')
  //   disabled.value = true
  // }
  // if (!userInfo.value.bedId) {
  //   showFailToast('请先完善宿舍信息')
  //   disabled.value = true
  // }
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
          <label for="applyReason" class="mb-1 block text-sm font-medium">申请原因</label>
          <textarea id="applyReason" v-model="formData.applyReason" placeholder="请详细描述申请原因(100字以内)" maxlength="100" class="h-20 w-full border-gray-300 rounded border-none bg-[#EBEDED] p-2" :disabled="disabled" />
        </div>
        <div @click="showSelect('old')">
          <label for="buildingName" class="mb-1 block text-sm font-medium">原楼栋</label>
          <input id="buildingName" v-model="formData.buildingName" readonly type="text" maxlength="20" class="w-full border-gray-300 rounded border-none bg-[#EBEDED] p-2" :disabled="disabled" @click="showSelect('old')">
        </div>
        <div @click="showSelect('old')">
          <label for="dormitoryName" class="mb-1 block text-sm font-medium">原宿舍</label>
          <input id="dormitoryName" v-model="formData.dormitoryName" readonly type="text" maxlength="20" class="w-full border-gray-300 rounded border-none bg-[#EBEDED] p-2" :disabled="disabled" @click="showSelect('old')">
        </div>
        <div @click="showSelect('old')">
          <label for="bedName" class="mb-1 block text-sm font-medium">原床位</label>
          <input id="bedName" v-model="formData.bedName" readonly type="text" maxlength="20" class="w-full border-gray-300 rounded border-none bg-[#EBEDED] p-2" :disabled="disabled" @click="showSelect('old')">
        </div>
        <div @click="showSelect('in')">
          <label for="inBuildingName" class="mb-1 block text-sm font-medium">入住楼栋</label>
          <input id="inBuildingName" v-model="formData.inBuildingName" readonly type="text" maxlength="20" class="w-full border-gray-300 rounded border-none bg-[#EBEDED] p-2" :disabled="disabled" @click="showSelect('in')">
        </div>
        <div @click="showSelect('in')">
          <label for="inDormitoryName" class="mb-1 block text-sm font-medium">入住宿舍</label>
          <input id="inDormitoryName" v-model="formData.inDormitoryName" readonly type="text" maxlength="20" class="w-full border-gray-300 rounded border-none bg-[#EBEDED] p-2" :disabled="disabled" @click="showSelect('in')">
        </div>
        <div @click="showSelect('in')">
          <label for="inBedName" class="mb-1 block text-sm font-medium">入住床位</label>
          <input id="inBedName" v-model="formData.inBedName" readonly type="text" maxlength="20" class="w-full border-gray-300 rounded border-none bg-[#EBEDED] p-2" :disabled="disabled" @click="showSelect('in')">
        </div>
      </div>
      <Teleport to="body">
        <van-popup v-model:show="showPicker" round position="bottom">
          <van-cascader
            v-model="cascaderValue"
            title="请选择"
            :options="columns"
            :field-names="{
              text: 'name',
              value: 'value',
              children: 'children',
            }"
            @close="showPicker = false"
            @change="onChange"
            @finish="onFinish"
          />
        </van-popup>
      </Teleport>
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
