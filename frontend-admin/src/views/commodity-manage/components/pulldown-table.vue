<script lang="ts" setup>
import { useVModel } from '@vueuse/core'
import { onMounted, reactive, ref } from 'vue'
import { confirmMsg, successMsg } from '@/utils/message'

const props = defineProps({
  modelValue: {
    type: Array,
    default: () => [],
  },
  placeholder: {
    type: String,
    default: '请操作',
  },
  disabled: {
    type: Boolean,
    default: false,
  },
})

const emit = defineEmits(['update:modelValue'])

const modelValue = useVModel(props, 'modelValue', emit)

const tableData = ref<any[]>([])
const searchName = ref('')

watch(() => props.modelValue, (val) => {
  tableData.value = val
  searchName.value = `${val.length}项`
}, {
  immediate: true,
  deep: true,
})

const GridRef = ref()
const loading = ref(false)
const tableColumn = ref([
  { field: 'specsName', title: '规格名称',
    editRender: { autofocus: '.vxe-input--inner' },
    slots: { edit: 'specsName_edit' },
  },
  { field: 'commodityPrice', title: '商品价格',
    editRender: { autofocus: '.vxe-input--inner' },
    slots: { edit: 'commodityPrice_edit' },
  },
  { title: '操作', slots: { default: 'operate' } },
])

const pulldownRef = ref()
function focusEvent() {
  const $pulldown = pulldownRef.value
  if ($pulldown) {
    $pulldown.showPanel()
  }
}

function suffixClick() {
  const $pulldown = pulldownRef.value
  if ($pulldown) {
    $pulldown.togglePanel()
  }
}

function hasActiveEditRow(row: any) {
  const $grid = GridRef.value
  if ($grid) {
    return $grid.isEditByRow(row)
  }
  return false
}

function keyupEvent() {
  loading.value = true
  setTimeout(() => {
    loading.value = false
  }, 100)
}

function clearRowEvent() {
  const $grid = GridRef.value
  if ($grid) {
    $grid.clearEdit()
  }
}

async function saveRowEvent() {
  const $grid = GridRef.value
  if ($grid) {
    await $grid.clearEdit()
    loading.value = true
    // 模拟异步保存
    setTimeout(() => {
      loading.value = false
      successMsg('保存成功')
    }, 300)
  }
}

async function removeRowEvent(row: any) {
  const $grid = GridRef.value
  if ($grid) {
    $grid.remove(row)
    tableData.value = $grid.getTableData().fullData
    modelValue.value = tableData.value
  }
}

function handleOpt(type: string, row?: any) {
  const $grid = GridRef.value
  if (type === 'add') {
    tableData.value.push({
      specsName: '',
      commodityPrice: '',
    })
    modelValue.value = tableData.value

    nextTick(() => {
      if ($grid) {
        $grid.setEditRow(tableData.value[tableData.value.length - 1])
      }
    })
  }
  else if (type === 'edit') {
    console.log(row)
    if ($grid) {
      $grid.setEditRow(row)
    }
  }
}
</script>

<template>
  <vxe-pulldown ref="pulldownRef" popup-class-name="my-dropdown4" transfer>
    <template #header>
      <div class="my-headdown4">
        <vxe-button v-if="!disabled" mode="text" status="primary" @click="handleOpt('add')">
          新增
        </vxe-button>
      </div>
    </template>

    <template #default>
      <vxe-input
        v-model="searchName" suffix-icon="vxe-icon-table" :placeholder="props.placeholder" @focus="focusEvent"
        @suffix-click="suffixClick" @keyup="keyupEvent"
      />
    </template>

    <template #dropdown>
      <div class="my-bodydown4">
        <vxe-grid
          ref="GridRef"
          auto-resize border
          height="auto"
          :row-config="{ isHover: true }"
          :loading="loading"
          :data="tableData"
          :columns="tableColumn"
          :pager-config="{ enabled: false }"
          :keep-source="true"
          :edit-config="{
            trigger: 'manual',
            mode: 'row',
            showStatus: true,
          }"
        >
          <template #specsName_edit="{ row }">
            <vxe-input v-model="row.specsName" />
          </template>
          <template #commodityPrice_edit="{ row }">
            <vxe-input v-model="row.commodityPrice" />
          </template>
          <template #operate="{ row }">
            <template v-if="!disabled">
              <template v-if="hasActiveEditRow(row)">
                <vxe-button size="mini" content="取消" @click="clearRowEvent" />
                <vxe-button size="mini" status="primary" content="保存" @click="saveRowEvent()" />
              </template>
              <template v-else>
                <vxe-button size="mini" mode="text" status="primary" content="编辑" @click="handleOpt('edit', row)" />
              </template>
              <vxe-button size="mini" status="danger" content="删除" @click="removeRowEvent(row)" />
            </template>
          </template>
        </vxe-grid>
      </div>
    </template>
  </vxe-pulldown>
</template>

<style lang="scss" scoped>
.my-dropdown4 {
  background-color: #fff;
  box-shadow: 0 0 6px 2px rgb(0 0 0 / 10%);

  .my-bodydown4 {
    width: 600px;
    height: 300px;
  }

  .my-footdown4 {
    padding: 5px 0;
    text-align: center;
    border-top: 1px solid #e8eaec;
  }
}
</style>
