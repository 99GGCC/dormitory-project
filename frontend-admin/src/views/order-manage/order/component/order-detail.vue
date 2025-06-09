<script lang="ts" setup>
import { ref } from 'vue'

const show = ref(false)

const tableData = ref([])
const evaluationData = ref({
  evaluationGrade: 0,
  evaluationContent: '',
})

function cancelEvent() {
  show.value = false
}

function open(type: string, tableValue: any, evaluation: any = {}) {
  tableData.value = tableValue
  evaluationData.value = evaluation
  show.value = true
}

defineExpose({
  open,
})
</script>

<template>
  <div>
    <vxe-modal v-model="show" width="600" show-footer title="订单详情">
      <template #default>
        <vxe-table
          show-overflow
          auto-resize
          height="300"
          :row-config="{ height: 60 }"
          :data="tableData"
        >
          <vxe-column field="commodityImg" title="商品图片">
            <template #default="{ row }">
              <ImagePreview :src="row.commodityImg" :width="50" :height="50" />
            </template>
          </vxe-column>
          <vxe-column field="commodityName" title="商品名称" />
          <vxe-column field="commodityPrice" title="单价" />
          <vxe-column field="itemNum" title="数量" />
          <vxe-column field="itemSubtotal" title="总计" />
        </vxe-table>
        <div v-if="evaluationData.evaluationGrade > 0" class="mt-4 flex flex-row items-center justify-center text-2xl">
          <vxe-icon v-for="item in evaluationData.evaluationGrade" :key="item" name="star" color="#f5a623" />
          <vxe-icon v-for="item in 5 - evaluationData.evaluationGrade" :key="item" name="star" color="#ccc" />
          <span class="ml-2">{{ evaluationData.evaluationContent }}</span>
        </div>
      </template>
      <template #footer>
        <vxe-button @click="cancelEvent">
          关闭
        </vxe-button>
      </template>
    </vxe-modal>
  </div>
</template>
