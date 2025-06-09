<script lang="ts" setup>
import { useConfig } from './config'
import OrderDetail from './component/order-detail.vue'
import API from '@/api/modules/order/order'
import type { BtnOptType } from '@/types/global'
import { confirmMsg, successMsg, warningMsg } from '@/utils/message'

type CBtnOptType = 'evaluation' | BtnOptType

defineOptions({
  name: 'Order',
})

const xGrid = ref()

// 业务主键
const keyId = 'orderId'
const { gridOptions } = useConfig(API)
const orderDetailRef = ref()

function handleOperate(type: CBtnOptType, data: any = {}) {
  if (type === 'handle') {
    confirmMsg('开始订单').then(() => {
      API.handle(data[keyId]).then((res) => {
        console.log('🚀 ~ res:', res)
        successMsg('开始订单成功')
        xGrid.value.commitProxy('reload')
      })
    })
  }
  if (type === 'view') {
    API.info(data[keyId]).then((res) => {
      if (res.data) {
        if (res.data.orderStatus === 4) {
          API.evaluationList(data[keyId]).then((res2) => {
            orderDetailRef.value.open('view', res.data.orderItemList, res2.data)
          }).catch(() => warningMsg('获取评价失败'))
        }
        else {
          orderDetailRef.value.open('view', res.data.orderItemList)
        }
      }
    }).catch(() => warningMsg('获取详情失败'))
  }
}
</script>

<!--
  0: '已取消',
            1: '已下单',
            2: '进行中',
            3: '已完成',
            4: '已评价',
 -->
<template>
  <PageMainFull>
    <vxe-grid ref="xGrid" v-bind="gridOptions">
      <template #operate="{ row }">
        <vxe-button content="详情" @click="handleOperate('view', row)" />
        <vxe-button v-if="+row.orderStatus === 1" content="处理" @click="handleOperate('handle', row)" />
      </template>
    </vxe-grid>
    <OrderDetail ref="orderDetailRef" />
  </PageMainFull>
</template>
