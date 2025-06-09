import type { VxeGridProps } from 'vxe-table'
import { createRequiredValidateRule, createVxeFormItem } from '@/utils/vxe-utils'
import ImagePreview from '@/components/ImagePreview/index.vue'

interface RowVO {
  [key: string]: any
}
export function useConfig(API: {
  page: (params: any) => Promise<any>
}) {
  const gridOptions = reactive<VxeGridProps<RowVO>>({
    border: true,
    showOverflow: true,
    height: 'auto',
    exportConfig: {},
    columnConfig: {
      resizable: true,
    },
    pagerConfig: {
      enabled: true,
      pageSize: 20,
      pageSizes: [5, 10, 20, 50, 100],
      autoHidden: false, // 当只有一页时自动隐藏
    },
    formConfig: {
      items: [
        createVxeFormItem('input', {
          field: 'orderNum', title: '订单编号', span: 4,
        }),
        createVxeFormItem('input', {
          field: 'orderStatus', title: '订单状态', span: 4,
        }),
        createVxeFormItem('date', {
          field: 'orderTimeStart', title: '下单日期开始', span: 4,
        }),
        createVxeFormItem('date', {
          field: 'orderTimeEnd', title: '下单日期结束', span: 4,
        }),
        {
          itemRender: {
            name: '$button',
            props: { type: 'submit', content: '查询', status: 'primary' },
          },
        },
        {
          itemRender: {
            name: '$button',
            props: { type: 'reset', content: '重置' },
          },
        },
      ],
    },
    toolbarConfig: {
      custom: true,
      slots: {
        // buttons: 'toolbar_buttons',
      },
    },
    proxyConfig: {
      enabled: true,
      autoLoad: true,
      form: true,
      seq: true, // 启用动态序号代理，每一页的序号会根据当前页数变化
      props: {
        result: 'data.records', // 响应结果中获取数据列表的
        total: 'data.total', // 响应结果中获取数据总数的
      },
      ajax: {
        query: ({ page, form }) => {
          return API.page({
            ...form,
            page: page.currentPage,
            limit: page.pageSize,
          })
        },
      },
    },
    columns: [
      { type: 'seq', width: 60 },
      { field: 'orderNum', title: '订单编号' },
      { field: 'orderStatus', title: '订单状态',
        formatter({ cellValue }) {
          return {
            0: '已取消',
            1: '已下单',
            2: '进行中',
            3: '已完成',
            4: '已评价',
          }[cellValue as string] ?? ''
        } },
      { field: 'userName', title: '用户姓名' },
      { field: 'userAvatar', title: '用户头像',
        slots: {
          default({ row }) {
            return h(ImagePreview, {
              src: row.userAvatar,
              width: 'auto',
              height: 40,
            })
          },
        },
      },
      { field: 'orderTotal', title: '订单总价', headerAlign: 'center', align: 'right', formatter: 'amount' },
      { field: 'orderTime', title: '下单日期',
        formatter: ['time', 'YYYY-MM-DD hh:mm:ss'],
        minWidth: 100,
      },
      { field: 'addressDetail', title: '下单地址',
        formatter({ row: { addressProvince, addressCity, addressCounty, addressDetail, addressUserName, addressUserPhone } }) {
          return `${addressUserName}-${addressUserPhone}- ${addressProvince}${addressCity}${addressCounty}${addressDetail}`
        },
        minWidth: 150,
      },

      { title: '操作', width: 300, slots: { default: 'operate' } },
    ],
  })

  const formItems = ref([
    createVxeFormItem('input', {
      field: 'messageTitle', title: '留言标题',
    }),
    createVxeFormItem('textarea', {
      field: 'messageContent', title: '留言内容',
    }),
  ])

  const setDisable = (disabled: boolean) => {
    formItems.value.forEach((item) => {
      if (item.itemRender && item.itemRender.props) {
        item.itemRender.props.disabled = disabled
      }
    })
  }

  const formRules = ref({
    userName: [createRequiredValidateRule()],
  })

  return {
    gridOptions,
    formItems,
    formRules, setDisable,
  }
}
