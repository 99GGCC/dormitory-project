import type {VxeGridProps} from 'vxe-table'
import {createRegExpValidateRule, createRequiredValidateRule, createVxeFormItem} from '@/utils/vxe-utils'
import {isPhoneNumber} from '@/utils/is'

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
          field: 'visitingName', title: '来访姓名', span: 4,
        }),
        createVxeFormItem('input', {
          field: 'visitingReason', title: '来访缘由', span: 4,
        }),
        {
          itemRender: {
            name: '$button',
            props: {type: 'submit', content: '查询', status: 'primary'},
          },
        },
        {
          itemRender: {
            name: '$button',
            props: {type: 'reset', content: '重置'},
          },
        },
      ],
    },
    toolbarConfig: {
      custom: true,
      slots: {
        buttons: 'toolbar_buttons',
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
        query: ({page, form}) => {
          return API.page({
            ...form,
            page: page.currentPage,
            limit: page.pageSize,
          })
        },
      },
    },
    columns: [
      {type: 'seq', width: 60},
      {field: 'createName', title: '创建者名称'},
      // {field: 'createTime', title: '创建时间', formatter: 'time'},

      {field: 'registTime', title: '登记时间', formatter: 'time'},
      {field: 'visitingName', title: '来访姓名'},
      {field: 'visitingPhone', title: '来访电话'},
      {field: 'visitingReason', title: '来访缘由'},

      // {field: 'updateName', title: '修改者名称'},
      // {field: 'updateTime', title: '修改时间', formatter: 'time'},
      {title: '操作', width: 300, slots: {default: 'operate'}},
    ],
  })

  const formItems = ref([
    createVxeFormItem('input', {
      field: 'visitingName', title: '来访姓名',
    }),
    createVxeFormItem('input', {
      field: 'visitingPhone', title: '来访电话',
    }),
    createVxeFormItem('textarea', {
      field: 'visitingReason', title: '来访缘由',
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
    visitingName: [createRequiredValidateRule()],
    visitingPhone: [createRequiredValidateRule(), createRegExpValidateRule(isPhoneNumber)],
    visitingReason: [createRequiredValidateRule()],
  })

  return {
    gridOptions,
    formItems,
    formRules, setDisable,
  }
}
