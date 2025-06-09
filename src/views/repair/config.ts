import type { VxeGridProps } from 'vxe-table'
import { createRequiredValidateRule, createVxeFormItem } from '@/utils/vxe-utils'
import useDictStore from '@/store/modules/dict'

interface RowVO {
  [key: string]: any
}
export function useConfig(API: {
  page: (params: any) => Promise<any>
}) {
  const dictStore = useDictStore()
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
        createVxeFormItem('select', {
          field: 'repairStatus', title: '维修状态', span: 4,
          options: dictStore.getDict('REPAIR_STATUS'),
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
      // { field: 'createName', title: '创建者名称' },
      { field: 'createTime', title: '创建时间', formatter: 'time' },

      { field: 'dormitoryName', title: '宿舍名称' },
      { field: 'shortName', title: '楼栋简称' },
      // { field: 'buildingFloor', title: '楼层' },
      { field: 'repairFacilities', title: '维修设施' },
      { field: 'faultDescription', title: '故障描述' },
      { field: 'repairStatus', title: '维修状态', formatter: ['dict', 'REPAIR_STATUS'] },
      // { field: 'repairResult', title: '维修结果', formatter: ['dict', 'REPAIR_STATUS'] },
      { field: 'studentName', title: '学生姓名' },
      { field: 'studentNum', title: '学生学号' },
      { field: 'studentPhone', title: '手机号码' },

      // { field: 'updateName', title: '修改者名称' },
      { field: 'updateTime', title: '修改时间', formatter: 'time' },
      { title: '操作', width: 300, slots: { default: 'operate' } },
    ],
  })

  const formItems = ref([
    createVxeFormItem('input', {
      field: 'shortName', title: '楼栋简称',
      props: {
        disabled: true,
      },
    }),
    createVxeFormItem('input', {
      field: 'repairFacilities', title: '维修设施',
      props: {
        disabled: true,
      },
    }),
    createVxeFormItem('textarea', {
      field: 'faultDescription', title: '故障描述',
      props: {
        disabled: true,
      },
    }),
    // createVxeFormItem('select', {
    //   field: 'repairStatus', title: '维修状态',
    //   options: dictStore.getDict('REPAIR_STATUS'),
    // }),
    {
      field: 'repairStatus', title: '维修状态', span: 24,
      slots: {
        default: 'repairStatusSlots',
      },
      itemRender: {},
    },
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
