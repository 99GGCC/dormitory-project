import type { VxeGridProps } from 'vxe-table'
import { createRequiredValidateRule, createVxeFormItem } from '@/utils/vxe-utils'
import useDictStore from '@/store/modules/dict'
import BuildApi from '@/api/modules/building/info'

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
        createVxeFormItem('datetime', {
          field: 'endTimeStart', title: '考勤截止时间开始', span: 6,
        }),
        createVxeFormItem('datetime', {
          field: 'endTimeEnd', title: '考勤截止时间结束', span: 6,
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
      // { field: 'createTime', title: '创建时间', formatter: 'time' },

      { field: 'issueTime', title: '发布时间', formatter: ['time', 'YYYY-MM-DD HH:mm:ss'] },
      { field: 'endTime', title: '考勤截止时间', formatter: ['time', 'YYYY-MM-DD HH:mm:ss'] },
      { field: 'signInStatus', title: '考勤状态', formatter: ['dict', 'SIGN_IN_STATUS'] },
      { field: 'realityStudent', title: '实际签到人数' },
      { field: 'totalStudent', title: '考勤总人数' },

      // { field: 'updateName', title: '修改者名称' },
      // { field: 'updateTime', title: '修改时间', formatter: 'time' },
      { title: '操作', width: 300, slots: { default: 'operate' } },
    ],
  })

  const formItems = ref([
    createVxeFormItem('datetime', {
      field: 'issueTime', title: '发布时间',
    }),
    createVxeFormItem('datetime', {
      field: 'endTime', title: '考勤截止时间',
    }),
    createVxeFormItem('select', {
      field: 'signInStatus', title: '考勤状态',
      options: useDictStore().getDict('SIGN_IN_RECORD_STATUS'),
    }),
    createVxeFormItem('input', {
      field: 'realityStudent', title: '实际签到人数',
    }),
    createVxeFormItem('input', {
      field: 'totalStudent', title: '考勤总人数',
    }),
    {
      field: 'buildingVOList', title: '考勤宿舍列表', span: 24,
      slots: {
        default: 'buildingVOListSlots',
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
  })

  return {
    gridOptions,
    formItems,
    formRules, setDisable,
  }
}

export function useAddConfig() {
  const formItems = ref([
    createVxeFormItem('select', {
      field: 'buildingIds', title: '考勤楼栋',
      options: [],
      props: { multiple: true, filterable: true, clearable: true, transfer: true },
    }),
    createVxeFormItem('date', {
      field: 'endTime', title: '考勤截止时间',
    }),
  ])
  BuildApi.list().then((res) => {
    formItems.value[0].itemRender!.props!.options = res.data.map((item: { buildingName: any, buildingId: any }) => {
      return {
        label: item.buildingName,
        value: item.buildingId,
      }
    })
  })
  const formRules = ref({
    buildingIds: [createRequiredValidateRule('xz')],
    endTime: [createRequiredValidateRule('xz')],
  })
  return {
    formItems,
    formRules,
  }
}
