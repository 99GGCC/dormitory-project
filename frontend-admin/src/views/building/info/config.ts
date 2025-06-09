import type { VxeGridProps } from 'vxe-table'
import { isNumber, isPhoneNumber, isStringNumber } from '@/utils/is'
import { createRegExpValidateRule, createRequiredValidateRule, createVxeFormItem } from '@/utils/vxe-utils'
import API from '@/api/modules/building/info'

// import ImagePreview from '@/components/ImagePreview/index.vue'

interface RowVO {
  [key: string]: any
}
export function useConfig() {
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
          field: 'buildingAdmin', title: '楼层管理员', span: 4,
        }),
        createVxeFormItem('input', {
          field: 'buildingName', title: '楼栋名称', span: 4,
        }),
        createVxeFormItem('select', {
          field: 'buildingType', title: '楼栋类型', span: 4, options: [
            { label: '男生宿舍', value: 1 },
            { label: '女生宿舍', value: 0 },
          ],
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

      // { field: 'buildingId', title: '楼栋ID' },
      { field: 'buildingName', title: '楼栋名称' },
      { field: 'shortName', title: '楼栋简称' },
      { field: 'buildingAdmin', title: '楼层管理员' },
      { field: 'buildingPhone', title: '联系电话' },
      { field: 'buildingType', title: '楼栋类型', formatter: ({ cellValue }) => {
        return cellValue === 1 ? '男生宿舍' : '女生宿舍'
      } },
      { field: 'allRoomNum', title: '总房间数量' },
      { field: 'buildingFloor', title: '楼层' },
      { field: 'roomNum', title: '楼层房间数量' },
      { field: 'useRoomNum', title: '使用房间数量' },
      { field: 'idleRoomNum', title: '空闲房间数量' },
      { field: 'disableRoomNum', title: '禁用房间数量' },
      { title: '操作', width: 300, slots: { default: 'operate' } },
    ],
  })

  const formItems = ref([
    createVxeFormItem('input', {
      field: 'buildingName', title: '楼栋名称',
    }),
    createVxeFormItem('input', {
      field: 'shortName', title: '楼栋简称',
    }),
    createVxeFormItem('input', {
      field: 'buildingAdmin', title: '楼层管理员',
    }),
    createVxeFormItem('select', {
      field: 'buildingType', title: '楼栋类型',
      options: [
        { label: '男生宿舍', value: 1 },
        { label: '女生宿舍', value: 0 },
      ],
    }),
    createVxeFormItem('input', {
      field: 'buildingFloor', title: '楼层',
    }),
    createVxeFormItem('input', {
      field: 'roomNum', title: '楼层房间数量',
    }),
    createVxeFormItem('input', {
      field: 'buildingPhone', title: '联系电话',
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
    buildingName: [createRequiredValidateRule()],
    buildingType: [createRequiredValidateRule()],
    buildingAdmin: [createRequiredValidateRule()],
    buildingFloor: [createRequiredValidateRule(), createRegExpValidateRule(isStringNumber)],
    roomNum: [createRequiredValidateRule(), createRegExpValidateRule(isStringNumber)],
    buildingPhone: [createRequiredValidateRule(), createRegExpValidateRule(isPhoneNumber)],
  })
  return {
    gridOptions,
    formItems,
    formRules, setDisable,
  }
}
