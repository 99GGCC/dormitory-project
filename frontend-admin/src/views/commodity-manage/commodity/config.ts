import type { VxeGridProps } from 'vxe-table'
import { createRequiredValidateRule, createVxeFormItem } from '@/utils/vxe-utils'
import ImagePreview from '@/components/ImagePreview/index.vue'
import TYPE_API from '@/api/modules/commodity/lsType'
import type { ModalType } from '@/types/global'

interface RowVO {
  [key: string]: any
}
export function useConfig(API: {
  page: (params: any) => Promise<any>
}) {
  const typeOptions = ref([])

  TYPE_API.list({}).then((res) => {
    typeOptions.value = res.data.map((item: any) => ({
      ...item,
      label: item.typeName,
      value: item.typeId,
    }))
  })

  const gridOptions = computed<VxeGridProps<RowVO>>(() => ({
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
          field: 'commodityName', title: '商品名称', span: 4,
        }),
        createVxeFormItem('select', {
          field: 'commoditySort', title: '商品排序', span: 4,
          options: [
            { label: '热度', value: 0 },
            { label: '最新', value: 1 },
          ],
        }),
        // 类型ID
        createVxeFormItem('select', {
          field: 'typeId', title: '类型ID', span: 4,
          options: typeOptions.value,
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

      { field: 'commodityId', title: '商品ID' },
      { field: 'commodityName', title: '商品名称' },
      {
        field: 'commodityImg', title: '商品图片',
        slots: {
          default({ row }) {
            return h(ImagePreview, {
              src: row.commodityImg,
              width: 'auto',
              height: 40,
            })
          },
        },
      },
      { field: 'commodityPrice', title: '商品价格' },
      { field: 'commoditySales', title: '商品销量' },
      { field: 'commodityUnit', title: '商品单位' },
      // { field: 'specsList', title: '商品规格列表' },
      // { field: 'typeId', title: '类型Id' },
      { field: 'typeName', title: '类型名称' },

      { title: '操作', width: 300, slots: { default: 'operate' } },
    ],
  }))

  const formItems = ref([
    createVxeFormItem('input', {
      field: 'commodityName', title: '商品名称',
    }),
    createVxeFormItem('input', {
      field: 'commodityUnit', title: '商品单位',
    }),
    createVxeFormItem('input', {
      field: 'commodityPrice', title: '商品价格',
    }),
    {
      field: 'commodityImg', title: '商品图片', span: 24,
      slots: {
        default: 'commodityImgSlots',
      },
      itemRender: {},
    },
    createVxeFormItem('textarea', {
      field: 'commodityDescribe', title: '商品描述',
    }),
    {
      field: 'specsList', title: '商品规格列表', span: 24,
      slots: {
        default: 'specsListSlots',
      },
      itemRender: {},
    },
    {
      field: 'typeId', title: '类型', span: 24,
      slots: {
        default: 'typeIdSlots',
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
