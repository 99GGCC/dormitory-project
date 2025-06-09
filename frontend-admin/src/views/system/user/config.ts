import type { VxeGridProps } from 'vxe-table'
import { isPassword, isPhoneNumber } from '@/utils/is'
import { createRegExpValidateRule, createRequiredValidateRule, createVxeFormItem } from '@/utils/vxe-utils'
import userApi from '@/api/modules/user'

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
          field: 'adminName', title: '管理员名称', span: 4,
        }),
        createVxeFormItem('input', {
          field: 'adminPhone', title: '手机号码', span: 4,
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
          return userApi.userPage({
            ...form,
            page: page.currentPage,
            limit: page.pageSize,
          })
        },
      },
    },
    columns: [
      { type: 'seq', width: 60 },
      { field: 'adminName', title: '姓名' },
      { field: 'adminPhone', title: '电话' },
      { field: 'roleName', title: '角色' },
      { title: '操作', width: 300, slots: { default: 'operate' } },
    ],
  })
  const formItems = ref([
    createVxeFormItem('input', {
      field: 'adminName', title: '管理员名称',
    }),
    createVxeFormItem('input', {
      field: 'adminPass', title: '登录密码',
      props: {
        type: 'password',
      },
    }),
    createVxeFormItem('input', {
      field: 'adminPhone', title: '手机号码',
    }),
    {
      field: 'roleId', title: '角色', span: 24,
      slots: {
        default: 'roleIdSlots',
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
    adminName: [createRequiredValidateRule()],
    adminPass: [createRequiredValidateRule(),
      createRegExpValidateRule((value: any) => {
        // 密码长度为6到18位
        return String(value).length >= 6 && String(value).length <= 18
      }, '密码长度为6到18位'),
    ],
    adminPhone: [createRequiredValidateRule(), createRegExpValidateRule(isPhoneNumber)],
    roleId: [createRequiredValidateRule()],
  })
  return {
    gridOptions,
    formItems,
    formRules, setDisable,
  }
}
