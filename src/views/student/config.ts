import type { VxeFormPropTypes, VxeGridProps } from 'vxe-table'
import { createRegExpValidateRule, createRequiredValidateRule, createVxeFormItem } from '@/utils/vxe-utils'
import { isEmail, isPhoneNumber } from '@/utils/is'

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
          field: 'studentName', title: '学生姓名', span: 4,
        }),
        createVxeFormItem('input', {
          field: 'studentNum', title: '学生学号', span: 4,
        }),
        createVxeFormItem('select', {
          field: 'studentStatus', title: '学生状态', span: 4,
          options: [
            { label: '正常', value: 0 },
            { label: '毕业', value: 1 },
            { label: '休学', value: 2 },
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
      // {field: 'createName', title: '创建者名称'},
      // {field: 'createTime', title: '创建时间', formatter: 'time'},

      // { field: 'studentId', title: '学生ID' },
      { field: 'studentName', title: '学生姓名' },
      { field: 'studentNum', title: '学生学号' },
      { field: 'studentPhone', title: '手机号码' },
      { field: 'studentEmail', title: '学生邮箱' },
      {
        field: 'studentSex', title: '学生性别', formatter: ({ cellValue }) => {
          if (cellValue === undefined) {
            return '未知'
          }
          return cellValue === 0 ? '女' : '男'
        },
      },
      { field: 'studentStatus', title: '学生状态', formatter: ['dict', 'STUDENT_STATUS'] },
      { field: 'collegeName', title: '学院名称' },
      { field: 'classesName', title: '班级名称' },
      { field: 'majorName', title: '专业名称' },
      {
        field: 'dormitoryName', title: '宿舍', formatter: ({ row }) => {
          return `${row.buildingName || ''}-${row.buildingFloor || ''}-${row.dormitoryName || ''}`
        },
      },

      // {field: 'updateName', title: '修改者名称'},
      { field: 'updateTime', title: '修改时间', formatter: 'time' },
      { title: '操作', width: 300, slots: { default: 'operate' } },
    ],
  })

  const formItems = ref<VxeFormPropTypes.Items>([
    createVxeFormItem('input', {
      field: 'studentName', title: '学生姓名',
    }),
    createVxeFormItem('select', {
      field: 'studentSex', title: '学生性别',
      options: [
        { label: '男', value: 1 },
        { label: '女', value: 0 },
      ],
    }),
    createVxeFormItem('input', {
      field: 'studentNum', title: '学生学号',
    }),
    createVxeFormItem('input', {
      field: 'studentPhone', title: '手机号码',
    }),
    createVxeFormItem('input', {
      field: 'studentEmail', title: '学生邮箱',
    }),
    {
      field: 'collegeId', title: '学院', span: 8,
      titleWidth: 150,
      slots: {
        default: 'collegeIdSlots',
      },
      itemRender: {},
    },
    {
      field: 'majorId', title: '专业', span: 8,
      titleWidth: 60,
      slots: {
        default: 'majorIdSlots',
      },
      itemRender: {},
    },
    {
      field: 'classesId', title: '班级', span: 8,
      titleWidth: 60,
      slots: {
        default: 'classesIdSlots',
      },
      itemRender: {},
    },
    {
      field: 'buildingId', title: '楼栋', span: 8,
      titleWidth: 150,
      slots: {
        default: 'buildingIdSlots',
      },
      itemRender: {},
    },
    {
      field: 'dormitoryId', title: '宿舍', span: 8,
      titleWidth: 60,
      slots: {
        default: 'dormitoryIdSlots',
      },
      itemRender: {},
    },
    {
      field: 'bedId', title: '床位', span: 8,
      titleWidth: 60,
      slots: {
        default: 'bedIdSlots',
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
    studentName: [createRequiredValidateRule()],
    studentSex: [createRequiredValidateRule()],
    studentNum: [createRequiredValidateRule()],
    studentPhone: [createRequiredValidateRule(), createRegExpValidateRule(isPhoneNumber)],
    studentEmail: [createRequiredValidateRule(), createRegExpValidateRule(isEmail)],
    collegeId: [createRequiredValidateRule('xz')],
    majorId: [createRequiredValidateRule('xz')],
    classesId: [createRequiredValidateRule('xz')],
    // buildingId: [createRequiredValidateRule('xz')],
    // dormitoryId: [createRequiredValidateRule('xz')],
    // bedId: [createRequiredValidateRule('xz')],
  })

  return {
    gridOptions,
    formItems,
    formRules, setDisable,
  }
}
