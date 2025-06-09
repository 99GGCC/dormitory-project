import type { VxeGridProps } from 'vxe-table'
import { createVxeFormItem } from '@/utils/vxe-utils'
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
        createVxeFormItem('input', {
          field: 'classesName', title: '班级名称', span: 4,
        }),
        createVxeFormItem('input', {
          field: 'dormitoryName', title: '宿舍名称', span: 4,
        }),
        createVxeFormItem('input', {
          field: 'studentName', title: '学生姓名', span: 4,
        }),
        createVxeFormItem('input', {
          field: 'studentNum', title: '学生学号', span: 4,
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
      { field: 'createName', title: '创建者名称' },
      { field: 'createTime', title: '创建时间', formatter: 'time' },

      { field: 'studentName', title: '学生姓名' },
      { field: 'studentNum', title: '学生学号' },
      { field: 'studentPhone', title: '手机号码' },
      { field: 'dormitoryName', title: '宿舍名称' },
      { field: 'bedName', title: '床位名称' },
      { field: 'classesName', title: '班级名称' },
      { field: 'classesStatus', title: '班级状态', formatter: ['dict', 'CLASSES_STATUS'] },
      { field: 'collegeName', title: '学院名称' },
      { field: 'majorName', title: '专业名称' },
      { field: 'relocationTime', title: '记录时间' },
      { field: 'relocationType', title: '动迁类型',
        formatter({ cellValue }) {
          const format1 = dictStore.formatDict('RELOCATION_TYPE_IN', cellValue)
          if (format1 !== cellValue) {
            return format1
          }
          const format2 = dictStore.formatDict('RELOCATION_TYPE_OUT', cellValue)
          if (format2 !== cellValue) {
            return format2
          }
          return cellValue
        },
      },
      { field: 'studentSex', title: '学生性别',
        formatter({ cellValue }) {
          if (cellValue === 1) {
            return '男'
          }
          else {
            return '女'
          }
        },
      },
      { field: 'studentStatus', title: '学生状态',
        formatter: ['dict', 'STUDENT_STATUS'],
      },

      { field: 'updateName', title: '修改者名称' },
      { field: 'updateTime', title: '修改时间', formatter: 'time' },
      // { title: '操作', width: 300, slots: { default: 'operate' } },
    ],
  })

  return {
    gridOptions,
  }
}
