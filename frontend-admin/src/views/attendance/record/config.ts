import type { VxeGridProps } from 'vxe-table'
import { createRequiredValidateRule, createVxeFormItem } from '@/utils/vxe-utils'
import useDictStore from '@/store/modules/dict'

interface RowVO {
  [key: string]: any
}
export function useConfig(API: {
  recordPage: (params: any) => Promise<any>
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
        createVxeFormItem('select', {
          field: 'recordStatus', title: '考勤状态', span: 4,
          options: useDictStore().getDict('SIGN_IN_STATUS'),
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
          return API.recordPage({
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

      { field: 'recordTime', title: '考勤时间', formatter: ['time', 'YYYY-MM-DD HH:mm:ss'] },
      { field: 'endTime', title: '考勤截止时间', formatter: ['time', 'YYYY-MM-DD HH:mm:ss'] },
      { field: 'totalStudent', title: '考勤总人数' },
      { field: 'realityStudent', title: '实际签到人数' },
      { field: 'signInStatus', title: '考勤状态', formatter: ['dict', 'SIGN_IN_STATUS'] },
      { field: 'recordStatus', title: '学生考勤状态', formatter: ['dict', 'SIGN_IN_RECORD_STATUS'] },
      { field: 'studentName', title: '学生姓名' },
      { field: 'studentPhone', title: '手机号码' },
      { field: 'studentNum', title: '学生学号' },
      { field: 'studentEmail', title: '学生邮箱' },

      // { field: 'updateName', title: '修改者名称' },
      { field: 'updateTime', title: '修改时间', formatter: 'time' },
      // { title: '操作', width: 200, fixed: 'right', slots: { default: 'operate' } },
    ],
  })

  return {
    gridOptions,
  }
}
