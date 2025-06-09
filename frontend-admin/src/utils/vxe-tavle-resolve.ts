import type { App } from 'vue'
import XEUtils from 'xe-utils'
import {
  Edit,
  Export,
  Filter,
  Keyboard,
  Menu,
  VXETable,
  Validator,
  VxeButton,
  VxeCheckbox,

  VxeCheckboxGroup,
  VxeColgroup,
  VxeColumn,
  VxeForm,
  VxeFormGather,
  VxeFormItem,
  VxeGrid,
  VxeIcon,
  VxeInput,
  VxeList,
  VxeModal,
  VxeOptgroup,
  VxeOption,
  VxePager,
  VxePulldown,
  VxeRadio,
  VxeRadioButton,
  VxeRadioGroup,
  VxeSelect,
  VxeSwitch,
  VxeTable,
  VxeTextarea,
  VxeToolbar,

  VxeTooltip,
} from 'vxe-table'
import zhCN from 'vxe-table/es/locale/lang/zh-CN'
import 'vxe-table/styles/cssvar.scss'
import 'vxe-table/lib/style.css'
import dayjs from 'dayjs'
import useDictStore from '@/store/modules/dict'

export default function useVxeTableComponents(app: App) {
  // 按需加载的方式默认是不带国际化的，自定义国际化需要自行解析占位符 '{0}'，例如：
  VXETable.config({
    i18n: (key, args) => XEUtils.toFormatString(XEUtils.get(zhCN, key), args),
  })

  // 格式金额，默认2位数
  VXETable.formats.add('amount', {
    cellFormatMethod({ cellValue }, digits = 2) {
      return XEUtils.commafy(XEUtils.toNumber(cellValue), { digits })
    },
  })

  // 格式字典
  VXETable.formats.add('dict', {
    cellFormatMethod({ cellValue }, dictCode: string) {
      const dictStore = useDictStore()
      if (cellValue !== undefined && dictCode) {
        const formatValue = dictStore.formatDict(dictCode, cellValue)
        return formatValue || cellValue
      }
      return cellValue
    },
  })

  // 格式日期
  VXETable.formats.add('time', {
    cellFormatMethod({ cellValue }, tem = 'YYYY-MM-DD') {
      return dayjs(cellValue).isValid() ? dayjs(cellValue).format(tem) : cellValue
    },
  })

  // 表格功能
  app.use(Filter)
    .use(Edit)
    .use(Keyboard)
    .use(Export)
    .use(Menu)
    .use(Validator)

  // 可选组件
  app.use(VxeIcon)
    .use(VxeColumn)
    .use(VxeColgroup)
    .use(VxeGrid)
    .use(VxeTooltip)
    .use(VxeToolbar)
    .use(VxePager)
    .use(VxeForm)
    .use(VxeFormItem)
    .use(VxeFormGather)
    .use(VxeCheckbox)
    .use(VxeCheckboxGroup)
    .use(VxeRadio)
    .use(VxeRadioGroup)
    .use(VxeRadioButton)
    .use(VxeSwitch)
    .use(VxeInput)
    .use(VxeSelect)
    .use(VxeOptgroup)
    .use(VxeOption)
    .use(VxeTextarea)
    .use(VxeButton)
    .use(VxeModal)
    .use(VxeList)
    .use(VxePulldown)

    .use(VxeTable)
}
