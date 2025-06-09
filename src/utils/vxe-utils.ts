import type { VxeFormDefines, VxeFormItemProps } from 'vxe-table'

// type TypeName = 'input' | 'textarea' | 'select' | '$input' | '$textarea' | '$select' | '$button' | '$buttons' | '$radio' | '$checkbox' | '$switch'
const typeMap = {
  input: '$input',
  date: '$input',
  datetime: '$input',
  textarea: '$textarea',
  select: '$select',
  button: '$button',
  buttons: '$buttons',
  radio: '$radio',
  checkbox: '$checkbox',
  switch: '$switch',
}
const placeholderMap = {
  input: '请输入',
  date: '请选择日期',
  datetime: '请选择日期和时间',
  textarea: '请输入',
  select: '请选择',
  button: '请选择',
  buttons: '请选择',
  radio: '请选择',
  checkbox: '请选择',
  switch: '请选择',
}
type TypeName = keyof typeof typeMap

/**
 * 创建vxe-table的表单项
 * @param type 组件类型
 * @param options 选项
 * @param options.field 字段
 * @param options.title 标题
 * @param options.span 跨度
 * @param options.options 选项
 * @param options.props 属性
 * @param options.slots 属性
 * @returns {VxeFormItemProps} 表单项
 */
export function createVxeFormItem(type: TypeName, options: {
  field: string
  title: string
  span?: number
  options?: { label: string, value: any, [key: string]: any }[]
  remote?: {
    api: any
    params?: any
    keyMap: [string, string]
  }
  props?: any
  slots?: any
}): VxeFormItemProps {
  // 这里可以根据type动态生成表单项
  // 例如：如果type是'input'，则返回一个输入框的表单项
  const selectOptions = options.options ?? []
  const item = {
    field: options.field, title: options.title, span: options.span ?? 24,
    itemRender: {
      name: typeMap[type],
      options: selectOptions,
      props: { placeholder: placeholderMap[type], ...options.props },
    },
    slots: options.slots || {},
  }
  if (type === 'date') {
    item.itemRender.props.type = 'date'
  }
  if (type === 'datetime') {
    item.itemRender.props.type = 'datetime'
  }
  return item
}

/**
 * 生成必填的校验
 * @param {string} message
 * @returns {VxeFormDefines.FormRule[]} 校验
 */
export function createRequiredValidateRule(message: 'sr' | 'xz' | 'sc' | string = 'sr'): VxeFormDefines.FormRule {
  const msg: Record<string, string> = {
    sr: '请输入',
    xz: '请选择',
    sc: '请上传',
  }
  return {
    required: true,
    message: msg[message] ?? message,
  }
}

/**
 * 生成正则校验
 * @param {() => boolean} regExp
 * @param message
 * @returns {VxeFormDefines.FormRule[]} 校验
 */
export function createRegExpValidateRule(regExp: (value: any) => boolean,
  message = '格式错误'): VxeFormDefines.FormRule {
  return {
    validator({ itemValue }) {
      if (!regExp(itemValue)) {
        return new Error(message)
      }
    },
    message,
  }
}
