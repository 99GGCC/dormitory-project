/**
 * 获取服务器的资源地址
 * @param assets 地址
 * @returns {string} 服务器地址
 */
export function serverAssetsURI(assets: string) {
  return `${import.meta.env.VITE_APP_ASSETS_URL}${assets}`
}

/**
 * 参数处理
 * @param {*} params  参数
 */
export function tansParams(params: { [x: string]: any }) {
  let result = ''
  for (const propName of Object.keys(params)) {
    const value = params[propName]
    const part = `${encodeURIComponent(propName)}=`
    if (value !== null && value !== '' && typeof (value) !== 'undefined') {
      if (typeof value === 'object') {
        for (const key of Object.keys(value)) {
          if (value[key] !== null && value[key] !== '' && typeof (value[key]) !== 'undefined') {
            const params = `${propName}[${key}]`
            const subPart = `${encodeURIComponent(params)}=`
            result += `${subPart + encodeURIComponent(value[key])}&`
          }
        }
      }
      else {
        result += `${part + encodeURIComponent(value)}&`
      }
    }
  }
  return result
}

/**
 * 树查找
 * @param {Array} tree 数据
 * @param {Function} conditionFn 判断符合条件
 * @param {Function} fieldFn 字段
 * @param {string} childKey 字键名
 */
export function findNodesWithCondition(
  tree: any[],
  conditionFn: (node: any) => boolean,
  fieldFn: (node: any) => any,
  childKey: string = 'children',
): any[] {
  const result: any[] = []

  function traverse(nodes: any[]) {
    nodes.forEach((node: any) => {
      if (conditionFn(node)) {
        result.push(fieldFn(node))
      }

      if (node[childKey] && Array.isArray(node[childKey])) {
        traverse(node[childKey])
      }
    })
  }

  traverse(tree)

  return result
}
