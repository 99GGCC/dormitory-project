import { groupBy } from 'lodash-es'
import API from '@/api/modules/dict'
import storage from '@/utils/storage'

const useDictStore = defineStore(
  // 唯一ID
  'dict',
  () => {
    const dictList = ref<any>(storage.local.get('dictList') ?? [])
    const dictMap = computed(() => {
      return groupBy(dictList.value, 'dictTypeCode')
    })
    async function init() {
      API.list({ }).then((res: any) => {
        dictList.value = res.data
        storage.local.set('dictList', dictList.value)
      })
    }
    function getDict(key: string) {
      return dictMap.value[key].map((it) => {
        return {
          ...it,
          label: it.dictName,
          value: +it.dictValue,
        }
      })
    }

    function formatDict(dictCode: string, value: number) {
      const item = dictMap.value[dictCode].find(it => +it.dictValue === +value)
      if (item) {
        return item.dictName
      }
      else {
        return value
      }
    }

    function clear() {
      storage.local.remove('dictList')
    }

    return {
      dictList,
      init, clear,
      getDict, formatDict,
    }
  },
)

export default useDictStore
