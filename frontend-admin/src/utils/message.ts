import type { MessageIntance, MessageOptions, MessageTypeOptions } from 'vue-m-message'
import Message from 'vue-m-message'
import type { ModalEventTypes, VxeModalDefines } from 'vxe-table'
import { VXETable } from 'vxe-table'

export function successMsg(msg: string, options?: MessageTypeOptions): MessageIntance {
  return Message.success(msg, { duration: 3000, zIndex: 2000, ...options })
}

export function errorMsg(msg: string, options?: MessageTypeOptions): MessageIntance {
  return Message.error(msg, { duration: 3000, zIndex: 2000, ...options })
}

export function warningMsg(msg: string, options?: MessageTypeOptions): MessageIntance {
  return Message.warning(msg, { duration: 3000, zIndex: 2000, ...options })
}

export function loadingMsg(msg: string, options?: MessageTypeOptions): MessageIntance {
  return Message.loading(msg, { duration: -1, zIndex: 2000, ...options })
}

export function infoMsg(msg: string, options?: MessageTypeOptions): MessageIntance {
  return Message.info(msg, { duration: 3000, zIndex: 2000, ...options })
}

export function customMsg(options: MessageOptions): MessageIntance {
  return Message(options)
}
export function confirmMsg(msg: string, options: VxeModalDefines.ModalOptions = {}): Promise<ModalEventTypes> {
  return new Promise((resolve, reject) => {
    VXETable.modal.confirm(msg, '', options).then((res) => {
      if (res === 'confirm') {
        resolve('confirm')
      }
      else {
        reject(res)
      }
    })
  })
}

export function closeAllMsg(): void {
  Message.closeAll()
}
