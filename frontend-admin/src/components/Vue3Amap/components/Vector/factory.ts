/* eslint-disable ts/ban-ts-comment */
export default class Factory {
  private AMap: typeof AMap

  private map: AMap.Map

  // @ts-expect-error
  constructor({ AMap, map }) {
    this.AMap = AMap
    this.map = map
  }

  createVector(config: { type?: any, points?: any, center?: any, radius?: any }) {
    const { AMap } = this

    switch (config.type) {
      // https://lbs.amap.com/api/javascript-api-v2/documentation#polyline
      case 'polygon': {
        const { points } = config
        return new AMap.Polygon({
          path: points.split(';').map((position: string) => new AMap.LngLat(...position.split(',') as unknown as AMap.Vector2)),
        })
      }
      // https://lbs.amap.com/api/javascript-api-v2/documentation#circle
      case 'circle': {
        const { center, radius } = config
        return new AMap.Circle({
          center: new AMap.LngLat(...center.split(',') as unknown as AMap.Vector2),
          radius,
        })
      }
      default: {
        throw new Error(`Unknown type ${config.type}`)
      }
    }
  }

  serializeVector(vector: { getPath: () => { lng: any, lat: any }[], getCenter: () => { lng: any, lat: any }, getRadius: () => string }) {
    const { AMap } = this
    const { constructor } = Object.getPrototypeOf(vector)

    switch (constructor) {
      case AMap.Polygon: {
        return {
          type: 'polygon',
          points: vector.getPath().map(({ lng, lat }) => `${lng},${lat}`).join(';'),
        }
      }
      case AMap.Circle: {
        const { lng, lat } = vector.getCenter()
        return {
          type: 'circle',
          center: `${lng},${lat}`,
          radius: Number.parseInt(vector.getRadius(), 10),
        }
      }
      default: {
        throw new Error(`Unknown vector ${constructor}`)
      }
    }
  }

  createEditor(vector: { _opts: { draggable: boolean }, on: (arg0: string, arg1: { (): void, (): void }) => void, off: (arg0: string, arg1: { (): void, (): void }) => void }) {
    const { AMap, map } = this
    const { constructor } = Object.getPrototypeOf(vector)

    switch (constructor) {
      case AMap.Polygon: {
        // @ts-expect-error
        const editor = new AMap.PolygonEditor(map, vector)
        const { open: originalOpen, close: originalClose } = editor

        const handleDragstart = () => {
          editor.close()
          vector._opts.draggable = true // HACK: closeEdit 会将 draggable 置为 false
        }

        const handleDragend = () => {
          editor.open()
        }

        // 支持在微调过程中拖拽位置
        editor.open = function open(...argus: any[]) {
          setTimeout(() => {
            vector._opts.draggable = true
            vector.on('dragstart', handleDragstart)
            vector.on('dragend', handleDragend)
          })
          // @ts-expect-error
          return originalOpen.apply(this, argus)
        }

        editor.close = function close(...argus: any[]) {
          setTimeout(() => {
            vector._opts.draggable = false
            vector.off('dragstart', handleDragstart)
            vector.off('dragend', handleDragend)
          })
          // @ts-expect-error
          return originalClose.apply(this, argus)
        }

        return editor
      }
      case AMap.Circle: {
        // @ts-expect-error
        return new AMap.CircleEditor(map, vector)
      }
      default: {
        throw new Error(`Unknown vector ${constructor}`)
      }
    }
  }
}
