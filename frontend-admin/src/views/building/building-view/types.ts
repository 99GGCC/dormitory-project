export interface Dormitory {
  'bedInfoList'?: {
    'bedId'?: number
    'bedName'?: string
    'bedStatus'?: number
    'classesId'?: number
    'classesName'?: string
    'classesStatus'?: number
    'collegeId'?: number
    'collegeName'?: string
    'dormitoryId'?: number
    'isHead'?: number
    'majorId'?: number
    'majorName'?: string
    'studentId'?: number
    'studentName'?: string
    'studentNum'?: string
    'studentPhone'?: string
    'studentSex'?: number
    'studentStatus'?: number
    'useStudent'?: number
  }[]
  'buildingFloor'?: number
  'buildingId'?: number
  'dormitoryId'?: number
  'dormitoryName'?: string
  'dormitoryStatus'?: number
  'useStatus'?: number
}
