├── 固定路由（constantRoutes）
│   ├── /login - 登录页
│   └── /:all(.*) - 404页面（找不到页面）
│
├── 系统路由（systemRoutes）
│   └── / - 主布局
│       ├── / - 首页
│       └── /reload - 重新加载页面
│
└── 动态路由（asyncRoutes - 导航栏菜单路由）
    ├── 学院管理（College）
    │   └── /college
    │       └── /college/index - 学院管理页面
    │
    ├── 专业管理（Major）
    │   └── /major
    │       └── /major/index - 专业管理页面
    │
    ├── 班级管理（Classes）
    │   └── /classes
    │       └── /classes/index - 班级管理页面
    │
    ├── 学生管理（Student）
    │   └── /student
    │       └── /student/index - 学生管理页面
    │
    ├── 楼栋管理（Building）
    │   └── /building
    │       └── /building/info - 楼栋信息
    │       └── /building/view - 楼栋视图
    │       └── /building/record - 动迁记录
    │
    ├── 维修管理（Repair）
    │   └── /repair
    │       └── /repair/index - 维修管理页面
    │
    ├── 调宿申请（ChangeApply）
    │   └── /changeApply
    │       └── /changeApply/index - 调宿申请页面
    │
    ├── 考勤管理（Attendance）
    │   └── /attendance
    │       └── /attendance/user - 学生考勤
    │       └── /attendance/record - 考勤记录
    │
    ├── 来访管理（Visiting）
    │   └── /visiting
    │       └── /visiting/index - 来访管理页面
    │
    └── 系统管理（System）
        └── /system
            ├── /system/user - 用户管理
            ├── /system/role - 角色管理
            └── /system/notice - 公告管理
