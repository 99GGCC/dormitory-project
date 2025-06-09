package com.dormitory.common;

import lombok.Getter;

/**
 * 维修状态枚举
 *
 * @author XXX
 * @since 2023-07-13
 */
@Getter
public enum RepairStatusEnum {
    /**
     * 已申请
     */
    APPLY(0, "已申请"),
    /**
     * 已处理
     */
    HANDLE(1, "已处理"),
    /**
     * 无需处理
     */
    NO_HANDLE(2, "无需处理"),
    /**
     * 已取消
     */
    CANCEL(3, "已取消");

    private final Integer code;
    private final String mess;

    RepairStatusEnum(Integer code, String mess) {
        this.code = code;
        this.mess = mess;
    }
}
