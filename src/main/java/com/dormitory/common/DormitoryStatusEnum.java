package com.dormitory.common;

import lombok.Getter;

/**
 * 宿舍状态枚举
 *
 * @author XXX
 * @since 2023-07-13
 */
@Getter
public enum DormitoryStatusEnum {
    /**
     * 禁用
     */
    DISABLE(0, "禁用"),
    /**
     * 启用
     */
    ENABLE(1, "启用");

    private final Integer code;
    private final String mess;

    DormitoryStatusEnum(Integer code, String mess) {
        this.code = code;
        this.mess = mess;
    }
}
