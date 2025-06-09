package com.dormitory.common;

import lombok.Getter;

/**
 * 宿舍使用状态枚举
 *
 * @author XXX
 * @since 2023-07-13
 */
@Getter
public enum UseStatusEnum {
    /**
     * 未使用
     */
    NOT_USE(0, "未使用"),
    /**
     * 已使用
     */
    USE(1, "已使用");

    private final Integer code;
    private final String mess;

    UseStatusEnum(Integer code, String mess) {
        this.code = code;
        this.mess = mess;
    }
}
