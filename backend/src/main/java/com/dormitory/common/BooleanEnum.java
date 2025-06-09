package com.dormitory.common;

import lombok.Getter;

/**
 * 是否枚举
 *
 * @author XXX
 * @since 2023-07-13
 */
@Getter
public enum BooleanEnum {
    /**
     * 否
     */
    FALSE(0, "否"),
    /**
     * 是
     */
    TRUE(1, "是");

    private final Integer code;
    private final String mess;

    BooleanEnum(Integer code, String mess) {
        this.code = code;
        this.mess = mess;
    }
}
