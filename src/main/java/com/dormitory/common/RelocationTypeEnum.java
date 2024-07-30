package com.dormitory.common;

import lombok.Getter;

/**
 * 动迁状态枚举
 *
 * @author XXX
 * @since 2023-07-13
 */
@Getter
public enum RelocationTypeEnum {
    /**
     * 首次迁入
     */
    FIRST_IN(0, "首次迁入"),
    /**
     * 调换迁入
     */
    CHANGE_IN(1, "调换迁入"),
    /**
     * 调换迁出
     */
    CHANGE_OUT(2, "调换迁出"),
    /**
     * 毕业迁出
     */
    GRADUATE_OUT(3, "毕业迁出"),
    /**
     * 其他迁出
     */
    OTHER_OUT(4, "其他迁出"),
    /**
     * 其他迁入
     */
    OTHER_IN(5, "其他迁入");

    private final Integer code;
    private final String mess;

    RelocationTypeEnum(Integer code, String mess) {
        this.code = code;
        this.mess = mess;
    }
}
