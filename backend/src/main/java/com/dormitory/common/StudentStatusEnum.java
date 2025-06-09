package com.dormitory.common;

import lombok.Getter;

/**
 * 学生状态枚举
 *
 * @author XXX
 * @since 2023-07-13
 */
@Getter
public enum StudentStatusEnum {
    /**
     * 正常
     */
    NORMAL(0, "正常"),
    /**
     * 毕业
     */
    GRADUATE(1, "毕业"),
    /**
     * 休学
     */
    STOP(2, "休学");

    private final Integer code;
    private final String mess;

    StudentStatusEnum(Integer code, String mess) {
        this.code = code;
        this.mess = mess;
    }
}
