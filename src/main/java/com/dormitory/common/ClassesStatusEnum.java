package com.dormitory.common;

import lombok.Getter;

/**
 * 班级状态枚举
 *
 * @author XXX
 * @since 2023-07-13
 */
@Getter
public enum ClassesStatusEnum {
    /**
     * 正常
     */
    NORMAL(0, "正常"),
    /**
     * 操作失败
     */
    GRADUATED(1, "已毕业");

    private final Integer code;
    private final String mess;

    ClassesStatusEnum(Integer code, String mess) {
        this.code = code;
        this.mess = mess;
    }
}
