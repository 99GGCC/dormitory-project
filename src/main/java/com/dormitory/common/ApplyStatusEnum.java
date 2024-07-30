package com.dormitory.common;

import lombok.Getter;

/**
 * 调换申请状态枚举
 *
 * @author XXX
 * @since 2023-07-13
 */
@Getter
public enum ApplyStatusEnum {
    /**
     * 已申请
     */
    APPLY(0, "已申请"),
    /**
     * 通过申请
     */
    PASS(1, "通过申请"),
    /**
     * 拒绝申请
     */
    REFUSE(2, "拒绝申请"),
    /**
     * 取消申请
     */
    CANCEL(3, "取消申请");

    private final Integer code;
    private final String mess;

    ApplyStatusEnum(Integer code, String mess) {
        this.code = code;
        this.mess = mess;
    }
}
