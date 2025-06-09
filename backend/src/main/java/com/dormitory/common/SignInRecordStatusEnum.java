package com.dormitory.common;

import lombok.Getter;

/**
 * 考勤记录状态枚举
 *
 * @author XXX
 * @since 2023-07-13
 */
@Getter
public enum SignInRecordStatusEnum {
    /**
     * 未签到
     */
    NOT_SIGN(0, "未签到"),
    /**
     * 已签到
     */
    SIGN(1, "已签到"),
    /**
     * 已取消
     */
    CANCEL(2, "已取消");

    private final Integer code;
    private final String mess;

    SignInRecordStatusEnum(Integer code, String mess) {
        this.code = code;
        this.mess = mess;
    }
}
