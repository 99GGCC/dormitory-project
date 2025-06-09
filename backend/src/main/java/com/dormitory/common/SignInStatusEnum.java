package com.dormitory.common;

import lombok.Getter;

/**
 * 考勤状态
 *
 * @author XXX
 * @since 2023-07-13
 */
@Getter
public enum SignInStatusEnum {
    /**
     * 已发布
     */
    PUBLISHED(1, "已发布"),
    /**
     * 已截止
     */
    CLOSED(2, "已截止"),
    /**
     * 已取消
     */
    CANCELLED(3, "已取消");

    private final Integer code;
    private final String mess;

    SignInStatusEnum(Integer code, String mess) {
        this.code = code;
        this.mess = mess;
    }
}
