
package com.dormitory.common;

import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

/**
 * 统一通用接口返回封装类
 *
 * @author XXX
 * @since 2024-06-12
 */
@Data
@NoArgsConstructor
public class R<T> implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * 后台是否处理成功（状态）
     */
    private boolean state;

    /**
     * 前后端约定的状态码（状态码）
     */
    private int code;

    /**
     * 后台响应的信息（处理信息）
     */
    private String message;

    /**
     * 后台响应的数据（返回数据）
     */
    private transient T data;

    public static <T> R<T> success() {
        R<T> r = new R<>();
        r.setState(true);
        r.setCode(Constant.SUCCESS_CODE);
        r.setMessage(Constant.SUCCESS);
        return r;
    }

    public static <T> R<T> success(T t) {
        R<T> r = new R<>();
        r.setState(true);
        r.setCode(Constant.SUCCESS_CODE);
        r.setMessage(Constant.SUCCESS);
        r.setData(t);
        return r;
    }

    public static <T> R<T> success(int code, String message) {
        R<T> r = new R<>();
        r.setState(true);
        r.setCode(code);
        r.setMessage(message);
        return r;
    }

    public static <T> R<T> success(int code, String message, T t) {
        R<T> r = new R<>();
        r.setState(true);
        r.setCode(code);
        r.setMessage(message);
        r.setData(t);
        return r;
    }

    public static <T> R<T> fail() {
        R<T> r = new R<>();
        r.setState(false);
        r.setCode(Constant.FAIL_CODE);
        r.setMessage(Constant.FAIL);
        return r;
    }

    public static <T> R<T> fail(T t) {
        R<T> r = new R<>();
        r.setState(false);
        r.setCode(Constant.FAIL_CODE);
        r.setMessage(Constant.FAIL);
        r.setData(t);
        return r;
    }

    public static <T> R<T> fail(int code, String message) {
        R<T> r = new R<>();
        r.setState(false);
        r.setCode(code);
        r.setMessage(message);
        return r;
    }

    public static <T> R<T> fail(int code, String message, T t) {
        R<T> r = new R<>();
        r.setState(false);
        r.setCode(code);
        r.setMessage(message);
        r.setData(t);
        return r;
    }

}

