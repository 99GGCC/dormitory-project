package com.dormitory.exception;

import com.dormitory.common.Constant;
import com.fasterxml.jackson.annotation.JsonFormat;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.io.Serializable;
import java.util.Date;

/**
 * 自定义异常捕捉类
 *
 * @author XXX
 * @since 2023-04-27
 */
@Data
@EqualsAndHashCode(callSuper = true)
public class ServiceException extends RuntimeException implements Serializable {

    private static final long serialVersionUID = 7064034156794512610L;

    /**
     * 异常信息
     */
    private String message;
    private Integer code;
    @JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    private Date timestamp;
    private Boolean state;

    /**
     * 构造函数
     */
    public ServiceException() {
        this.code = Constant.FAIL_CODE;
        this.message = Constant.FAIL;
        this.timestamp = new Date();
        this.state = Boolean.FALSE;
    }

    public ServiceException(Integer code, String message) {
        this.message = message;
        this.code = code;
        this.timestamp = new Date();
        this.state = Boolean.FALSE;
    }

    public ServiceException(Integer code, Boolean state, String message) {
        this.message = message;
        this.code = code;
        this.state = state;
        this.timestamp = new Date();
    }

    public ServiceException(String message) {
        this.message = message;
        this.timestamp = new Date();
        this.code = Constant.FAIL_CODE;
        this.state = Boolean.FALSE;
    }


}
