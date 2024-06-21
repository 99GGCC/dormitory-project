package com.dormitory.exception;

import cn.dev33.satoken.exception.NotLoginException;
import cn.dev33.satoken.exception.NotPermissionException;
import com.dormitory.common.R;
import com.dormitory.common.Constant;
import com.dormitory.common.R;
import org.springframework.context.support.DefaultMessageSourceResolvable;
import org.springframework.validation.BindException;
import org.springframework.validation.ObjectError;
import org.springframework.web.bind.MethodArgumentNotValidException;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RestControllerAdvice;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.util.List;
import java.util.stream.Collectors;

/**
 * 全局异常拦截
 *
 * @author XXX
 */
@RestControllerAdvice
public class GraceExceptionHandler {
    /**
     * 全局异常拦截（拦截项目中的NotLoginException异常）
     *
     * @param nle
     * @param request
     * @param response
     * @return
     */
    @ExceptionHandler(NotLoginException.class)
    public R<Object> handlerNotLoginException(NotLoginException nle, HttpServletRequest request, HttpServletResponse response) {

        // 打印堆栈，以供调试
        nle.printStackTrace();

        // 判断场景值，定制化异常信息
        String message;
        switch (nle.getType()) {
            case NotLoginException.NOT_TOKEN:
                message = "未提供token";
                break;
            case NotLoginException.INVALID_TOKEN:
                message = "token无效";
                break;
            case NotLoginException.TOKEN_TIMEOUT:
                message = "token已过期";
                break;
            case NotLoginException.BE_REPLACED:
                message = "token已被顶下线";
                break;
            case NotLoginException.KICK_OUT:
                message = "token已被踢下线";
                break;
            default:
                message = "当前会话未登录";
                break;
        }

        // 返回给标识码和问题描述给前端
        return R.fail(Constant.NOT_LOGIN_CODE, message);
    }

    /**
     * 全局抛出参数验证异常类
     *
     * @param e 参数验证异常
     * @return Result
     */
    @ExceptionHandler(value = MethodArgumentNotValidException.class)
    public R<Object> handleMethodArgumentNotValidException(MethodArgumentNotValidException e) {
        List<ObjectError> allErrors = e.getBindingResult().getAllErrors();
        String message = allErrors.stream().map(DefaultMessageSourceResolvable::getDefaultMessage).collect(Collectors.joining(";"));
        return R.fail(Constant.FAIL_CODE, message + "不能为空");
    }

    /**
     * 全局抛出参数验证异常类
     *
     * @param e 参数验证异常
     * @return Result
     */
    @ExceptionHandler(value = BindException.class)
    public R<Object> bindException(BindException e) {
        List<ObjectError> allErrors = e.getBindingResult().getAllErrors();
        String message = allErrors.stream().map(DefaultMessageSourceResolvable::getDefaultMessage).collect(Collectors.joining(";"));
        return R.fail(Constant.FAIL_CODE, message);
    }


    /**
     * 全局抛出业务异常类
     *
     * @param e 业务异常类
     * @return Result
     * @author XXX
     */
    @ExceptionHandler(value = ServiceException.class)
    public R<Object> serviceException(ServiceException e) {
        e.printStackTrace();
        return R.fail(e.getCode(), e.getMessage());
    }
}
