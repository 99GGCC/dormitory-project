package com.dormitory.config;

import cn.dev33.satoken.stp.StpUtil;
import com.alibaba.fastjson.JSON;
import com.dormitory.entity.SysAdmin;
import com.dormitory.entity.SysStudent;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Pointcut;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.context.request.RequestAttributes;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

import javax.servlet.http.HttpServletRequest;
import java.lang.reflect.Field;
import java.util.HashMap;
import java.util.Map;

/**
 * 切面日志
 *
 * @author XXX
 * @since 2024-05-07
 */
@Aspect
@Configuration
public class LogAopConfig {
    private static final Logger logger = LoggerFactory.getLogger(LogAopConfig.class);

    @Pointcut(value = "execution(public * com.dormitory.controller..*(..))")
    public void execute() {
    }

    @Around("execute()")
    public Object doAround(ProceedingJoinPoint pjp) throws Throwable {
        RequestAttributes requestAttributes = RequestContextHolder.getRequestAttributes();
        ServletRequestAttributes servletRequestAttributes = (ServletRequestAttributes) requestAttributes;
        assert servletRequestAttributes != null;
        HttpServletRequest httpServletRequest = servletRequestAttributes.getRequest();
        // 获取当前请求对象
        String uri = httpServletRequest.getRequestURI();
        String method = httpServletRequest.getMethod();
        String queryString = httpServletRequest.getQueryString();
        Object[] args = pjp.getArgs();
        StringBuilder params = new StringBuilder();
        if (args.length > 0) {
            if ("POST".equals(method)) {
                // 获取请求参数
                for (Object o : args) {
                    if (null != o) {
                        Map<String, Object> map = getKeyAndValue(o);
                        if (!map.isEmpty()) {
                            params.append(map);
                        }
                    }
                }
            } else if ("GET".equals(method)) {
                params = new StringBuilder(StringUtils.isBlank(queryString) ? StringUtils.EMPTY : queryString);
            }
        }
        SysAdmin admin;
        SysStudent student;
        try {
            admin = JSON.parseObject(String.valueOf(JSON.toJSON(StpUtil.getSession().get("admin"))), SysAdmin.class);
        } catch (Exception e) {
            admin = null;
        }
        try {
            student = JSON.parseObject(String.valueOf(JSON.toJSON(StpUtil.getSession().get("student"))), SysStudent.class);
        } catch (Exception e) {
            student = null;
        }
        long beginTime = System.currentTimeMillis();
        if (!ObjectUtils.isEmpty(admin)) {
            logger.info("管理员：[{}-{}],请求URI:[{}],请求方式:[{}],参数:[{}]",
                    admin.getAdminId(),
                    admin.getAdminName(),
                    uri, method, params);
        } else if (!ObjectUtils.isEmpty(student)) {
            logger.info("学生：[{}-{}],请求URI:[{}],请求方式:[{}],参数:[{}]",
                    student.getStudentId(),
                    student.getStudentName(),
                    uri, method, params);
        } else {
            logger.info("未登录用户：[{}-{}],请求URI:[{}],请求方式:[{}],参数:[{}]", "0", "系统", uri, method, params);
        }
        Object result = pjp.proceed();
        long endTime = System.currentTimeMillis();
        long time = endTime - beginTime;
        logger.info("请求结束，返回参数:[{}],执行时长：{}毫秒", result, time);
        return result;
    }

    public static Map<String, Object> getKeyAndValue(Object object) {
        Map<String, Object> map = new HashMap<>();
        Class<?> uCla = object.getClass();

        if (!(uCla.getPackage().getName().startsWith("com.dormitory")
                || uCla.getPackage().getName().startsWith("java."))) {
            return map;
        }

        Field[] fs = uCla.getDeclaredFields();
        for (Field f : fs) {
            f.setAccessible(true);
            Object val;
            try {
                val = f.get(object);
                map.put(f.getName(), val);
            } catch (Exception e) {
                logger.error(e.getMessage(), e);
            }
        }
        return map;
    }
}
