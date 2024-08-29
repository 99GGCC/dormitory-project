package com.dormitory.config;

import cn.dev33.satoken.context.SaHolder;
import cn.dev33.satoken.filter.SaServletFilter;
import cn.dev33.satoken.interceptor.SaInterceptor;
import cn.dev33.satoken.router.SaHttpMethod;
import cn.dev33.satoken.router.SaRouter;
import cn.dev33.satoken.stp.StpUtil;
import cn.hutool.json.JSONUtil;
import com.dormitory.common.Constant;
import com.dormitory.common.R;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.servlet.config.annotation.CorsRegistry;
import org.springframework.web.servlet.config.annotation.InterceptorRegistry;
import org.springframework.web.servlet.config.annotation.ResourceHandlerRegistry;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;

/**
 * Sa-Token拦截器配置
 *
 * @author XXX
 * @since 2024-05-07
 */
@Configuration
@Slf4j
public class SaTokenConfigure implements WebMvcConfigurer {
    /**
     * 放行数据端口
     */
    private final String[] exclude = {
            "/swagger2",
            "/error",
            "/favicon.ico",
            "/doc.html",
            "/doc.html#/**",
            "/webjars/**",
            "/swagger-resources",
            "/v3/api-docs",
            "/images/**",
            "/common/getImageCode",
            "/common/getKeyId/**",
            "/common/upload/image",
            "/student/token/**"
    };

    /**
     * 获取配置文件的地址
     */
    @Value("${base.img}")
    private String baseUrl;

    /**
     * 注册Sa-Token的拦截器
     *
     * @param registry
     */
    @Override
    public void addInterceptors(InterceptorRegistry registry) {
        // 注册 Sa-Token 拦截器，打开注解式鉴权功能
        registry.addInterceptor(new SaInterceptor(handle -> {
            SaRouter.match("/admin/**", "/admin/login", StpUtil::checkLogin);
            SaRouter.match("/student/**", "/student/login", StpStudentUtil::checkLogin);
        })).addPathPatterns("/**").excludePathPatterns(exclude);
    }

    /**
     * 重写父类提供的跨域请求处理的接口
     *
     * @param registry
     */
    @Override
    public void addCorsMappings(CorsRegistry registry) {
        // 添加映射路径
        registry.addMapping("/**")
                // 放行哪些原始域
                .allowedOriginPatterns("*")
                // 是否发送Cookie信息
                .allowCredentials(true)
                // 放行哪些原始域(请求方式)
                .allowedMethods("GET", "POST", "DELETE", "PUT", "OPTIONS", "HEAD")
                // 放行哪些原始域(头部信息)
                .allowedHeaders("*")
                // 暴露哪些头部信息（因为跨域访问默认不能获取全部头部信息）
                .exposedHeaders("Server", "Content-Length", "saToken", "Access-Token", "Access-Control-Allow-Origin", "Access-Control-Allow-Credentials");
    }

    @SneakyThrows
    @Override
    public void addResourceHandlers(ResourceHandlerRegistry registry) {
        // 该方法返回的为当前项目的工作目录，即在哪个地方启动的java线程
        String dirPath = System.getProperty("user.dir");
        registry.addResourceHandler("/images/**").addResourceLocations("File:" + dirPath + baseUrl);
    }

    /**
     * 注册 [Sa-Token全局过滤器]
     */
    @Bean
    public SaServletFilter getSaServletFilter() {
        return new SaServletFilter()
                // 拦截与排除 path
                .addInclude("/**")
                // 异常处理函数
                .setError(e -> {
                    // 设置响应头
                    SaHolder.getResponse().setHeader("Content-Type", "application/json;charset=UTF-8");
                    // 使用封装的 JSON 工具类转换数据格式
                    return JSONUtil.toJsonStr(R.fail(Constant.FAIL_CODE, e.getMessage()));
                })
                // 前置函数：在每次认证函数之前执行
                .setBeforeAuth(obj -> {
                    // ---------- 设置跨域响应头 ----------
                    SaHolder.getResponse()
                            // 允许指定域访问跨域资源
                            .setHeader("Access-Control-Allow-Origin", "*")
                            // 允许所有请求方式
                            .setHeader("Access-Control-Allow-Methods", "POST, GET, OPTIONS, DELETE")
                            // 有效时间
                            .setHeader("Access-Control-Max-Age", "3600")
                            // 允许的header参数
                            .setHeader("Access-Control-Allow-Headers", "*");

                    // 如果是预检请求，则立即返回到前端
                    SaRouter.match(SaHttpMethod.OPTIONS)
                            .free(r -> log.info("--------OPTIONS预检请求，不做处理"))
                            .back();
                });
    }

}
