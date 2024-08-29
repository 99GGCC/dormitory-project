package com.dormitory.config;

import lombok.Data;
import lombok.Getter;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

/**
 * 读取项目相关配置
 *
 * @author ruoyi
 */
@Data
@Component
@ConfigurationProperties(prefix = "email")
public class EmailConfig {
    /**
     * 发送协议
     */
    private String host;

    /**
     * 端口号
     */
    private Integer port;

    /**
     * 发送邮箱
     */
    private String form;

    /**
     * 发送人
     */
    private String name;

    /**
     * 鉴权码
     */
    private String authentication;

    /**
     * 学生端口
     */
    private Integer studentPort;

}
