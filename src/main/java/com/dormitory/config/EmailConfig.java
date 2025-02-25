package com.dormitory.config;

import lombok.Data;
import lombok.Getter;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

/**
 * 读取与电子邮件相关的配置属性
 *
 * @author ruoyi
 */
@Data
@Component
@ConfigurationProperties(prefix = "email")
public class EmailConfig {
    /**
     * 邮箱发送的协议
     */
    private String host;

    /**
     * 发送邮件的端口号
     */
    private Integer port;

    /**
     * 发送邮件的邮箱地址
     */
    private String form;

    /**
     * 发送人
     */
    private String name;

    /**
     * 发送邮件时所需的鉴权码或密码，用于验证发件人的身份
     */
    private String authentication;

    /**
     * 学生端口
     */
    private Integer studentPort;

}
