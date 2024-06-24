package com.dormitory.controller.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotBlank;
import java.io.Serializable;

/**
 * 管理员登录DTO
 *
 * @author XXX
 * @since 2024-05-07
 */
@Data
@EqualsAndHashCode(callSuper = false)
@Accessors(chain = true)
@ApiModel(value = "管理员登录DTO")
public class AdminLoginDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * 用户账号
     */
    @NotBlank(message = "用户账号")
    @ApiModelProperty(value = "用户账号")
    private String adminPhone;

    /**
     * 用户密码
     */
    @NotBlank(message = "用户密码")
    @ApiModelProperty(value = "用户密码")
    private String adminPass;

    /**
     * 验证码
     */
    @NotBlank(message = "验证码")
    @ApiModelProperty(value = "验证码")
    private String code;
}
