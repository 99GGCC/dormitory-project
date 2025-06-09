package com.dormitory.controller.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import java.io.Serializable;


/**
 * <p>
 * 管理员DTO
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
@Data
@EqualsAndHashCode(callSuper = false)
@Accessors(chain = true)
@ApiModel(value = "管理员DTO")
public class AdminDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * 管理员名称
     */
    @NotBlank(message = "管理员名称")
    @ApiModelProperty("管理员名称")
    private String adminName;

    /**
     * 手机号码
     */
    @NotBlank(message = "手机号码")
    @ApiModelProperty("手机号码")
    private String adminPhone;

    /**
     * 登录密码
     */
    @NotBlank(message = "登录密码")
    @ApiModelProperty("登录密码")
    private String adminPass;

    /**
     * 角色ID
     */
    @NotNull(message = "角色ID")
    @ApiModelProperty("角色ID")
    private Long roleId;
}
