package com.dormitory.controller.vo;

import cn.dev33.satoken.stp.SaTokenInfo;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.experimental.Accessors;

/**
 * 管理员登录VO
 */
@Data
@EqualsAndHashCode(callSuper = false)
@Accessors(chain = true)
@ApiModel(value = "管理员登录VO")
public class AdminLoginVO {

    /**
     * 管理员ID
     */
    private Long adminId;

    /**
     * 管理员名称
     */
    private String adminName;

    /**
     * 手机号码
     */
    private String adminPhone;

    /**
     * saToken
     */
    @ApiModelProperty("saToken")
    private String saToken;

    /**
     * 用户登录信息
     */
    @ApiModelProperty("用户登录信息")
    private SaTokenInfo tokenInfo;
}
