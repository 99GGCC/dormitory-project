package com.dormitory.controller.vo;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.experimental.Accessors;

import java.io.Serializable;


/**
 * <p>
 * 管理员VO
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
@Data
@EqualsAndHashCode(callSuper = false)
@Accessors(chain = true)
@ApiModel(value = "管理员VO")
public class AdminVO implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * 管理员ID
     */
    @ApiModelProperty("管理员ID")
    private Long adminId;

    /**
     * 管理员名称
     */
    @ApiModelProperty("管理员名称")
    private String adminName;

    /**
     * 手机号码
     */
    @ApiModelProperty("手机号码")
    private String adminPhone;

}
