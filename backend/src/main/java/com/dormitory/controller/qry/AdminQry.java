package com.dormitory.controller.qry;

import com.dormitory.common.Base;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * 管理员信息Qry
 *
 * @author XXX
 * @since 2024-05-07
 */
@Data
@EqualsAndHashCode(callSuper = false)
@Accessors(chain = true)
@ApiModel(value = "管理员信息Qry")
public class AdminQry extends Base implements Serializable {

    private static final long serialVersionUID = 1L;

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

    /**
     * 角色ID
     */
    @ApiModelProperty("角色ID")
    private Long roleId;
}
