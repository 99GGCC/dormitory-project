package com.dormitory.controller.qry;

import com.dormitory.common.Base;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.experimental.Accessors;

/**
 * 角色查询Qry
 *
 * @author XXX
 * @since 2024-05-07
 */
@Data
@EqualsAndHashCode(callSuper = false)
@Accessors(chain = true)
@ApiModel(value = "角色查询Qry")
public class RoleQry extends Base {

    /**
     * 角色名称
     */
    @ApiModelProperty("角色名称")
    private String roleName;
}
