package com.dormitory.controller.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotNull;
import java.io.Serializable;
import java.util.List;

/**
 * 角色菜单设置DTO
 *
 * @author XXX
 * @since 2024-05-07
 */
@Data
@EqualsAndHashCode(callSuper = false)
@Accessors(chain = true)
@ApiModel(value = "角色菜单设置DTO")
public class RoleMenuDTO implements Serializable {

    private static final long serialVersionUID = 1L;
    
    /**
     * 角色ID
     */
    @NotNull(message = "角色ID")
    @ApiModelProperty(value = "角色ID")
    private Long roleId;
    /**
     * 菜单IDs
     */
    @NotNull(message = "菜单IDs")
    @ApiModelProperty(value = "菜单IDs")
    private List<Long> menuIds;
}
