package com.dormitory.controller.vo;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.List;

/**
 * 角色菜单VO
 *
 * @author XXX
 * @since 2024-05-07
 */
@Data
@EqualsAndHashCode(callSuper = false)
@Accessors(chain = true)
@ApiModel("角色菜单VO")
public class RoleMenuVO implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * 角色菜单ID
     */
    @ApiModelProperty("角色菜单ID")
    private Long roleMenuId;

    /**
     * 角色ID
     */
    @ApiModelProperty("角色ID")
    private Long roleId;

    /**
     * 菜单ID
     */
    @ApiModelProperty("菜单ID")
    private Long menuId;

    /**
     * 菜单名称
     */
    @ApiModelProperty("菜单名称")
    private String menuName;

    /**
     * 菜单地址
     */
    @ApiModelProperty("菜单地址")
    private String menuUrl;

    /**
     * 菜单路由
     */
    @ApiModelProperty("菜单路由")
    private String menuPath;

    /**
     * 权限标识
     */
    @ApiModelProperty("权限标识")
    private String menuFlag;

    /**
     * 菜单父编号
     */
    @ApiModelProperty("菜单父编号")
    private Long menuParent;

    /**
     * 菜单类型 1、父菜单 2、菜单 3、按钮
     */
    @ApiModelProperty("菜单类型 1、父菜单 2、菜单 3、按钮")
    private Integer menuType;

    /**
     * 菜单排序
     */
    @ApiModelProperty("菜单排序")
    private Integer menuSort;

    /**
     * 菜单图标
     */
    @ApiModelProperty("菜单图标")
    private String menuIcon;

    /**
     * 组件位置
     */
    @ApiModelProperty("组件位置")
    private String menuPosition;

    /**
     * 备注
     */
    @ApiModelProperty("备注")
    private String remark;

    /**
     * 子菜单
     */
    @ApiModelProperty("子菜单")
    private List<RoleMenuVO> children;
}
