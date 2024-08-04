package com.dormitory.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.dormitory.controller.vo.RoleMenuVO;
import com.dormitory.entity.SysRoleMenu;
import org.apache.ibatis.annotations.Param;

import java.util.List;

/**
 * <p>
 * 角色菜单表 Mapper 接口
 * </p>
 *
 * @author XXX
 * @since 2024-05-07
 */
public interface SysRoleMenuMapper extends BaseMapper<SysRoleMenu> {
    /**
     * 根据角色ID获取角色菜单列表
     *
     * @param roleId 角色ID
     * @return List<RoleMenuVO>
     */
    List<RoleMenuVO> listByRoleId(@Param("roleId") Long roleId);
}
