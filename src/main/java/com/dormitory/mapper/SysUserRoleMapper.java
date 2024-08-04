package com.dormitory.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.dormitory.entity.SysRole;
import com.dormitory.entity.SysUserRole;
import org.apache.ibatis.annotations.Param;

/**
 * <p>
 * 用户角色表 Mapper 接口
 * </p>
 *
 * @author XXX
 * @since 2024-05-07
 */
public interface SysUserRoleMapper extends BaseMapper<SysUserRole> {

    /**
     * 根据用户ID查询角色信息
     *
     * @param userId 用户ID
     * @return SysRole
     */
    SysRole selectRoleByUserId(@Param("userId") Long userId);
}
