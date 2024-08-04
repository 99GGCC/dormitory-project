package com.dormitory.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.dormitory.entity.SysRolePer;
import org.apache.ibatis.annotations.Param;

import java.util.List;

/**
 * <p>
 * 角色权限表 Mapper 接口
 * </p>
 *
 * @author XXX
 * @since 2024-05-07
 */
public interface SysRolePerMapper extends BaseMapper<SysRolePer> {

    /**
     * 根据用户ID获取权限列表
     *
     * @param loginId 用户ID
     * @return List<String>
     */
    List<String> getPerListByUserId(@Param("loginId") Object loginId);
}
