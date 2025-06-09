package com.dormitory.service.impl;

import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.dormitory.entity.SysRolePer;
import com.dormitory.mapper.SysRolePerMapper;
import com.dormitory.service.SysRolePerService;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.List;

/**
 * <p>
 * 角色权限表 服务实现类
 * </p>
 *
 * @author XXX
 * @since 2024-05-07
 */
@Service
@RequiredArgsConstructor
public class SysRolePerServiceImpl extends ServiceImpl<SysRolePerMapper, SysRolePer> implements SysRolePerService {

    /**
     * 根据用户ID获取权限列表
     *
     * @param loginId 用户ID
     * @return List<String>
     */
    @Override
    public List<String> getPerListByUserId(Object loginId) {
        return baseMapper.getPerListByUserId(loginId);
    }
}
