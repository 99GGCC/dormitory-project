package com.dormitory.service.impl;

import cn.dev33.satoken.stp.StpUtil;
import com.baomidou.mybatisplus.extension.conditions.query.LambdaQueryChainWrapper;
import com.baomidou.mybatisplus.extension.conditions.update.LambdaUpdateChainWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.dormitory.common.Constant;
import com.dormitory.controller.dto.RoleMenuDTO;
import com.dormitory.controller.vo.RoleMenuVO;
import com.dormitory.entity.SysMenu;
import com.dormitory.entity.SysRole;
import com.dormitory.entity.SysRoleMenu;
import com.dormitory.entity.SysRolePer;
import com.dormitory.exception.ServiceException;
import com.dormitory.mapper.SysMenuMapper;
import com.dormitory.mapper.SysRoleMenuMapper;
import com.dormitory.mapper.SysUserRoleMapper;
import com.dormitory.service.SysRoleMenuService;
import com.dormitory.service.SysRolePerService;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

/**
 * <p>
 * 角色菜单表 服务实现类
 * </p>
 *
 * @author XXX
 * @since 2024-05-07
 */
@Service
@RequiredArgsConstructor
public class SysRoleMenuServiceImpl extends ServiceImpl<SysRoleMenuMapper, SysRoleMenu> implements SysRoleMenuService {
    /**
     * 菜单Mapper
     */
    private final SysMenuMapper menuMapper;

    /**
     * 角色权限Service
     */
    private final SysRolePerService rolePerService;

    /**
     * 用户角色Mapper
     */
    private final SysUserRoleMapper userRoleMapper;

    /**
     * 根据角色ID获取菜单列表
     *
     * @param roleId 角色ID
     * @return List<RoleMenuVO>
     */
    @Override
    public List<RoleMenuVO> listTree(Long roleId) {
        List<RoleMenuVO> list = baseMapper.listByRoleId(roleId);
        return setTree(Constant.LONG_ZERO, list);
    }

    /**
     * 角色菜单授权
     *
     * @param roleMenuDTO 角色菜单操作DTO
     * @return Boolean
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public Boolean empower(RoleMenuDTO roleMenuDTO) {
        // 删除所有角色菜单授权
        new LambdaUpdateChainWrapper<>(baseMapper).eq(SysRoleMenu::getRoleId, roleMenuDTO.getRoleId()).remove();
        // 删除所有角色权限授权
        new LambdaUpdateChainWrapper<>(rolePerService.getBaseMapper()).eq(SysRolePer::getRoleId, roleMenuDTO.getRoleId()).remove();
        if (!CollectionUtils.isEmpty(roleMenuDTO.getMenuIds())) {
            List<SysRoleMenu> roleMenuAddList = new ArrayList<>();
            List<SysRolePer> perAddList = new ArrayList<>();
            List<SysMenu> list = new LambdaQueryChainWrapper<>(menuMapper).in(SysMenu::getMenuId, roleMenuDTO.getMenuIds()).list();
            // 组装角色菜单列表
            for (SysMenu menu : list) {
                // 组装角色菜单实体放入列表
                roleMenuAddList.add(new SysRoleMenu().setMenuId(menu.getMenuId()).setRoleId(roleMenuDTO.getRoleId()));
                //  组装角色权限实体放入列表
                perAddList.add(new SysRolePer().setPerFlag(menu.getMenuFlag()).setRoleId(roleMenuDTO.getRoleId()));
            }
            // 保存角色菜单
            this.saveBatch(roleMenuAddList);
            // 保存角色权限
            rolePerService.saveBatch(perAddList);
        }
        // 返回操作成功
        return true;
    }

    /**
     * 登录用户的菜单树
     *
     * @return List<RoleMenuVO>
     */
    @Override
    public List<RoleMenuVO> user() {
        Long userId = StpUtil.getLoginIdAsLong();
        // 根据用户ID查询角色信息
        SysRole role = userRoleMapper.selectRoleByUserId(userId);
        if (ObjectUtils.isEmpty(role)) {
            throw new ServiceException("获取登录用户菜单树失败!");
        }
        // 根据角色信息查询用户信息
        List<RoleMenuVO> list = baseMapper.listByRoleId(role.getRoleId());
        // 筛选角色菜单ID不为空的菜单信息
        list = list.stream().filter(menu -> !ObjectUtils.isEmpty(menu.getRoleMenuId())).collect(Collectors.toList());
        // 组装树形结构返回
        return setTree(Constant.LONG_ZERO, list);
    }

    /**
     * 登录用户的菜单列表
     *
     * @return List<RoleMenuVO>
     */
    @Override
    public List<RoleMenuVO> userList() {
        Long userId = StpUtil.getLoginIdAsLong();
        // 根据用户ID查询角色信息
        SysRole role = userRoleMapper.selectRoleByUserId(userId);
        if (ObjectUtils.isEmpty(role)) {
            throw new ServiceException("获取登录用户菜单树失败!");
        }
        // 根据角色信息查询用户信息
        List<RoleMenuVO> list = baseMapper.listByRoleId(role.getRoleId());
        // 筛选角色菜单ID不为空的菜单信息
        return list.stream().filter(menu -> !ObjectUtils.isEmpty(menu.getRoleMenuId())).collect(Collectors.toList());
    }

    /**
     * 组装树形菜单列表
     *
     * @param menuParent 父ID
     * @param list       菜单列表
     * @return List<RoleMenuVO>
     */
    public List<RoleMenuVO> setTree(Long menuParent, List<RoleMenuVO> list) {
        List<RoleMenuVO> collect = list.stream().filter(menu -> menuParent.equals(menu.getMenuParent())).collect(Collectors.toList());
        if (!CollectionUtils.isEmpty(collect)) {
            for (RoleMenuVO roleMenuVO : collect) {
                roleMenuVO.setChildren(setTree(roleMenuVO.getMenuId(), list));
            }
        }
        return collect.stream().sorted(Comparator.comparing(RoleMenuVO::getMenuSort)).collect(Collectors.toList());
    }
}
