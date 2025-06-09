package com.dormitory.service.impl;

import cn.dev33.satoken.stp.SaTokenInfo;
import cn.dev33.satoken.stp.StpUtil;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.conditions.query.LambdaQueryChainWrapper;
import com.baomidou.mybatisplus.extension.conditions.update.LambdaUpdateChainWrapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.dormitory.common.Constant;
import com.dormitory.controller.dto.AdminDTO;
import com.dormitory.controller.dto.AdminLoginDTO;
import com.dormitory.controller.dto.ChangePasswordDTO;
import com.dormitory.controller.qry.AdminQry;
import com.dormitory.controller.vo.AdminLoginVO;
import com.dormitory.controller.vo.AdminVO;
import com.dormitory.entity.SysAdmin;
import com.dormitory.entity.SysRole;
import com.dormitory.entity.SysUserRole;
import com.dormitory.exception.ServiceException;
import com.dormitory.mapper.SysAdminMapper;
import com.dormitory.mapper.SysUserRoleMapper;
import com.dormitory.service.SysAdminService;
import com.dormitory.utils.CopyUtils;
import com.dormitory.utils.RedisUtil;
import com.dormitory.utils.SaltUtils;
import lombok.RequiredArgsConstructor;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

import java.util.List;

/**
 * <p>
 * 管理员表 服务实现类
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
@Service
@RequiredArgsConstructor
public class SysAdminServiceImpl extends ServiceImpl<SysAdminMapper, SysAdmin> implements SysAdminService {

    /**
     * redis工具类
     */
    private final RedisUtil redisUtil;
    /**
     * 管理员角色Mapper
     */
    private final SysUserRoleMapper userRoleMapper;

    /**
     * 管理员登录
     *
     * @param loginDTO 登录DTO
     * @return AdminLoginVO
     */
    @Override
    public AdminLoginVO login(AdminLoginDTO loginDTO) {
        // 判断验证码是否正确
        if (redisUtil.hasKey(Constant.SMS_CODE + loginDTO.getCode().toLowerCase())) {
            // 清除验证码
            redisUtil.del(Constant.SMS_CODE + loginDTO.getCode().toLowerCase());
            // 登录校验
            SysAdmin admin = new LambdaQueryChainWrapper<>(baseMapper)
                    .eq(SysAdmin::getAdminPhone, loginDTO.getAdminPhone())
                    .one();
            // 判断管理是否为空
            if (admin != null) {
                // 密码校验
                if (SaltUtils.verify(loginDTO.getAdminPass(), admin.getAdminSalt(), admin.getAdminPass())) {
                    // 调用登录返回值方法
                    return getLoginVO(admin);
                } else {
                    // 如果密码错误，返回消息
                    throw new ServiceException("管理员名或密码错误!");
                }
            } else {
                throw new ServiceException("管理员名或密码错误!");
            }
        } else {
            throw new ServiceException("验证码错误!");
        }
    }

    /**
     * 修改登录密码
     *
     * @param changeDTO 修改密码DTO
     * @return Boolean
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public Boolean changePassword(ChangePasswordDTO changeDTO) {
        long adminId = StpUtil.getLoginIdAsLong();
        SysAdmin admin = baseMapper.selectById(adminId);
        // 密码校验
        if (SaltUtils.verify(changeDTO.getOldPass(), admin.getAdminSalt(), admin.getAdminPass())) {
            // 加密新密码并保存数据库
            new LambdaUpdateChainWrapper<>(baseMapper)
                    .eq(SysAdmin::getAdminId, adminId)
                    .set(SysAdmin::getAdminPass, SaltUtils.md5Password(changeDTO.getNewPass(), admin.getAdminSalt()))
                    .update();
            // 登录失效
            StpUtil.logout();
            // 返回修改成功
            return true;
        } else {
            // 如果密码错误，返回消息
            throw new ServiceException("旧登录密码错误!");
        }
    }

    /**
     * 管理员个人信息
     *
     * @return AdminVO
     */
    @Override
    public AdminVO mine() {
        SysAdmin admin = baseMapper.selectById(StpUtil.getLoginIdAsLong());
        // 转换管理员VO
        AdminVO adminVO = CopyUtils.classCopy(admin, AdminVO.class);
        // 查询管理员角色
        SysRole role = userRoleMapper.selectRoleByUserId(admin.getAdminId());
        // 存入返回VO
        adminVO.setRoleId(role.getRoleId()).setRoleName(role.getRoleName()).setRoleFlag(role.getRoleFlag());
        // 返回管理员信息
        return adminVO;
    }

    /**
     * 管理员分页查询
     *
     * @param adminQry 查询Qry
     * @return IPage<AdminVO>
     */
    @Override
    public IPage<AdminVO> pageByQry(AdminQry adminQry) {
        return baseMapper.pageByQry(adminQry, new Page<>(adminQry.getPage(), adminQry.getLimit()));
    }

    /**
     * 管理员信息详情
     *
     * @param adminId 管理员ID
     * @return AdminVO
     */
    @Override
    public AdminVO detail(Long adminId) {
        return baseMapper.detail(adminId);
    }

    /**
     * 新增管理员信息
     *
     * @param dto 管理员DTO
     * @return Boolean
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public Boolean add(AdminDTO dto) {
        // 校验管理员密码是否为空
        if (StringUtils.isEmpty(dto.getAdminPass())) {
            throw new ServiceException("管理员密码不能为空!");
        }
        // 校验管理员是否存在
        List<SysAdmin> list = new LambdaQueryChainWrapper<>(baseMapper)
                .eq(SysAdmin::getAdminPhone, dto.getAdminPhone())
                .list();
        if (!CollectionUtils.isEmpty(list)) {
            throw new ServiceException("管理员信息已存在!");
        }
        // 复制管理员信息
        SysAdmin admin = CopyUtils.classCopy(dto, SysAdmin.class);
        // 设置管理员加密盐
        admin.setAdminSalt(SaltUtils.createSalt());
        // 加密管理员密码
        admin.setAdminPass(SaltUtils.md5Password(admin.getAdminPass(), admin.getAdminSalt()));
        // 保存管理员信息
        baseMapper.insert(admin);
        // 保存管理员角色信息
        userRoleMapper.insert(new SysUserRole().setUserId(admin.getAdminId()).setRoleId(dto.getRoleId()));
        // 返回新增成功
        return true;
    }

    /**
     * 编辑管理员信息
     *
     * @param adminId 管理员ID
     * @param dto     管理员DTO
     * @return Boolean
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public Boolean edit(Long adminId, AdminDTO dto) {
        // 校验管理员是否存在
        List<SysAdmin> list = new LambdaQueryChainWrapper<>(baseMapper)
                .eq(SysAdmin::getAdminPhone, dto.getAdminPhone())
                .ne(SysAdmin::getAdminId, adminId)
                .list();
        if (!CollectionUtils.isEmpty(list)) {
            throw new ServiceException("管理员信息已存在!");
        }
        // 复制管理员信息
        SysAdmin admin = CopyUtils.classCopy(dto, SysAdmin.class);
        // 设置管理员ID
        admin.setAdminId(adminId);
        // 校验管理员是否修改密码
        if (StringUtils.isNotEmpty(dto.getAdminPass())) {
            // 设置管理员加密盐
            admin.setAdminSalt(SaltUtils.createSalt());
            // 加密管理员密码
            admin.setAdminPass(SaltUtils.md5Password(admin.getAdminPass(), admin.getAdminSalt()));
        }
        // 修改管理员信息
        baseMapper.updateById(admin);
        // 删除管理员角色信息
        new LambdaUpdateChainWrapper<>(userRoleMapper)
                .eq(SysUserRole::getUserId, adminId)
                .remove();
        // 保存管理员角色信息
        userRoleMapper.insert(new SysUserRole().setUserId(admin.getAdminId()).setRoleId(dto.getRoleId()));
        // 返回修改成功
        return true;
    }

    /**
     * 删除管理员信息
     *
     * @param adminId 管理员ID
     * @return Boolean
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public Boolean del(Long adminId) {
        // 删除管理员角色信息
        new LambdaUpdateChainWrapper<>(baseMapper)
                .eq(SysAdmin::getAdminId, adminId)
                .remove();
        // 删除管理员信息
        baseMapper.deleteById(adminId);
        // 返回删除成功
        return true;
    }

    /**
     * 获取登录数据
     *
     * @param admin 管理员信息
     * @return AdminLoginVO
     */
    private AdminLoginVO getLoginVO(SysAdmin admin) {
        // 初始化管理员登录VO
        AdminLoginVO loginVO = new AdminLoginVO();
        // 登录实现
        StpUtil.login(admin.getAdminId());
        // 缓存管理员信息
        StpUtil.getSession().set("admin", admin);
        // 获取登录管理员信息
        SaTokenInfo tokenInfo = StpUtil.getTokenInfo();
        // 查询管理员角色
        SysRole role = userRoleMapper.selectRoleByUserId(admin.getAdminId());
        if (ObjectUtils.isNotEmpty(role)) {
            // 存入返回VO
            loginVO.setRoleId(role.getRoleId()).setRoleName(role.getRoleName()).setRoleFlag(role.getRoleFlag());
        } else {
            throw new ServiceException("获取管理员角色失败,请联系管理员!");
        }
        // 组装管理员登录返回VO并返回
        return loginVO.setAdminId(admin.getAdminId())
                .setAdminPhone(admin.getAdminPhone())
                .setAdminName(admin.getAdminName())
                .setSaToken(tokenInfo.getTokenValue())
                .setTokenInfo(tokenInfo);
    }
}
