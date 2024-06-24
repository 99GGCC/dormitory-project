package com.dormitory.service.impl;

import cn.dev33.satoken.stp.SaTokenInfo;
import cn.dev33.satoken.stp.StpUtil;
import com.baomidou.mybatisplus.extension.conditions.query.LambdaQueryChainWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.dormitory.common.Constant;
import com.dormitory.controller.dto.AdminLoginDTO;
import com.dormitory.controller.vo.AdminLoginVO;
import com.dormitory.entity.SysAdmin;
import com.dormitory.exception.ServiceException;
import com.dormitory.mapper.SysAdminMapper;
import com.dormitory.service.SysAdminService;
import com.dormitory.utils.RedisUtil;
import com.dormitory.utils.SaltUtils;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

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
                    throw new ServiceException("用户名或密码错误!");
                }
            } else {
                throw new ServiceException("用户名或密码错误!");
            }
        } else {
            throw new ServiceException("验证码错误!");
        }
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
        // 获取登录用户信息
        SaTokenInfo tokenInfo = StpUtil.getTokenInfo();
        // 组装管理员登录返回VO并返回
        return loginVO.setAdminId(admin.getAdminId())
                .setAdminPhone(admin.getAdminPhone())
                .setAdminName(admin.getAdminName())
                .setSaToken(tokenInfo.getTokenValue())
                .setTokenInfo(tokenInfo);
    }
}
