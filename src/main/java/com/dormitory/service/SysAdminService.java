package com.dormitory.service;

import com.baomidou.mybatisplus.extension.service.IService;
import com.dormitory.controller.dto.AdminLoginDTO;
import com.dormitory.controller.dto.ChangePasswordDTO;
import com.dormitory.controller.vo.AdminLoginVO;
import com.dormitory.controller.vo.AdminVO;
import com.dormitory.entity.SysAdmin;

/**
 * <p>
 * 管理员表 服务类
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
public interface SysAdminService extends IService<SysAdmin> {

    /**
     * 管理员登录
     *
     * @param loginDTO 登录DTO
     * @return AdminLoginVO
     */
    AdminLoginVO login(AdminLoginDTO loginDTO);

    /**
     * 修改登录密码
     *
     * @param changeDTO 修改密码DTO
     * @return Boolean
     */
    Boolean changePassword(ChangePasswordDTO changeDTO);

    /**
     * 管理员个人信息
     *
     * @return AdminVO
     */
    AdminVO mine();
}
