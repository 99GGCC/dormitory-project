package com.dormitory.service;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;
import com.dormitory.controller.dto.AdminDTO;
import com.dormitory.controller.dto.AdminLoginDTO;
import com.dormitory.controller.dto.ChangePasswordDTO;
import com.dormitory.controller.qry.AdminQry;
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

    /**
     * 管理员分页查询
     *
     * @param adminQry 查询Qry
     * @return IPage<AdminVO>
     */
    IPage<AdminVO> pageByQry(AdminQry adminQry);

    /**
     * 管理员信息详情
     *
     * @param adminId 管理员ID
     * @return AdminVO
     */
    AdminVO detail(Long adminId);


    /**
     * 新增管理员信息
     *
     * @param dto 管理员DTO
     * @return Boolean
     */
    Boolean add(AdminDTO dto);

    /**
     * 编辑管理员信息
     *
     * @param adminId 管理员ID
     * @param dto     管理员DTO
     * @return Boolean
     */
    Boolean edit(Long adminId, AdminDTO dto);

    /**
     * 删除管理员信息
     *
     * @param adminId 管理员ID
     * @return Boolean
     */
    Boolean del(Long adminId);
}
