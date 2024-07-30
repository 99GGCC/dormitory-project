package com.dormitory.service;

import com.baomidou.mybatisplus.extension.service.IService;
import com.dormitory.controller.dto.ChangePasswordDTO;
import com.dormitory.controller.dto.StudentLoginDTO;
import com.dormitory.controller.dto.StudentMineDTO;
import com.dormitory.controller.vo.StudentLoginVO;
import com.dormitory.controller.vo.StudentVO;
import com.dormitory.entity.SysStudent;

/**
 * <p>
 * 学生信息表 服务类
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
public interface SysStudentService extends IService<SysStudent> {

    /**
     * 学生登录
     *
     * @param loginDTO 学生登录DTO
     * @return StudentLoginVO
     */
    StudentLoginVO login(StudentLoginDTO loginDTO);

    /**
     * 学生个人信息
     *
     * @return StudentVO
     */
    StudentVO mine();

    /**
     * 修改个人信息
     *
     * @param mineDTO 个人信息DTO
     * @return Boolean
     */
    Boolean editMine(StudentMineDTO mineDTO);

    /**
     * 修改登录密码
     *
     * @param changeDTO 修改密码DTO
     * @return Boolean
     */
    Boolean changePassword(ChangePasswordDTO changeDTO);
}
