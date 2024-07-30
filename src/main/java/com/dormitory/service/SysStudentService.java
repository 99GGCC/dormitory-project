package com.dormitory.service;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;
import com.dormitory.controller.dto.ChangePasswordDTO;
import com.dormitory.controller.dto.StudentDTO;
import com.dormitory.controller.dto.StudentLoginDTO;
import com.dormitory.controller.dto.StudentMineDTO;
import com.dormitory.controller.qry.StudentQry;
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
     * 学生信息分页查询
     *
     * @param qry 查询Qry
     * @return IPage<StudentVO>
     */
    IPage<StudentVO> pageByQry(StudentQry qry);

    /**
     * 学生信息详情
     *
     * @param studentId 学生ID
     * @return StudentVO
     */
    StudentVO detailById(Long studentId);

    /**
     * 新增学生信息
     *
     * @param dto 学生DTO
     * @return Boolean
     */
    Boolean add(StudentDTO dto);

    /**
     * 编辑学生信息
     *
     * @param studentId 学生ID
     * @param dto       学生DTO
     * @return Boolean
     */
    Boolean edit(Long studentId, StudentDTO dto);

    /**
     * 删除学生信息
     *
     * @param studentId 学生ID
     * @return Boolean
     */
    Boolean del(Long studentId);

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

    /**
     * 设置学生状态
     *
     * @param studentId 学生ID
     * @param status    学生状态
     * @return Boolean
     */
    Boolean status(Long studentId, Integer status);
}
