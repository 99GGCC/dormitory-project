package com.dormitory.service;

import com.baomidou.mybatisplus.extension.service.IService;
import com.dormitory.controller.dto.StudentLoginDTO;
import com.dormitory.controller.vo.StudentLoginVO;
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
}
