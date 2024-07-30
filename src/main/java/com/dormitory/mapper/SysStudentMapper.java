package com.dormitory.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.dormitory.controller.vo.StudentVO;
import com.dormitory.entity.SysStudent;
import org.apache.ibatis.annotations.Param;

/**
 * <p>
 * 学生信息表 Mapper 接口
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
public interface SysStudentMapper extends BaseMapper<SysStudent> {

    /**
     * 学生个人信息
     *
     * @param studentId 学生ID
     * @return StudentVO
     */
    StudentVO selectByStudentId(@Param("studentId") Long studentId);
}
