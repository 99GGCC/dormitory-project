package com.dormitory.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.dormitory.controller.qry.StudentQry;
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

    /**
     * 学生信息分页查询
     *
     * @param qry   查询Qry
     * @param pages 分页类
     * @return IPage<StudentVO>
     */
    IPage<StudentVO> pageByQry(@Param("qry") StudentQry qry, @Param("pages") Page<StudentVO> pages);

    /**
     * 学生信息详情
     *
     * @param studentId 学生ID
     * @return StudentVO
     */
    StudentVO detailById(@Param("studentId") Long studentId);
}
