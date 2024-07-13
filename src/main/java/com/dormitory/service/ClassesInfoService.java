package com.dormitory.service;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.dormitory.controller.dto.ClassesInfoDTO;
import com.dormitory.controller.qry.ClassesInfoQry;
import com.dormitory.controller.vo.ClassesInfoVO;
import com.dormitory.entity.ClassesInfo;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;

/**
 * <p>
 * 班级信息表 服务类
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
public interface ClassesInfoService extends IService<ClassesInfo> {
    /**
     * 班级信息分页查询
     *
     * @param qry 查询Qry
     * @return IPage<ClassesInfoVO>
     */
    IPage<ClassesInfoVO> pageByQry(ClassesInfoQry qry);

    /**
     * 班级信息列表查询
     *
     * @param qry 查询Qry
     * @return List<ClassesInfoVO>
     */
    List<ClassesInfoVO> listByQry(ClassesInfoQry qry);

    /**
     * 班级信息详情
     *
     * @param classesId 班级ID
     * @return ClassesInfoVO
     */
    ClassesInfoVO detailById(Long classesId);

    /**
     * 新增班级信息
     *
     * @param dto 班级DTO
     * @return Boolean
     */
    Boolean add(ClassesInfoDTO dto);

    /**
     * 编辑班级信息
     *
     * @param classesId 班级ID
     * @param dto       班级DTO
     * @return Boolean
     */
    Boolean edit(Long classesId, ClassesInfoDTO dto);

    /**
     * 删除班级信息
     *
     * @param classesId 班级ID
     * @return Boolean
     */
    Boolean del(Long classesId);
}
