package com.dormitory.service;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;
import com.dormitory.controller.dto.CollegeInfoDTO;
import com.dormitory.controller.qry.CollegeInfoQry;
import com.dormitory.controller.vo.CollegeInfoVO;
import com.dormitory.entity.CollegeInfo;

import java.util.List;

/**
 * <p>
 * 学院信息表 服务类
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
public interface CollegeInfoService extends IService<CollegeInfo> {

    /**
     * 学院信息分页查询
     *
     * @param qry 查询Qry
     * @return IPage<CollegeInfoVO>
     */
    IPage<CollegeInfoVO> pageByQry(CollegeInfoQry qry);

    /**
     * 学院信息列表查询
     *
     * @param qry 查询Qry
     * @return List<CollegeInfoVO>
     */
    List<CollegeInfoVO> listByQry(CollegeInfoQry qry);

    /**
     * 学院信息详情
     *
     * @param collegeId 学院ID
     * @return CollegeInfoVO
     */
    CollegeInfoVO detailById(Long collegeId);

    /**
     * 新增学院信息
     *
     * @param dto 学院DTO
     * @return Boolean
     */
    Boolean add(CollegeInfoDTO dto);

    /**
     * 编辑学院信息
     *
     * @param collegeId 学院ID
     * @param dto       学院DTO
     * @return Boolean
     */
    Boolean edit(Long collegeId, CollegeInfoDTO dto);

    /**
     * 删除学院信息
     *
     * @param collegeId 学院ID
     * @return Boolean
     */
    Boolean del(Long collegeId);
}
