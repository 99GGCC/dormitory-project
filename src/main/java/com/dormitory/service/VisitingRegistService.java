package com.dormitory.service;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;
import com.dormitory.controller.dto.VisitingRegistDTO;
import com.dormitory.controller.qry.VisitingRegistQry;
import com.dormitory.controller.vo.VisitingRegistVO;
import com.dormitory.entity.VisitingRegist;

/**
 * <p>
 * 来访登记表 服务类
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
public interface VisitingRegistService extends IService<VisitingRegist> {

    /**
     * 来访登记分页查询
     *
     * @param qry 查询Qry
     * @return IPage<VisitingRegistVO>
     */
    IPage<VisitingRegistVO> pageByQry(VisitingRegistQry qry);

    /**
     * 来访登记详情
     *
     * @param registId 登记ID
     * @return VisitingRegistVO
     */
    VisitingRegistVO detailById(Long registId);

    /**
     * 新增来访登记信息
     *
     * @param dto 登记DTO
     * @return Boolean
     */
    Boolean add(VisitingRegistDTO dto);

    /**
     * 编辑来访登记信息
     *
     * @param registId 登记ID
     * @param dto      登记DTO
     * @return Boolean
     */
    Boolean edit(Long registId, VisitingRegistDTO dto);

    /**
     * 删除来访登记
     *
     * @param registId 登记ID
     * @return Boolean
     */
    Boolean del(Long registId);
}
