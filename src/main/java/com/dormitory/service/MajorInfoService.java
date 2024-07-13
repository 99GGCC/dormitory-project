package com.dormitory.service;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;
import com.dormitory.controller.dto.MajorInfoDTO;
import com.dormitory.controller.qry.MajorInfoQry;
import com.dormitory.controller.vo.MajorInfoVO;
import com.dormitory.entity.MajorInfo;

import java.util.List;

/**
 * <p>
 * 专业信息表 服务类
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
public interface MajorInfoService extends IService<MajorInfo> {

    /**
     * 专业信息分页查询
     *
     * @param qry 查询Qry
     * @return IPage<MajorInfoVO>
     */
    IPage<MajorInfoVO> pageByQry(MajorInfoQry qry);

    /**
     * 专业信息列表查询
     *
     * @param qry 查询Qry
     * @return List<MajorInfoVO>
     */
    List<MajorInfoVO> listByParam(MajorInfoQry qry);

    /**
     * 专业信息详情
     *
     * @param majorId 专业ID
     * @return MajorInfoVO
     */
    MajorInfoVO detailById(Long majorId);

    /**
     * 新增专业信息
     *
     * @param dto 专业DTO
     * @return Boolean
     */
    Boolean add(MajorInfoDTO dto);

    /**
     * 编辑专业信息
     *
     * @param majorId 专业ID
     * @param dto     专业DTO
     * @return Boolean
     */
    Boolean edit(Long majorId, MajorInfoDTO dto);

    /**
     * 删除专业信息
     *
     * @param majorId 专业ID
     * @return Boolean
     */
    Boolean del(Long majorId);
}
