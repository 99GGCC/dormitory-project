package com.dormitory.service;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;
import com.dormitory.controller.dto.BuildingInfoDTO;
import com.dormitory.controller.qry.BuildingInfoQry;
import com.dormitory.controller.vo.BuildingInfoVO;
import com.dormitory.entity.BuildingInfo;

/**
 * <p>
 * 楼栋信息表 服务类
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
public interface BuildingInfoService extends IService<BuildingInfo> {

    /**
     * 楼栋信息分页查询
     *
     * @param qry 查询Qry
     * @return IPage<BuildingInfoVO>
     */
    IPage<BuildingInfoVO> pageByQry(BuildingInfoQry qry);

    /**
     * 楼栋信息详情
     *
     * @param buildingId 楼栋ID
     * @return BuildingInfoVO
     */
    BuildingInfoVO detailById(Long buildingId);

    /**
     * 新增楼栋信息
     *
     * @param dto 楼栋DTO
     * @return Boolean
     */
    Boolean add(BuildingInfoDTO dto);

    /**
     * 编辑楼栋信息
     *
     * @param buildingId 楼栋ID
     * @param dto        楼栋DTO
     * @return Boolean
     */
    Boolean edit(Long buildingId, BuildingInfoDTO dto);

    /**
     * 删除楼栋信息
     *
     * @param buildingId 楼栋ID
     * @return Boolean
     */
    Boolean del(Long buildingId);
}
