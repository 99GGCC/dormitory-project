package com.dormitory.mapper;

import com.dormitory.controller.vo.BuildingInfoVO;
import com.dormitory.entity.BuildingInfo;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import org.apache.ibatis.annotations.Param;

/**
 * <p>
 * 楼栋信息表 Mapper 接口
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
public interface BuildingInfoMapper extends BaseMapper<BuildingInfo> {

    /**
     * 楼栋信息详情
     *
     * @param buildingId 楼栋ID
     * @return BuildingInfoVO
     */
    BuildingInfoVO detailById(@Param("buildingId") Long buildingId);
}
