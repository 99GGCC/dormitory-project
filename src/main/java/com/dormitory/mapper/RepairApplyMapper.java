package com.dormitory.mapper;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.dormitory.controller.qry.RepairApplyQry;
import com.dormitory.controller.vo.RepairApplyVO;
import com.dormitory.entity.RepairApply;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import org.apache.ibatis.annotations.Param;

/**
 * <p>
 * 维修申请表 Mapper 接口
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
public interface RepairApplyMapper extends BaseMapper<RepairApply> {

    /**
     * 维修申请分页查询
     *
     * @param qry 查询Qry
     * @return IPage<RepairApplyVO>
     */
    IPage<RepairApplyVO> pageByQry(@Param("qry") RepairApplyQry qry, @Param("pages") Page<RepairApplyVO> pages);

    /**
     * 维修申请详情
     *
     * @param repairId 申请ID
     * @return RepairApplyVO
     */
    RepairApplyVO detailById(@Param("repairId") Long repairId);
}
