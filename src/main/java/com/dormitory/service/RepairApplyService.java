package com.dormitory.service;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;
import com.dormitory.controller.dto.RepairApplyDTO;
import com.dormitory.controller.qry.RepairApplyQry;
import com.dormitory.controller.vo.RepairApplyVO;
import com.dormitory.entity.RepairApply;

/**
 * <p>
 * 维修申请表 服务类
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
public interface RepairApplyService extends IService<RepairApply> {

    /**
     * 维修申请分页查询
     *
     * @param qry 查询Qry
     * @return IPage<RepairApplyVO>
     */
    IPage<RepairApplyVO> pageByQry(RepairApplyQry qry);

    /**
     * 维修申请详情
     *
     * @param repairId 申请ID
     * @return RepairApplyVO
     */
    RepairApplyVO detailById(Long repairId);

    /**
     * 处理维修申请
     *
     * @param repairId 申请ID
     * @param status   维修状态
     * @return Boolean
     */
    Boolean status(Long repairId, Integer status);

    /**
     * 学生维修申请分页查询
     *
     * @param qry 查询Qry
     * @return IPage<RepairApplyVO>
     */
    IPage<RepairApplyVO> studentPageByQry(RepairApplyQry qry);

    /**
     * 发起维修申请
     *
     * @param dto 申请DTO
     * @return Boolean
     */
    Boolean initiate(RepairApplyDTO dto);

    /**
     * 取消维修申请
     *
     * @param repairId 申请ID
     * @return Boolean
     */
    Boolean cancel(Long repairId);
}
