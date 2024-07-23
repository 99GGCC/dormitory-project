package com.dormitory.service;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;
import com.dormitory.controller.dto.SetBedDTO;
import com.dormitory.controller.qry.DormitoryInfoQry;
import com.dormitory.controller.vo.DormitoryInfoVO;
import com.dormitory.entity.DormitoryInfo;

import java.util.List;
import java.util.Map;

/**
 * <p>
 * 宿舍信息表 服务类
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
public interface DormitoryInfoService extends IService<DormitoryInfo> {

    /**
     * 宿舍信息分页查询
     *
     * @param qry 查询Qry
     * @return IPage<DormitoryInfoVO>
     */
    IPage<DormitoryInfoVO> pageByQry(DormitoryInfoQry qry);

    /**
     * 宿舍信息楼层列表查询
     *
     * @param buildingId 楼栋ID
     * @return Map<Integer, List < DormitoryInfoVO>>
     */
    Map<Integer, List<DormitoryInfoVO>> listByBuildingId(String buildingId);

    /**
     * 宿舍信息详情
     *
     * @param dormitoryId 宿舍ID
     * @return DormitoryInfoVO
     */
    DormitoryInfoVO detail(Long dormitoryId);

    /**
     * 设置宿舍状态
     *
     * @param dormitoryId 宿舍ID
     * @param status      状态
     * @return Boolean
     */
    Boolean status(Long dormitoryId, Integer status);

    /**
     * 批量设置床位（适用于不存在床位的宿舍）
     *
     * @param setBedDTO 设置床位DTO
     * @return Boolean
     */
    Boolean setBed(SetBedDTO setBedDTO);
}
