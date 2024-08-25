package com.dormitory.service;

import com.baomidou.mybatisplus.extension.service.IService;
import com.dormitory.controller.dto.ArrangeBedDTO;
import com.dormitory.controller.dto.BedInfoDTO;
import com.dormitory.controller.dto.ReleaseBedDTO;
import com.dormitory.controller.qry.BedInfoQry;
import com.dormitory.controller.vo.BedInfoVO;
import com.dormitory.entity.BedInfo;

import java.util.List;

/**
 * <p>
 * 床位信息表 服务类
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
public interface BedInfoService extends IService<BedInfo> {

    /**
     * 床位信息列表查询
     *
     * @param qry 查询Qry
     * @return List<BedInfoVO>·
     */
    List<BedInfoVO> listByQry(BedInfoQry qry);

    /**
     * 新增床位信息
     *
     * @param dto 床位DTO
     * @return Boolean
     */
    Boolean add(BedInfoDTO dto);

    /**
     * 编辑床位信息
     *
     * @param bedId 床位ID
     * @param dto   床位DTO
     * @return Boolean
     */
    Boolean edit(Long bedId, BedInfoDTO dto);

    /**
     * 删除床位信息
     *
     * @param bedId 床位ID
     * @return Boolean
     */
    Boolean del(Long bedId);

    /**
     * 安排床位
     *
     * @param dto 安排DTO
     * @return Boolean
     */
    Boolean arrange(ArrangeBedDTO dto);

    /**
     * 释放床位
     *
     * @param dto 释放DTO
     * @return Boolean
     */
    Boolean release(ReleaseBedDTO dto);
}
